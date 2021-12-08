install.packages("gmodels")
library(gmodels)
library(corrplot)
library(RColorBrewer)


### DATA CLEANING ###
train <- read.csv("train.csv")
test_X <- read.csv("test.csv")
head(train)

train_X <- subset(train, select = -c(default))
train_y <- train$default
ls.str(train_X)

CrossTable(train$default)
CrossTable(train$num_bankrupts)
CrossTable(train_X_fe$state)
train_X_fe[train_X_fe$state == 'W']

############################
# 1. IMPUTE MISSING VALUES #
############################

train_X_impute <- train_X
test_X_impute <- test_X
colMeans(is.na(train_X_impute))

# -------------------------- #
# Columns with missing values :
# - numerical :
# 1) annual_income
# 2) num_bankrupts
# 3) num_records
# 4) num_mortgages
# 5) num_total_credit
# 6) revol_util
# 7) monthly payment
# - categorical :
# 1) home_status
# 2) emp_lenght
# --------------------------- #

colMeans(is.na(test_X_impute))

#------#
# SAME 
#------#



#Some variables are self reported by the borrower. These missing 
#values could be interesting for our analysis. 
#That's why we will create indicator columns for annual_income,
#emp_title, and HomeStatus


naFlag <- function(df, df_val = NULL) {
  if (is.null(df_val)) {
    df_val <- df
  }
  mask <- sapply(df_val, anyNA)
  out <- lapply(df[mask], function(x)as.numeric(is.na(x)))
  if (length(out) > 0) names(out) <- paste0(names(out), "_flag")
  return(as.data.frame(out))
}

str(naFlag(df=train_X))
train_X_impute <- cbind(train_X_impute,
                        naFlag(df = train_X))
test_X_impute <- cbind(test_X_impute,
                       naFlag(df = test_X, df_val = train_X))

# ONLY KEEP THE FLAG FOR HOME_STATUS

cols.dont.want <- c('annual_income_flag', 
                    'emp_length_flag',
                    'emp_title_flag',
                    'monthly_payment_flag',
                    'num_bankrupts_flag',
                    'num_mortgages_flag',
                    'num_records_flag'  ,
                    'num_total_credit_flag',
                    'revol_util_flag')


train_X_impute = train_X_impute[ , ! names(train_X_impute) %in% cols.dont.want]
test_X_impute = test_X_impute[ , ! names(test_X_impute) %in% cols.dont.want]

# NOW WE ARE GOING TO IMPUTE NUMERIC VARIABLES WITH MEAN AND CATEGORICAL VALUES WITH MODE
            


impute <- function(x, method = mean, val = NULL) {
  if (is.null(val)) {
    val <- method(x, na.rm = TRUE)
  }
  x[is.na(x)] <- val
  return(x)
}

modus <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}




num.cols <- sapply(train_X_impute, is.numeric) 
train_X_impute[, num.cols] <- lapply(train_X_impute[, num.cols],
                                     FUN = impute,
                                     method = mean)
#--------------------------------------------------------------STRANGE
test_X_impute[, num.cols] <- mapply(test_X_impute[, num.cols],
                                    FUN = impute,
                                    val = mean(train_X$num.cols ,na.rm = T))
# This code doesnt work, so I do it in the cumbersome way :

test_X_impute$annual_income <- impute(test_X_impute$annual_income, val = mean(train_X$annual_income, na.rm = T))
test_X_impute$num_bankrupts <- impute(test_X_impute$num_bankrupts, val = mean(train_X$num_bankrupts, na.rm = T))
test_X_impute$num_records   <- impute(test_X_impute$num_records, val = mean(train_X$num_records, na.rm = T))
test_X_impute$num_mortgages <- impute(test_X_impute$num_mortgages, val = mean(train_X$num_mortgages, na.rm = T))
test_X_impute$num_total_credit <- impute(test_X_impute$num_total_credit, val = mean(train_X$num_total_credit, na.rm = T))
test_X_impute$revol_util    <- impute(test_X_impute$revol_util, val = mean(train_X$revol_util, na.rm = T))
test_X_impute$monthly_payment <- impute(test_X_impute$monthly_payment, val = mean(train_X$monthly_payment, na.rm = T))

#----------------------------------------------------------------------STRANGE




cat.cols <- !num.cols
train_X_impute[, cat.cols] <- lapply(train_X_impute[, cat.cols],
                                     FUN = impute,
                                     method = modus)
test_X_impute[, cat.cols] <- mapply(test_X_impute[, cat.cols],
                                    FUN = impute,
                                    val = sapply(train_X[, cat.cols], modus, na.rm = T))


colMeans(is.na(train_X_impute))  # -->no more missing values       
colMeans(is.na(test_X_impute))   # -->no more missing values




?sapply



#################################
# 2.BINNING NUMERIC VARIABLES ###   
#################################


# Binning often improves performance

head(train_X_impute)
M <-cor(train_X_impute[sapply(train_X_impute,is.numeric)])
corrplot(M, type="upper", order="hclust",col=brewer.pal(n=8, name="RdYlBu"))



#################################
# 3. LEAVE OUT VARIABLES      ###   
#################################

# For now we'll leave out the date columns. 

cols.to.drop <- c('earliest_cr_line', 'date_funded', 'address')
train_X_impute = train_X_impute[ , ! names(train_X_impute) %in% cols.to.drop]
test_X_impute = test_X_impute[ , ! names(test_X_impute) %in% cols.to.drop]



