install.packages("gmodels")
library(gmodels)


### DATA CLEANING ###
#Import datasets, don't forget to set working directory to source file location
train <- read.csv("../data/bronze/train.csv")
test_X <- read.csv("../data/bronze/test.csv")
head(train)

train_X <- train

ls.str(train_X)

CrossTable(train$default)
CrossTable(train$num_bankrupts)


#########################################
# 1. SPOT NA VALUES AND IMPUTE NA FLAGS #
#########################################

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

#FOR NOW WE KEEP THE NA FLAGS FOR ALL VARIABLES, HOWEVER THE MISSING VALUES FOR HOUSE
#ARE 25% SO MAYBE ONLY THIS NA FLAG MIGHT TELL US SOMETHING AND WE CAN DROP THE REST? 
#WE'LL SEE

##################################################
#2. Inputing mean and mode values in missing data. 
##################################################

#All data is lower than 25% missing so we can still accept this data and inpute values
# NOW WE ARE GOING TO IMPUTE NUMERIC VARIABLES WITH MEAN AND CATEGORICAL VALUES WITH MODE

#define the functions for imputing median and mode            
impute <- function(x, method = median, val = NULL) {
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


#determine the numeric columns and impute mean value in the train and test set
num.cols <- sapply(train_X_impute, is.numeric) 
train_X_impute[, num.cols] <- lapply(train_X_impute[, num.cols],
                                     FUN = impute,
                                     method = median)

test_X_impute$annual_income <- impute(test_X_impute$annual_income, val = median(train_X$annual_income, na.rm = T))
test_X_impute$num_bankrupts <- impute(test_X_impute$num_bankrupts, val = median(train_X$num_bankrupts, na.rm = T))
test_X_impute$num_records   <- impute(test_X_impute$num_records, val = median(train_X$num_records, na.rm = T))
test_X_impute$num_mortgages <- impute(test_X_impute$num_mortgages, val = median(train_X$num_mortgages, na.rm = T))
test_X_impute$num_total_credit <- impute(test_X_impute$num_total_credit, val = median(train_X$num_total_credit, na.rm = T))
test_X_impute$revol_util    <- impute(test_X_impute$revol_util, val = median(train_X$revol_util, na.rm = T))
test_X_impute$monthly_payment <- impute(test_X_impute$monthly_payment, val = median(train_X$monthly_payment, na.rm = T))

#determine the categoric columns and impute the mode in the train and test set
cat.cols <- !num.cols
train_X_impute[, cat.cols] <- lapply(train_X_impute[, cat.cols],
                                     FUN = impute,
                                     method = modus)
test_X_impute[, cat.cols] <- mapply(test_X_impute[, cat.cols],
                                    FUN = impute,
                                    val = sapply(train_X[, cat.cols], modus, na.rm = T))

#We double check our results and spot no more missing values for both the test and train set
colMeans(is.na(train_X_impute))  # -->no more missing values       
colMeans(is.na(test_X_impute))   # -->no more missing values


#For some non parametric models it is possible to work WITH outliers so we save one basetable


write.csv(train_X_impute, file= "../data/silver/train_with_outlier.csv")
write.csv(test_X_impute, file = "../data/silver/test_cleaned_data.csv")

###############################
# 3.1 REMOVING OUTLIERS      ###   
###############################

train_X_outlier = train_X_impute


#below we can find a easy for loop that shows us which numeric variables contain outliers
num.cols <- sapply(train_X_impute, is.numeric) 
all_cols <- colnames(train_X)[num.cols]
for (i in all_cols[2:13]){
  print(paste0("column name:",i,"  ",sum(abs(scale(train_X_outlier[i]))>3)))
}

#we create all the conditions for which rows we want to remove because they contain an outlier
b_ai <- abs(scale(train_X_outlier$annual_income)) < 3
b_dti <- abs(scale(train_X_outlier$debt_to_income)) < 3
b_ir <- abs(scale(train_X_outlier$interest_rate)) < 3
b_mp <- abs(scale(train_X_outlier$monthly_payment)) < 3
b_nb <- abs(scale(train_X_outlier$num_bankrupts)) < 3
b_nm <- abs(scale(train_X_outlier$num_mortgages)) < 3
b_oc <- abs(scale(train_X_outlier$num_open_credit)) < 3
b_nr <- abs(scale(train_X_outlier$num_records)) < 3
b_ntc <- abs(scale(train_X_outlier$num_total_credit)) < 3
b_rb <- abs(scale(train_X_outlier$revol_balance)) < 3
b_ru <- abs(scale(train_X_outlier$revol_util)) < 3

#we subset from our original dataset for which the outlier conditions are ALWAYS true,
#in other words, if one of the conditions is false we drop the whole row
new_train_x <- subset(train_X_outlier, (b_ai&b_dti&b_ir&b_mp&b_nb&b_nm&b_oc&b_nr&b_ntc&b_rb&b_ru))

write.csv(new_train_x, file= "../data/silver/train_cleaned_data.csv")

#we omitted 70954 -> 64943 = 6011 rows from our dataset


################################
# 3.2 Treat OUTLIERS as NA values 
################################

train_X_outlier = train_X_impute

#define outlier function that replaces the outlier with a mean value?
handle_outlier_z <- function(col){
  col_z <- scale(col)
  ifelse(abs(col_z)>3,
         sign(col_z)*3*attr(col_z,"scaled:scale") + attr(col_z,"scaled:center"), col)
}

#We now change all outlier values to NA values
num.cols <- sapply(train_X, is.numeric)
train_X_outlier[,all_cols[2:13]] <-  sapply(train_X[,all_cols[2:13]], FUN = handle_outlier_z)

#we see several of the columns now containg NA values
for (i in all_cols[2:13]){
  print(paste0("column name:",i,"  ",sum(abs(scale(train_X_outlier[i]))>3)))
}

#We impute those NA values again like step 1
impute <- function(x, method = median, val = NULL) {
  if (is.null(val)) {
    val <- method(x, na.rm = TRUE)
  }
  x[is.na(x)] <- val
  return(x)
}

num.cols <- sapply(train_X_impute, is.numeric) 
train_X_outlier[, num.cols] <- lapply(train_X_outlier[, num.cols],
                                     FUN = impute,
                                     method = median)

write.csv(train_X_outlier, file= "../data/silver/train_cleaned_NA_data.csv")

#we no longer have any NA values and all original outliers were replaced by the mean value

############################################
#3.3 Specific outliers with 'expert' opinion
############################################

train_X_outlier = train_X_impute

b_ai2 <- abs(scale(train_X_outlier$annual_income)) < 3 #INCREDIBLY skewed results, remove them all
b_dti2 <- (train_X_outlier$debt_to_income < 1500) #Only one value that is bad (1600+), 
#remove this value, also self reported so could be wrong data, remove!
b_ir2 <- NULL #outliers are not really that bad 
#and the values make sens, so we don't remove any of them (see boxplots)
b_mp2 <- NULL # gradual increase of our outlier, 922 outliers but there are 
#no crazy values so we keep them
b_nb2 <- NULL #Very important! Don't drop these! 
#this shows us that the people that take out a loan are people with 0 or 7 bankruptcies! 
b_nm2 <- abs(scale(train_X_outlier$num_mortgages)) < 3 #the values are up to three times more
#than the limit of our boxplot, we do remove these values
b_oc2 <- abs(scale(train_X_outlier$num_open_credit)) < 3 #almost all of the observations are 
#contained up until thirty so we remove the outliers
b_nr2 <- abs(scale(train_X_outlier$num_records)) < 3 #99% is contained in the first three
#categories of number of records so we remove the outliers
b_ntc2 <- abs(scale(train_X_outlier$num_total_credit)) < 3 #majority of the observations are 
#normally distributed so we remove the outliers
b_rb2 <- NULL #outliers are very close to the boxplots so
#keep them, also no steep increase but a gradual increase in outliers
b_ru2 <- abs(scale(train_X_outlier$revol_util)) < 3 #only three outliers, we can remove them

new_train_x2 <- subset(train_X_outlier, (b_ai2&b_dti2&b_nm2&b_oc2&b_nr2&b_ntc2&b_ru2))

#We have 66407 rows se we removed 4547 rows

write.csv(new_train_x2, file= "../data/silver/train_cleaned_less_restrictive_data.csv")

