##### VALIDATION SET APPROACH #####
library(randomForest)
library(gbm)

train <- read.csv("train.csv")
samp = sample(nrow(train), 0.7*nrow(train))
TRAIN_X <- train[samp, -26]
TRAIN_Y <- train[samp,26]
VAL_X <- train[-samp, -26]
VAL_Y <- train[-samp,26]

TRAIN_X_impute <- TRAIN_X
VAL_X_impute <- VAL_X

str(naFlag(df=TRAIN_X))
TRAIN_X_impute <- cbind(TRAIN_X_impute,
                        naFlag(df = TRAIN_X))
VAL_X_impute <- cbind(VAL_X_impute,
                       naFlag(df = VAL_X, df_val = TRAIN_X))






TRAIN_X_impute$num_bankrupts <- factor(TRAIN_X_impute$num_bankrupts)
TRAIN_X_impute$num_mortgages <- factor(TRAIN_X_impute$num_mortgages)
TRAIN_X_impute$num_total_credit <- factor(TRAIN_X_impute$num_total_credit)
TRAIN_X_impute$num_open_credit <- factor(TRAIN_X_impute$num_open_credit)
TRAIN_X_impute$num_records <- factor(TRAIN_X_impute$num_records)
VAL_X_impute$num_bankrupts <- factor(VAL_X_impute$num_bankrupts)
VAL_X_impute$num_mortgages <- factor(VAL_X_impute$num_mortgages)
VAL_X_impute$num_total_credit <- factor(VAL_X_impute$num_total_credit)
VAL_X_impute$num_open_credit <- factor(VAL_X_impute$num_open_credit)
VAL_X_impute$num_records <- factor(VAL_X_impute$num_records)



match <- regmatches(TRAIN_X_impute$date_funded, regexec("[0-9]{4}", TRAIN_X_impute$date_funded))
TRAIN_X_impute$date_funded <- sapply(match, `[`, 1)
match2 <- regmatches(VAL_X_impute$date_funded, regexec("[0-9]{4}", VAL_X_impute$date_funded))
VAL_X_impute$date_funded <- sapply(match2, `[`, 1)
match3 <- regmatches(TRAIN_X_impute$earliest_cr_line, regexec("[0-9]{4}", TRAIN_X_impute$earliest_cr_line))
TRAIN_X_impute$earliest_cr_line <- sapply(match3, `[`, 1)
match4 <- regmatches(VAL_X_impute$earliest_cr_line, regexec("[0-9]{4}", VAL_X_impute$earliest_cr_line))
VAL_X_impute$earliest_cr_line <- sapply(match4, `[`, 1)
TRAIN_X_impute$date_funded <- as.numeric(TRAIN_X_impute$date_funded)
TRAIN_X_impute$earliest_cr_line <- as.numeric(TRAIN_X_impute$earliest_cr_line)
VAL_X_impute$date_funded <- as.numeric(VAL_X_impute$date_funded)
VAL_X_impute$earliest_cr_line <- as.numeric(VAL_X_impute$earliest_cr_line)
TRAIN_X_impute[['difference']] <- TRAIN_X_impute$date_funded - TRAIN_X_impute$earliest_cr_line
VAL_X_impute[['difference']] <- VAL_X_impute$date_funded - VAL_X_impute$earliest_cr_line
#TRAIN_X_impute$date_funded <- format(as.Date(TRAIN_X_impute$date_funded, format = "%b-%Y"), "%Y")
match5 <- regmatches(TRAIN_X_impute$address, regexec("[A-Z][A-Z]", TRAIN_X_impute$address))
match6 <- regmatches(VAL_X_impute$address, regexec("[A-Z][A-Z]", VAL_X_impute$address))
TRAIN_X_impute$state <- sapply(match5, `[`, 1)
VAL_X_impute$state <- sapply(match6, `[`, 1)






cols.dont.want <- c( 
  'emp_length_flag',
  'monthly_payment_flag',
  'num_bankrupts_flag',
  'num_mortgages_flag',
  'num_records_flag'  ,
  'num_total_credit_flag',
  'revol_util_flag', 'address', 'id', 'state', 'emp_title', 'date_funded', 'earliest_cr_line',
  'annual_income_flag', 'application_type')

cols.dont.want <- c( 
  'emp_length_flag',
  'monthly_payment_flag',
  'num_bankrupts_flag',
  'num_mortgages_flag',
  'num_records_flag'  ,
  'num_total_credit_flag',
  'revol_util_flag', 'id', 'address', 'emp_title', 'date_funded', 'earliest_cr_line',
  'annual_income_flag', 'application_type')






TRAIN_X_impute = TRAIN_X_impute[ , ! names(TRAIN_X_impute) %in% cols.dont.want]
VAL_X_impute = VAL_X_impute[ , ! names(VAL_X_impute) %in% cols.dont.want]

num.cols <- sapply(TRAIN_X_impute, is.numeric) 
TRAIN_X_impute[, num.cols] <- lapply(TRAIN_X_impute[, num.cols],
                                     FUN = impute,
                                     method = mean)
#--------------------------------------------------------------STRANGE
VAL_X_impute[, num.cols] <- mapply(test_X_impute[, num.cols],
                                    FUN = impute,
                                    val = mean(train_X$num.cols ,na.rm = T))
# This code doesnt work, so I do it in the cumbersome way :

VAL_X_impute$annual_income <- impute(VAL_X_impute$annual_income, val = mean(TRAIN_X$annual_income, na.rm = T))
VAL_X_impute$revol_util    <- impute(VAL_X_impute$revol_util, val = mean(TRAIN_X$revol_util, na.rm = T))
VAL_X_impute$monthly_payment <- impute(VAL_X_impute$monthly_payment, val = mean(TRAIN_X$monthly_payment, na.rm = T))

#----------------------------------------------------------------------STRANGE


cat.cols

cat.cols <- !num.cols
cat.cols
TRAIN_X_impute[, cat.cols] <- lapply(TRAIN_X_impute[, cat.cols],
                                     FUN = impute,
                                     method = modus)
#---------------------------------------------------------------------------#
VAL_X_impute[, cat.cols] <- mapply(VAL_X_impute[, cat.cols],
                                    FUN = impute,
                                    val = sapply(TRAIN_X[, cat.cols], modus, na.rm = T))
#----------------------------------------------------------------------------------------#



VAL_X_impute$num_bankrupts <- impute(VAL_X_impute$num_bankrupts, val = modus(TRAIN_X$num_bankrupts, na.rm = T))
VAL_X_impute$emp_length <- impute(VAL_X_impute$emp_length, val = modus(TRAIN_X$emp_length, na.rm = T))
VAL_X_impute$num_mortgages <- impute(VAL_X_impute$num_mortgages, val = modus(TRAIN_X$num_mortgages, na.rm = T))
VAL_X_impute$home_status <- impute(VAL_X_impute$home_status, val = modus(TRAIN_X$home_status, na.rm = T))
VAL_X_impute$num_records <- impute(VAL_X_impute$num_records, val = modus(TRAIN_X$num_records, na.rm = T))
VAL_X_impute$num_total_credit <- impute(VAL_X_impute$num_total_credit, val = modus(TRAIN_X$num_total_credit, na.rm = T))


colMeans(is.na(TRAIN_X_impute))
colMeans(is.na(VAL_X_impute))





vec <- c('application_type','emp_length',  'grade', 
         'home_status', 'income_verif_status', 
         'home_status_flag', 'sub_grade', 'term', 'annual_income_flag', 'emp_title_flag')

TRAIN_X_fe <- TRAIN_X_impute
VAL_X_fe <- VAL_X_impute
ls.str(TRAIN_X_fe)
ls.str(VAL_X_fe)

#TRAIN_X_fe <- as.data.frame(unclass(train_X_fe), stringsAsFactors = T)
#VAL_X_fe <- as.data.frame(unclass(VAL_X_fe), stringsAsFactors = T)

TRAIN_X_fe$home_status_flag <- as.factor(TRAIN_X_fe$home_status_flag)
TRAIN_X_fe$emp_title_flag <- as.factor(TRAIN_X_fe$emp_title_flag)
VAL_X_fe$home_status_flag <- as.factor(VAL_X_fe$home_status_flag)
VAL_X_fe$emp_title_flag <- as.factor(VAL_X_fe$emp_title_flag)


num.cols <- sapply(TRAIN_X_fe, is.numeric)
mean_train <- colMeans(TRAIN_X_fe[, num.cols])
sd_train <- sapply(TRAIN_X_fe[, num.cols], sd)
TRAIN_X_fe[, num.cols] <- scale(TRAIN_X_fe[, num.cols], center = TRUE, scale = TRUE)

num.cols <- sapply(VAL_X_fe, is.numeric)
mean_train <- colMeans(VAL_X_fe[, num.cols])
sd_train <- sapply(VAL_X_fe[, num.cols], sd)
VAL_X_fe[, num.cols] <- scale(VAL_X_fe[, num.cols], center = TRUE, scale = TRUE)



emp_length_levels <- c("< 1 year", "1 year", "2 years", "3 years",
                       "4 years", "5 years", "6 years", "7 years",
                       "8 years", "9 years", "10+ years")
TRAIN_X_fe$emp_length <- as.numeric(factor(TRAIN_X_fe$emp_length, levels = emp_length_levels))
VAL_X_fe$emp_length <- as.numeric(factor(VAL_X_fe$emp_length, levels = emp_length_levels))
#new_TRAIN_X$num_bankrupts <- as.numeric(factor(new_TRAIN_X$num_bankrupts, levels = num_bankrupts_levels))
#new_VAL_X$num_bankrupts <- as.numeric(factor(new_VAL_X$num_bankrupts, levels = num_bankrupts_levels))

emp_length <- c("< 1 year", "1 year", "2 years", "3 years",
                "4 years", "5 years", "6 years", "7 years",
                "8 years", "9 years", "10+ years")



TRAIN_X_fe$emp_length <- ifelse(TRAIN_X_fe$emp_length %in% c("< 1 year", "1 year", "2 years"),"low_length",
                                ifelse(TRAIN_X_fe$emp_length %in% c("3 years", "4 years"), "medium_length", "high_length"))


new_VAL_X$emp_length <- ifelse(new_VAL_X$emp_length %in% c("< 1 year", "1 year", "2 years"),"low_length",
                                ifelse(new_VAL_X$emp_length %in% c("3 years", "4 years"), "medium_length", "high_length"))
str(TRAIN_X_fe$emp_length)
str(TRAIN_X_fe)
TRAIN_X_fe$emp_length <- as.factor(TRAIN_X_fe$emp_length)
########## TAREGET ENCODING ########################
####################################################

CrossTable(TRAIN_X_fe$num_mortgages) # i would say +8
CrossTable(TRAIN_X_fe$num_open_credit)
CrossTable(TRAIN_X_fe$num_records) #+4 
CrossTable(TRAIN_X_fe$num_total_credit) # + 50


encode_target <- function(x, y, sigma = NULL) {
  d <- aggregate(y, list(factor(x, exclude = NULL)), mean, na.rm = TRUE)
  m <- d[is.na(as.character(d[, 1])), 2]
  l <- d[, 2]
  names(l) <- d[, 1]
  l <- l[x]
  l[is.na(l)] <- m
  if (!is.null(sigma)) {
    l <- l * rnorm(length(l), mean = 1, sd = sigma)
  }
  l
}

class(TRAIN_Y)
TRAIN_Y <- as.factor(TRAIN_Y)
new_TRAIN_X <- TRAIN_X_fe
new_VAL_X <- VAL_X_fe
class(new_TRAIN_X$difference)
new_TRAIN_X[['num_mort_encoded']] <- encode_target(TRAIN_X_fe[["num_mortgages"]], TRAIN_Y)
new_TRAIN_X[['num_totalcredit_encoded']] <- encode_target(TRAIN_X_fe[["num_total_credit"]], TRAIN_Y)
new_TRAIN_X[['num_opencredit_encoded']] <- encode_target(TRAIN_X_fe[["num_open_credit"]], TRAIN_Y)
new_VAL_X[['num_mort_encoded']] <- encode_target(VAL_X_fe[["num_mortgages"]], VAL_Y)
new_VAL_X[['num_totalcredit_encoded']] <- encode_target(VAL_X_fe[["num_total_credit"]], VAL_Y)
new_VAL_X[['num_opencredit_encoded']] <- encode_target(VAL_X_fe[["num_open_credit"]], VAL_Y)
new_TRAIN_X[['state_encoded']] <- encode_target(TRAIN_X_fe[["state"]], TRAIN_Y)
new_VAL_X[['state_encoded']] <- encode_target(VAL_X_fe[["state"]], VAL_Y)


str(TRAIN_X_fe)
str(VAL_X_fe)
str(new_VAL_X)
head(new_TRAIN_X, 20)
head(new_VAL_X,10)


cols.drop.after.t.encoding <- c('num_mortgages', 'num_total_credit', 'num_open_credit', 'state')
cd <- c('num_total_credit') 
new_TRAIN_X = new_TRAIN_X[ , ! names(new_TRAIN_X) %in% cols.drop.after.t.encoding]
new_VAL_X = new_VAL_X[ , ! names(new_VAL_X) %in% cols.drop.after.t.encoding]

new_TRAIN_X$num_opencredit_encoded <- scale(new_TRAIN_X$num_opencredit_encoded,center = TRUE, scale = TRUE)
new_TRAIN_X$num_mort_encoded <- scale(new_TRAIN_X$num_mort_encoded,center = TRUE, scale = TRUE)
new_TRAIN_X$num_totalcredit_encoded <- scale(new_TRAIN_X$num_totalcredit_encoded,center = TRUE, scale = TRUE)
new_TRAIN_X$state_encoded <- scale(new_TRAIN_X$state_encoded, center = T, scale = T)
new_VAL_X$num_totalcredit_encoded <- scale(new_VAL_X$num_totalcredit_encoded, center = TRUE, scale = TRUE)
new_VAL_X$num_opencredit_encoded <- scale(new_VAL_X$num_opencredit_encoded, center = TRUE, scale = TRUE)
new_VAL_X$num_mort_encoded <- scale(new_VAL_X$num_mort_encoded, center = TRUE, scale = TRUE)
new_VAL_X$state_encoded <- scale(new_VAL_X$state_encoded, center = TRUE, scale = TRUE)

########## TAREGET ENCODING ########################
####################################################



class(TRAIN_Y)
TRAIN_Y <- as.factor(TRAIN_Y)
ls.str(new_TRAIN_X)

FIT <- randomForest(x = new_TRAIN_X, y = TRAIN_Y, mtry = 7 ,ntree = 40, importance = T)
Y_estimatee <- predict(FIT,XTEST)
cf.test <- table(Y_estimate,VAL_Y)
cf.test

ls.str(new_TRAIN_X)

build_target_encoding()

FIT
importance(FIT)

print('j')





TRAIN_X_impute$num_bankrupts
new_TRAIN_X$emp_length





TRAIN


class(TRAIN_Y)

head(new_VAL_X)
head(new_TRAIN_X)
typeof(new_TRAIN_X$num_totalcredit_encoded)


###############LOGISTIC REGRESSION#####

FITLR <- glm(x = new_TRAIN_X, y = TRAIN_Y , family = binomial())
FITLR <- glm(TRAIN_Y ~ ., data = new_TRAIN_X, family = binomial())
summary(FITLR)
Y_estimateLR <- predict(FITLR, new_VAL_X)

cf.test <- table(Y_estimateLR, total_train$TRAIN_Y)
acc.test <- sum(diag(cf.test))/sum(cf.test)
acc.test
ls.str(VAL_X_fe)
nrow(VAL_X_fe)

?predict()

#concat train_x_fe with train_y
tr <- as.data.frame(TRAIN_Y)
total_train <- cbind(TRAIN_X_fe,tr)

########>BOOSTING######################
attach(total_train)
fit_boost <- gbm(TRAIN_Y~., data = total_train, distribution = 'huberized',
                 n.trees = 100, interaction.depth = 1,shrinkage = 0.2)

Y_estimateB <- predict(fit_boost,VAL_X)

mod <- train(x=TRAIN_X_fe, y = TRAIN_Y, method="gbm", verbose = T)



cf.test
acc.test <- sum(diag(cf.test))/sum(cf.test)
acc.test

y_estimate <- as.numeric(as.character(y_estimate))
test_X$id <- as.character(test_X$id)


XTEST <- rbind(TRAIN_X_fe[1, ] , VAL_X_fe)
XTEST <- XTEST[-1,]

XTEST <- rbind(new_TRAIN_X[1, ] , new_VAL_X)
XTEST <- XTEST[-1,]


y_estimate <- as.numeric(as.character(y_estimate))
test_X$id <- as.character(test_X$id)
