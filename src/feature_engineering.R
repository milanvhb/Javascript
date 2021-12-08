  #Import possible datasets
install.packages("dummy")
library(dummy)
train <- read.csv("../data/silver/train_cleaned_data.csv")
#train <- read.csv("../data/silver/train_cleaned_less_restrictive_data.csv")
#train <- read.csv("../data/silver/train_cleaned_NA_data.csv")
#train <- read.csv("../data/silver/train_cleaned_with_outlier.csv")
test_X_fe <- read.csv("../data/silver/test_cleaned_data.csv")

train_X_fe <- subset(train, select = -c(default))
train_y <- train$default

############################################################################################
#1. TURNING INDIVIDUAL FEATURES INTO MORE USABLE FEATURES: GENERAL APPROACH FOR ALL DATASETS
############################################################################################


############
#ADDRESS
############

#States? dummy var? 



############
#AMOUNT, ANNUAL_INCOME, DEBT_TO_INCOME, INTEREST_RATE, MONTHLY_PAYMENT, NUM_BANKRUPTS, 
#NUM_MORTGAGES, NUM_OPEN_CREDIT, NUM_RECORDS, NUM_TOTAL_CREDIT, REVOL_BALANCE, REVOL_UTIL
############

#nothing, numerical variable: standardizing: see later


############
#TERM
############

#binary coding: 
#36 months --> 0 
#60 months --> 1




############
#PURPOSE
############

#dummies 




############
#APPLICATION_TYPE 
############

#dummies



############
#EMP_LENGTH
############

#as numerical! 



############
#EMP_TITLE
############




############
#HOME_STATUS
############

#We check all possible values for home status and see that the NONE or ANY column are extra
unique(train_X_fe$home_status) #MORTAGE RENT OWN NONE OTHER ANY
unique(test_X_fe$home_status) #MORTGAGE RENT OWN OTHER

#We will Register the "ANY" observations as "OTHER" observations and retain the "NONE" 
#observations (the "NONE" column functions as NA indicator)
train_X_fe$home_status[train_X_fe$home_status == "ANY"] <- "OTHER"
#The NONE information will be seen as an NA value which we impute with the mode of "
table(train_X_fe$home_status) #MORTAGE is the mode
train_X_fe$home_status[train_X_fe$home_status == "NONE"] <- "MORTGAGE"

#check
head(train_X_fe)
head(test_X_fe)

########################
#GRADE AND SUB_GRADE
########################

# making the categorical variables grade and sub_grade ordinal: rating A is better than C, and A1 is better than A5 

encode_ordinal <- function(x, order = unique(x)){
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}

train_X_fe[["grade_enc"]]<-encode_ordinal(train_X_fe[["grade"]],sort(unique(train_X_fe$grade)))
train_X_fe[["sub_grade_enc"]]<-encode_ordinal(train_X_fe[["sub_grade"]],sort(unique(train_X_fe$sub_grade)))
train_X_fe


#check
head(train_X_fe)
head(test_X_fe)

########################
#DATE_FUNDED and EARLIEST_CR_LINE ---> YEARS_CUSTOMERS
########################


# Making new column: years_customers, which expresses how long the customer has been customer on the moment the loan is started 
# years_customer = date_funded - earliest cr line (date at which loan is funded - date at which customer started first loan)
#afterwards: drop columns date funded and earliest cr line. 

match <- regmatches(train_X_fe$date_funded, regexec("[0-9]{4}", train_X_fe$date_funded))
train_X_fe$date_funded <- sapply(match, `[`, 1)
match2 <- regmatches(test_X_fe$date_funded, regexec("[0-9]{4}", test_X_fe$date_funded))
test_X_fe$date_funded <- sapply(match2, `[`, 1)
match3 <- regmatches(train_X_fe$earliest_cr_line, regexec("[0-9]{4}", train_X_fe$earliest_cr_line))
train_X_fe$earliest_cr_line <- sapply(match3, `[`, 1)
match4 <- regmatches(test_X_fe$earliest_cr_line, regexec("[0-9]{4}", test_X_fe$earliest_cr_line))
test_X_fe$earliest_cr_line <- sapply(match4, `[`, 1)
train_X_fe$date_funded <- as.numeric(train_X_fe$date_funded)
train_X_fe$earliest_cr_line <- as.numeric(train_X_fe$earliest_cr_line)
test_X_fe$date_funded <- as.numeric(test_X_fe$date_funded)
test_X_fe$earliest_cr_line <- as.numeric(test_X_fe$earliest_cr_line)
train_X_fe$years_customer <- train_X_fe$date_funded - train_X_fe$earliest_cr_line 
test_X_fe$years_customer <- test_X_fe$date_funded - test_X_fe$earliest_cr_line 

#drop column date_funded and earliest_cr_line: 
train_X_fe <- subset(train_X_fe, select = -c(date_funded,earliest_cr_line))
test_X_fe <- subset(test_X_fe, select=  -c(date_funded,earliest_cr_line))

#check
head(train_X_fe)
head(test_X_fe)


########################
#INCOME_VERIF_STATUS
########################
cats <- categories(data.frame(train_X_fe$income_verif_status))
cats
# apply on train set (exclude reference categories)
dummies_train <- dummy(data.frame(train_X_fe$income_verif_status),object = cats)
dummies_train
#rename vars: 
names(dummies_train)[1] <- "income_status_not_verified"
names(dummies_train)[2] <- "income_status_source_verified"
names(dummies_train)[3] <- "income_status_verified"

#exclude reference category (first column): 
dummies_train <- subset(dummies_train, select = -c(income_status_not_verified))

head(test_X_fe$income_verif_status)
# apply on test set: 
dummies_test <- dummy(data.frame(test_X_fe$income_verif_status)) #object = cats needs to be added as argument but it throws an error idk y)
dummies_test
names(dummies_test)[1] <- "income_status_not_verified"
names(dummies_test)[2] <- "income_status_source_verified"
names(dummies_test)[3] <- "income_status_verified"

#Exclude reference category 
dummies_test <- subset(dummies_test, select = -c(income_status_not_verified))
#check 
head(dummies_test)

## merge with overall training set
train_X_fe <- subset(train_X_fe, select = -c(income_verif_status))
train_X_fe <- cbind(train_X_fe, dummies_train)
## merge with overall test set
test_X_fe <- subset(test_X_fe, select = -c(income_verif_status))
test_X_fe <- cbind(test_X_fe, dummies_test)

#check
head(train_X_fe)
head(test_X_fe)


############################################################################################
#2. TURNING INDIVIDUAL FEATURES INTO MORE USABLE FEATURES: FOR USING SPECIFIC DATASETS OR MODELS
############################################################################################

#WHEN WE USE THE TRAIN_CLEAND_LESS_RESTRICTIVE_DATA WE DONT REMOVE A LOT OF OUR LARGER OUTLIERS
#SO THATS WHY WE WILL BIN THESE VARIABLES

####
#num_open_credit
###
n_oc_freq <- bin_data_frequency(train_X_fe$num_open_credit, train_X_fe$num_open_credit, bins = 9)
#1 = [0,6(
#2 = [6,7(
#3 = [7,9(
#4 = [9,10(
#5 = [10,11(
#6 = [11,13(
#7 = [13,14(
#8 = [14,18(
#9 = [18,52(

#assign the binned value
train_X_fe$num_open_credit <-as.numeric(n_oc_freq)

###
#num_total_credit
###
n_tc_freq <- bin_data_frequency(train_X_fe$num_total_credit, train_X_fe$num_total_credit, bins = 20)
#1 = [3,9(
#2 = [9,11(
#...
train_X_fe$num_total_credit <- as.numeric(n_tc_freq)

###
#num_mortgages
###

#every value where we have more than 7 mortgages are combined to one big group
train_X_fe$num_mortgages[train_X_fe$num_mortgages >= 7] <- 7

###
#num_records
###

#every value where we have more than 2 records are combined in to one big group
train_X_fe$num_records[train_X_fe$num_records >= 2] <- 2



#------------------------

#ALS JE VERDER GAAT MET DE DATA HIER ONDER KIJK DAN OF JE DE JUISTE DATASETS ENZO GEBRUIKT

#################################
# 1.ENCODING CATEGORICAL DATA ###   
#################################

ls.str(train_X_impute)
ls.str(test_X_impute)

# I Don't know why but factors are turned into characters?
# I'll change it back

vec <- c('application_type','emp_length', 'emp_title', 'grade', 
         'home_status', 'income_verif_status', 'state',
         'home_status_flag', 'sub_grade', 'term')

train_X_fe <- train_X_impute
test_X_fe <- test_X_impute
train_X_fe <- as.data.frame(unclass(train_X_fe), stringsAsFactors = T)
test_X_fe <- as.data.frame(unclass(test_X_fe), stringsAsFactors = T)

## NOT NECESSARY TO ENCODE CAT VAR'S FOR RF ##

#################################
# 2.SCALING NUMERICAL DATA    ###   
#################################

num.cols <- sapply(train_X_fe, is.numeric)
mean_train <- colMeans(train_X_fe[, num.cols])
sd_train <- sapply(train_X_fe[, num.cols], sd)
train_X_fe[, num.cols] <- scale(train_X_fe[, num.cols], center = TRUE, scale = TRUE)

num.cols <- sapply(test_X_fe, is.numeric)
mean_train <- colMeans(test_X_fe[, num.cols])
sd_train <- sapply(test_X_fe[, num.cols], sd)
test_X_fe[, num.cols] <- scale(test_X_fe[, num.cols], center = TRUE, scale = TRUE)


#################################
# 3.CLASS IMBALANCE           ###   
#################################





###################################
# 4.Encode dependent var as factor#           
###################################

train_y <- as.factor(train_y)


############################
# 2. STRING MANIPULATION   #
############################

#---------------EXAMPLE-FROM THE INTERNET-------------------#

#        26376  Alpine   Lane, Twin Peaks,  CA    92391
#       |_____| |_____| |____| |_________| |__|  |____|
#     House nr. Str.nm Str.Type  City      State  Postal Code

#---------------EXAMPLE-FROM THE INTERNET-------------------#

# I'll make a new column state and delete the column address


match <- regmatches(train_X_impute$address, regexec("[A-Z][A-Z]", train_X_impute$address))
match <- regmatches(test_X_impute$address, regexec("[A-Z][A-Z]", test_X_impute$address))
train_X_impute$state <- sapply(match, `[`, 1)
test_X_impute$state <- sapply

cats <- categories(train_X_encode[, c("State")], p = 10)

col.to.drop <- c('address')
train_X_impute = train_X_impute[ , ! names(train_X_impute) %in% col.to.drop]
test_X_impute = test_X_impute[ , ! names(test_X_impute) %in% col.to.drop]





