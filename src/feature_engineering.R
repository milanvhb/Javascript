#Import possible datasets
train <- read.csv("../data/silver/train_cleaned_data.csv")
#train <- read.csv("../data/silver/train_cleaned_less_restrictive_data.csv")
#train <- read.csv("../data/silver/train_cleaned_NA_data.csv")
#train <- read.csv("../data/silver/train_cleaned_with_outlier.csv")
test_X_fe <- read.csv("../data/silver/test_cleaned_data.csv")

train_X_fe <- subset(train, select = -c(default))
train_y <- train$default

#########################################################
#1. TURNING INDIVIDUAL FEATURES INTO MORE USABLE FEATURES
#########################################################

############
#HOUSE_TYPE
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

unique(train_X_fe$grade)
unique(train_X_fe$sub_grade)
grade_levels <- c("A", "B", "C", "D", "E", "F", "G")
train_X_fe$grade <- as.numeric(factor(train_X_fe$grade, levels = grade_levels))
test_X_fe$grade <- as.numeric(factor(test_X_fe$grade, levels = grade_levels))
subgrade_levels <- c("A1", "A2", "A3", "A4", "A5",
                     "B1", "B2", "B3", "B4", "B5",
                     "C1", "C2", "C3", "C4", "C5",
                     "D1", "D2", "D3", "D4", "D5",
                     "E1", "E2", "E3", "E4", "E5",
                     "F1", "F2", "F3", "F4", "F5")
train_X_fe$sub_grade <- as.numeric(factor(train_X_fe$sub_grade, levels = subgrade_levels))
test_X_fe$sub_grade <- as.numeric(factor(test_X_fe$sub_grade, levels = subgrade_levels))

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
train_X_fe$year_customer <- train_X_fe$date_funded - train_X_fe$earliest_cr_line 
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





