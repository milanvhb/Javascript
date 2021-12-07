#################
#DATA EXPLORATION
#################
train <- read.csv("../data/bronze/train.csv")
test_X <- read.csv("../data/bronze/test.csv")
head(train)

#Create our training sets
train_X <- subset(train, select = -c(default))
train_y <- train$default
ls.str(train_X)

#WHAT TO LOOK FOR:
#1) Missing values (can be done in one simple step)
#2) Outliers
#4) Can we bin certain features, continuous variables that are spread out a lot?
#3) for non numerical variables: check how many categories or what kind of information


num.cols <- sapply(train_X_impute, is.numeric) 
train_X_num <- train_X[ ,num.cols]
train_X_num

train_X[columns == num.cols]

########
#Summary
########

#1) ID: drop ID columns
#2) ADDRESS: Drop address column, maybe keep state as a feature
#3) AMOUNT: Do nothing
#4) Annual income: 
#5) application_type: create dummy vars 
#6) date funded: create new var: how long since loan is funded 
#7) debt_to_income: remove outlier 
#8) earliest_cr_line: create new var: how long since loan is funded 
#9) emp_length

#####
#1 ID
#####

#No need to check


##########
#2 address
##########
train_X$address[354]

#Addess structure is: 92462 Jacob Views Suite 768\nStoneside, MA 00813
# "House number" "Street name" *Kind of living facility* *Appartment number* "city" "State" "Zipcode"

#Personally I don't think any of this information is usefull except state MAYBE
#If you look these addresses up on google maps none of them are real



##########
#3) amount (of the loan)
##########

#all values can be borrowed here so no real outliers need to be omitted, even though the boxplot
#might indicate otherwise

hist(train_X$amount)
boxplot(train_X$amount)
sort(train_X$amount, TRUE)

#################
#4) annual income
#################

hist(train_X$annual_income)
boxplot(train_X$annual_income) #CLEAR indication of outliers, we should probably remove some of them
sort(train_X$annual_income, TRUE) #likewise here, almost all data is for the lower income people
sort(train_X$annual_income, FALSE) 

#################
#5) application_type
#################

#missing values? 
sum(is.na(train_X$application_type)) #no missing values 

#unique categories? 
unique(train_X$application_type) #3 different categories: Individual, Direct_Pay and Joint --> create dummy variables on the 3? 

#how many times does every category occurs
table(train_X$application_type, useNA = "ifany") #direct pay: 75, individual: 70813, joint: 66 
barplot(prop.table(table(train_X$application_type)))

#in my opinion: just add this var as dummy variables 

#################
#6) date funded: the month and year in which the loan was funded 
#################
sum(is.na(train_X$date_funded)) #0
#convert to dates, create some kind of recency to know how it has been since loan was funded. 
#idea: the longer ago the loan was funded, the more likely they are to repay them
#Input Sam: the thing is, there are no "current loans", so then we should create a date-var for when the loan terminated
sort(train_X$date_funded, FALSE) #no weird dates


#################
#7) debt_to_income: 
#################

hist(train_X$debt_to_income)
boxplot(train_X$debt_to_income) #Only a few outliers, we should probably remove the biggest one (D/I ratio > 1500: probably something has gone wrong here)
sort(train_X$debt_to_income, TRUE) 
sort(train_X$debt_to_income, FALSE) 


#################
#8) earliest_cr_line:  the month the borrower's earliest reported credit line was opened  
#################

#same idea as date funded: the longer it has been since a customer has opened a credit line, the more likely to be able to pay off the loan? 
sort(train_X$date_funded, FALSE) #no weird dates 

head(train_X)

#################
#9) emp_length: emp_length - employment length in years. Possible values are between 0 and 10 where 0 means less than one year and 10 means ten or more years 
#################

#missing values? 
sum(is.na(train_X$emp_length)) # 4591 missing values/emp_lengths 
table(train_X$emp_length, useNA = "ifany")

barplot(prop.table(table(train_X$emp_length, useNA = "ifany")),las = 2)

#grade
sum(is.na(train_X$grade)) #no nan
barplot(prop.table(table(train_X$grade, useNA = "ifany")),las = 2)

#sub-grade
sum(is.na(train_X$sub_grade)) #no nan
barplot(prop.table(table(train_X$sub_grade, useNA = "ifany")),las = 2)

#homestatus
barplot(prop.table(table(train_X$home_status, useNA = "ifany")),las = 2)
prop.table(table(train_X$home_status, useNA = "ifany"))

#income_ver_status
barplot(prop.table(table(train_X$income_verif_status, useNA = "ifany")),las = 2)
prop.table(table(train_X$income_verif_status, useNA = "ifany"))

#purpose
barplot(prop.table(table(train_X$purpose, useNA = "ifany")),las = 2)
prop.table(table(train_X$purpose, useNA = "ifany"))

#interest_rate
sum(is.na(train_X$interest_rate)) #no nan
boxplot(train_X$interest_rate) #some outliers

#monthly_payment =====> Maybe bin?

sum(is.na(train_X$monthly_payment)) #2513 nan
boxplot(train_X$monthly_payment) 
#num_bankrupts

sum(is.na(train_X$num_bankrupts)) #4670 nan
barplot(prop.table(table(train_X$num_bankrupts, useNA = "ifany")),las = 2)
prop.table(table(train_X$num_bankrupts, useNA = "ifany"))
#num_mortgages
sum(is.na(train_X$num_mortgages)) #6086 nan
boxplot(train_X$num_mortgages)
barplot(prop.table(table(train_X$num_mortgages, useNA = "ifany")),las = 2)
prop.table(table(train_X$num_mortgages, useNA = "ifany"))

#num_open_credit
sum(is.na(train_X$num_open_credit)) #no nan
boxplot(train_X$num_open_credit)
barplot(prop.table(table(train_X$num_open_credit, useNA = "ifany")),las = 2)
prop.table(table(train_X$num_open_credit, useNA = "ifany"))
#num_records
sum(is.na(train_X$num_records)) #4570 nan
boxplot(train_X$num_records)
barplot(prop.table(table(train_X$num_records, useNA = "ifany")),las = 2)
prop.table(table(train_X$num_records, useNA = "ifany"))
#num_total_credit ===> bin?
sum(is.na(train_X$num_total_credit)) #2038
boxplot(train_X$num_total_credit)
barplot(prop.table(table(train_X$num_total_credit, useNA = "ifany")),las = 2)
prop.table(table(train_X$num_total_credit, useNA = "ifany"))
#revol_balance
sum(is.na(train_X$revol_balance)) #no nan 
boxplot(train_X$interest_rate)
#revol_util
sum(is.na(train_X$revol_util)) #1327 nan
boxplot(train_X$revol_util)
