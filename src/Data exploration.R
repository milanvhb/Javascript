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
#4) 

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


