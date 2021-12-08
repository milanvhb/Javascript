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
train_X_fe$sub_grade



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





