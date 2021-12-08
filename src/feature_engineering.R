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





