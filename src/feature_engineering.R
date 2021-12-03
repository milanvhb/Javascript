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




