library(randomForest)
set.seed(1)
# https://www.youtube.com/watch?v=GKdv76N5IHY


#small glitch
#train_X_fe <- train_X_fe[train_X_fe$state != 'WN',]
########################################################
class(train_X_fe_mod$state)
train_X_fe_mod <- droplevels(train_X_fe, exclude = "WY")
#########################################################

colMeans(is.na(train_X_fe))
colMeans(is.na(test_X_fe))


fit <- randomForest(train_y ~.,data = train_X_fe, mtry = 5, ntree = 25)
fit <- randomForest(x = train_X_fe, y = train_y, mtry = 5, ntree = 25)
y_estimate <- predict(fit,xtest)
cf.test <- table(y_estimate,train_y)


## id is character en default is numeric
y_estimate <- as.numeric(as.character(y_estimate))
test_X$id <- as.character(test_X$id)

class(y_estimate)
class(test_X$id)

rf_preds_df <- data.frame(id = test_X$id, default = y_estimate)

write.csv(rf_preds_df, file = "C:\\Users\\Milan\\Documents\\Master hir 1\\Machine Learning Dries Benoit\\KaggleComp\\model1.csv", row.names = F)
?write.csv



#### DIT IS IETS VAN STACK_OVERFLOW ANDERS HAD IK EEN BUG
xtest <- rbind(train_X_fe[1, ] , test_X_fe)
xtest <- xtest[-1,]
#### DIT IS IETS VAN STACK_OVERFLOW ANDERS HAD IK EEN BUG








unique(train_X_fe_mod$state)