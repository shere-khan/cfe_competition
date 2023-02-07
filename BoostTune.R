setwd("/Users/yuan/Documents/GitHub/cfe_competition_2018/data")
setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/data"))
data <- #your cleaned data with 27 features
data.test <- read.csv('your final test data')
#data <- read.csv('filted_train.csv')



##########copy from below 
set.seed(1)
train <- sample(1:nrow(data),nrow(data)/2)

########set number of tree to 10000 first, and use cv plot to find a suitable tree number, it takes around 20 mins to run 
library(gbm)
set.seed(123)
#creare LoanStatus to binary value
data$LoanStatus <- unclass(data$LoanStatus)
data$LoanStatus <- ifelse(data$LoanStatus==1, 1, 0)
boost.cfe.1 <- gbm(data[train,]$LoanStatus~., data=data[train,], distribution = 'bernoulli', n.trees = 10000, interaction.depth = 3, cv.folds = 3, shrinkage = 0.1)
summary(boost.cfe.1)
sqrt(min(boost.cfe.1$cv.error))
gbm.perf(boost.cfe.1, method = "cv")  #lowest cv error at 2468 trees

##############code to tune lambda and depth
# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(0.1, 0.01),
  interaction.depth = c(3, 5),
  optimal_trees = 0,               # a place to save results
  min_MSE = 0                      # a place to save results
)

# total number of combinations
nrow(hyper_grid)

#following block of code run around 1hour
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(data[train,]$LoanStatus~.,
                  distribution = "bernoulli",
                  data = data[train,],
                  n.trees = 5000,
                  shrinkage = hyper_grid$shrinkage[i],
                  interaction.depth = hyper_grid$interaction.depth[i],
                  cv.folds = 2
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$cv.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$cv.error))
}

#final model parameters: number of trees 7000, shrinkage 0.01, tree depth 5
#train on whole train set with 50% threshold, and test is on the test set with lables
final.cfe <- gbm(data$LoanStatus~., data=data, distribution = 'bernoulli', n.trees = 7000, interaction.depth = 5, shrinkage = 0.01)
summary(boost.cfe)
predicted.boost <- predict(final.cfe, newdata = data.test, n.trees = 7000, type="response")
predicted.boost <- ifelse(predicted.boost >= 0.5, 1, 0)
table(predicted.boost, data.test$LoanStatus)
mean(predicted.boost == data.test$LoanStatus) 
#different threshold 70%, to get lower false positive rate
predicted.boost.1 <- predict(final.cfe, newdata = data.test, n.trees = 7000, type="response")
predicted.boost.1 <- ifelse(predicted.boost.1 >= 0.7, 1, 0)
table(predicted.boost.1, data.test$LoanStatus)
mean(predicted.boost.1 == data.test$LoanStatus) 



