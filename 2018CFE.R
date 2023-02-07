# setwd("/home/justin/pycharmprojects/cfe_competition_2018/data/")
# setwd("/Users/yuan/Documents/GitHub/cfe_competition_2018/data")
setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/data"))
#library(tidyverse)
data <- read.csv('train.csv')
dim(data)
names(data)

data.t <- read.csv('test.csv')

#plot AppReceiveDate between train and test
library(ggplot2)

data$AppReceiveDate <- as.Date( data$AppReceiveDate, '%m/%d/%Y')
data.t$AppReceiveDate <- as.Date( data.t$AppReceiveDate, '%m/%d/%Y')

ggplot(data, aes(x = AppReceiveDate)) + geom_histogram(binwidth = 2, fill = 'grey')
ggplot(data.t, aes(x = AppReceiveDate)) + geom_histogram(binwidth = 2, fill = 'grey')

# library(dplyr)
# #exploratory date analysis(EDA)
# attach(data)
# #plot for descriptive summary 
# #ModifiedCreditScore
# boxplot(ModifiedCreditScore,LoanStatus)
# hist(ModifiedCreditScore[LoanStatus=='Approved'])
# hist(ModifiedCreditScore[LoanStatus=='Declined'])
# summary(ModifiedCreditScore)
# summary(ModifiedCreditScore[LoanStatus=='Approved'])
# summary(ModifiedCreditScore[LoanStatus=='Declined'])
# #ModifiedBankruptcyScore
# boxplot(ModifiedBankruptcyScore,LoanStatus)
# hist(ModifiedBankruptcyScore[LoanStatus=='Approved'])
# hist(ModifiedBankruptcyScore[LoanStatus=='Declined'])
# summary(ModifiedBankruptcyScore)
# summary(ModifiedBankruptcyScore[LoanStatus=='Approved'])
# summary(ModifiedBankruptcyScore[LoanStatus=='Declined'])
# 
# 
# LoanStatus <- as.factor(LoanStatus)
# table(LoanStatus)
# # data description
# # skip variables which seems not important: LoanNumber, AppReceivedData, 
# 
# #categorical variables
# 
#Source, 'CONSUMER' has higher chance to get declined
train$Source <- as.factor(Source)#need to use 'train$', otherwise no update of var type

table(data$Source,data$LoanStatus)
prop.table(table(data$Source,data$LoanStatus))
par(col.lab="red")
#compare LoanStatus for each variables
plot(data$LoanStatus~data$Source,col=terrain.colors(3))
#put two plots side by side
par(mfrow=c(1,2))
plot(data$Source[which(data$LoanStatus=='Approved')],col=terrain.colors(3),las=1)
plot(data$Source[which(data$LoanStatus=='Declined')],col=terrain.colors(3),las=1)
#reset par
par(mfrow=c(1,1))
# 
# 

# #difference, we will combine those values.
# table(VehicleMake,LoanStatus)
# plot(LoanStatus~VehicleMake,col=terrain.colors(3))
# #consider to change all brands to upper case, as some brands have two values because of upper/lower case
# 
# 

#isNewVehicle, not new Vehicle has slightly higher chance to be declined
train$isNewVehicle <- as.factor(train$isNewVehicle)
plot(table(isNewVehicle,LoanStatus),col=terrain.colors(3))


#OccupancyStatus, BUYING higher the chance to approve, LIVEWITHPARENTS and RENT is thumb down.
train$OccupancyStatus <- as.factor(OccupancyStatus)
table(OccupancyStatus,LoanStatus)
plot(LoanStatus~OccupancyStatus,col=terrain.colors(3))


#RequestType, INDIRECT is a thumb down
train$RequestType <- as.factor(RequestType)
table(RequestType,LoanStatus)
plot(LoanStatus~RequestType,col=terrain.colors(3))


#MemberIncicator, Membership is a plus
train$MemberIndicator <- as.factor(MemberIndicator)
table(MemberIndicator,LoanStatus)
plot(LoanStatus~MemberIndicator,col=terrain.colors(3))
# 
# 

#CoApplicantIndicator, N has slightly higher Declined rate, but it seems trivial
train$CoApplicantIndicator <- as.factor(CoApplicantIndicator)
table(CoApplicantIndicator,LoanStatus)
par(col.lab="red")
plot(LoanStatus~CoApplicantIndicator,col=terrain.colors(3))
# 
# #CoMonthlyLiability, when equal to 'NULL', has little higher change to fail
# plot(LoanStatus[which(CoMonthlyLiability=='0')]~CoMonthlyLiability[which(CoMonthlyLiability=='0')],col=terrain.colors(3),las=2)
# plot(LoanStatus[which(CoMonthlyLiability=='NULL')]~CoMonthlyLiability[which(CoMonthlyLiability=='NULL')],col=terrain.colors(3),las=2)
# 
# #CoMonthlyRent, when equal to 'NULL', has little higher change to fail
# plot(LoanStatus[which(CoMonthlyRent=='NULL')]~CoMonthlyRent[which(CoMonthlyRent=='NULL')],col=terrain.colors(3),las=2)
# plot(LoanStatus[which(CoMonthlyRent=='0')]~CoMonthlyRent[which(CoMonthlyRent=='0')],col=terrain.colors(3),las=2)
# 
# 
# ########################
# #logistic regression
# #poor results
# logreg.fit <- glm(LoanStatus~Source+EmploymentStatus+VehicleMake+RequestType+OccupancyStatus+MemberIndicator + 
#                     ModifiedBankruptcyScore+ModifiedCreditScore+TotalVehicleValue+DownPayment+OccupancyDuration+
#                     NumberOfOpenRevolvingAccounts+PrevEmployedMonths+EmployedMonths+PrimeMonthlyIncome+TotalMonthlyIncome
#                   +PrimeMonthlyLiability+PrimeMonthlyRent+TotalMonthlyDebtBeforeLoan+AmountRequested+Loanterm
#                   , data = train1,family = binomial)
# summary(logreg.fit)
# coef(logreg.fit)
# logreg.probs <- predict(logreg.fit,type = 'response')
# logreg.pred <- rep("Declined",4990)
# logreg.pred[logreg.probs>0.5] <- 'Approved'
# table(logreg.pred,LoanStatus)
# mean(logreg.pred==LoanStatus)

##################################
#feature preparation
str(data)
data$LTV <- as.numeric(as.character(data$LTV))
data$DTI <- as.numeric(as.character(data$DTI))

#impute LTV and DTI with median(not mean, many outliers)
data$LTV[is.na(data$LTV)] <- 1.022
data$DTI[is.na(data$DTI)] <- 0.243
data$Loanterm[is.na(data$Loanterm)] <- 72.00
data$EstimatedMonthlyPayment[is.na(data$EstimatedMonthlyPayment)] <- 345.83
data$NumberOfOpenRevolvingAccounts[is.na(data$NumberOfOpenRevolvingAccounts)] <- 3.00
###########################
# data$CoMonthlyRent <- as.numeric(as.character(data$CoMonthlyRent))
# data$CoMonthlyLiability <- as.numeric(data$CoMonthlyLiability)

# data$CoMonthlyRent <- as.numeric(data$CoMonthlyRent)
# data$CoMonthlyLiability <- as.numeric(data$CoMonthlyLiability)

# Replace null values in LTV with mean value 2.223
#data$LTV[data$LTV == "NA"] <- 2.223
summary(data$LTV)

# Select only LTV and DTI
library(VIM)
library(dplyr)
data.imp.ltv.dti = select(data, LTV, DTI)
data.imp.emp.lt = select(data, EstimatedMonthlyPayment, Loanterm)
data.imp.nora = select(data, NumberOfOpenRevolvingAccounts)

# View missing data
mice_plot <- aggr(data.imp.ltv.dti, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data.imp.ltv.dti), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Impute missing data
library(mice)
imputed.data.ltv.dti <- mice(data.imp.ltv.dti, m=5, maxit = 50, method = 'pmm', seed = 500)
imputed.data.emp.lt <- mice(data.imp.emp.lt, m=5, maxit = 50, method = 'pmm', seed = 500)
imputed.data.nora <- mice(data.imp.nora, m=5, maxit = 50, method = 'pmm', seed = 500)

# summary(data.imp)
# summary(data.imp$DTI)
# summary(data.imp$LTV) 

# Replace missing values in datafram data.imp with imputed values
complete.data.ltv.dti <- complete(imputed.data.ltv.dti, 2)
complete.data.emp.lt <- complete(imputed.data.emp.lt, 2)
complete.data.nora <- complete(imputed.data.nora, 2)

# Drop columns from data frame
data = subset(data, select = -c(LTV, DTI))
data = subset(data, select = -c(EstimatedMonthlyPayment, Loanterm))
data = subset(data, select = -c(NumberOfOpenRevolvingAccounts))

# Combine data and completedData side-by-side
data.combined = cbind(data, complete.data.ltv.dti)
save(data, file="data.combined.ltv.dti")
data = cbind(data, complete.data.emp.lt)
data = cbind(data, complete.data.nora)

##########
data$CoMonthlyRent <- as.numeric(data$CoMonthlyRent)
data$CoMonthlyLiability <- as.numeric(data$CoMonthlyLiability)

#delete non-relevent columns
data <- data[,!(names(data) %in% c("AppReceiveDate", "LoanNumber"))]

#remove some features which seem not important
data <- data[,!(names(data) %in% c("PrimeMonthlyIncome", "CototalMonthlyIncome", "PrimeMonthlyLiability", 
                                   "CoMonthlyLiability", "PrimeMonthlyRent", "CoMonthlyRent", "VehicleMake"))]

#remove rows with missing value
dim(data[!complete.cases(data),])
data <- data[complete.cases(data),]
names(data)

#remove outliers of EmployedMonths, CoEmployedMonths, PrevEmployedMonths,TotalMonthlyIncome
#CototalMonthlyIncome,PrimeMonthlyIncome,VehicleMileage,PrimeMonthlyRent,Loanterm
#EstimatedMonthlyPayment
plot(data$EmployedMonths,ylab = 'EmployedMonths')
index_out.emp_m <- which(data$EmployedMonths > 1000)
data <- data[-index_out.emp_m,] #outliers deleted
plot(data$CoEmployedMonths,ylab = 'CoEmployedMonths')
index_out.coemp_m <- which(data$CoEmployedMonths > 500)
data <- data[-index_out.coemp_m,]
plot(data$PrevEmployedMonths,ylab = 'PrevEmployedMonths')
index_out.pre_m <- which(data$PrevEmployedMonths > 400)
data <- data[-index_out.pre_m,]

#PrimeMonthlyIncome outliers
plot(data$PrimeMonthlyIncome)
index_out.pri_inc <- which(data$PrimeMonthlyIncome > 500000)#one application with 700000monthincome
#asking for 36000 loan, it's very like a typo
data <- data[-index_out.pri_inc,]
plot(data$CototalMonthlyIncome)
index_out.co_to_inc <- which(data$CototalMonthlyIncome > 900000)
data <- data[-index_out.co_to_inc,]
plot(data$TotalMonthlyIncome)
index_out.to_m_inc <- which(data$TotalMonthlyIncome > 150000)
data <- data[-index_out.to_m_inc,]
plot(data$PrimeMonthlyRent)
index_out.pri_ren <- which(data$PrimeMonthlyRent > 20000)
data <- data[-index_out.pri_ren,]
plot(data$VehicleMileage)
index_out.veh_mil <- which(data$VehicleMileage > 1000000)
data <- data[-index_out.veh_mil,]
plot(data$TotalVehicleValue)
index_out.veh_val <- which(data$TotalVehicleValue > 2500000)
data <- data[-index_out.veh_val,]
plot(data$AmountRequested)
index_out.amount <- which(data$AmountRequested > 300000)
data <- data[-index_out.amount,]
plot(data$Loanterm)
index_out.term <- which(data$Loanterm > 100)
data <- data[-index_out.term,]
plot(data$EstimatedMonthlyPayment)
index_out.est_pay <- which(data$EstimatedMonthlyPayment > 10000)
data <- data[-index_out.est_pay,]
plot(data$DTI)
index_out.DTI <- which(data$DTI > 100)
data <- data[-index_out.DTI,]
plot(data$LTV)
index_out.LTV <- which(data$LTV > 10)
data <- data[-index_out.LTV]
dim(data)

#handling multicolinearity, only for numeric features
cor(data[, sapply(data, class) != "factor"])

#some features have high(more than 0.9) colinearity, remove those features
#PrimeMonthlyLiability and CoMonthlyRent
data <- data[,-which(names(data) %in% c("PrimeMonthlyLiability","CoMonthlyRent"))]


#randomely selected sample subset
# set.seed(1)
# sub <- data[sample(nrow(data),10000),]

#split into train and test data
set.seed(1)
train <- sample(1:nrow(data),nrow(data)/2)
test <- (-train)


###################
#backward stepwise selection
library(MASS)
full.model <- glm(LoanStatus ~., data = data[train,], family = binomial)
summary(full.model)
step.model <- stepAIC(full.model,direction = 'backward')
summary(step.model)
probabilities.step <- predict(step.model, data = data[test,], type = "response")
predicted.step <- ifelse(probabilities.step > 0.5, "Approved", "Declined")
# Prediction accuracy
observed.step <- data[test,]$LoanStatus
mean(predicted.step == observed.step)

##############
#lasso
#create x matrix and y vector
train.x <- model.matrix(LoanStatus~.,data[train,])[,-1]
train.y <- ifelse(data[train,]$LoanStatus=='Approved',1,0)
test.x <- model.matrix(LoanStatus~.,data[test,])[,-1]
test.y <- data[test,]$LoanStatus

library(glmnet)
#grid <- 10^seq(10,-2,length=100)
# lasso.mod <- glmnet(train.x,train.y,alpha = 1,family="binomial",lambda=grid)
# plot(lasso.mod)
# dim(coef(lasso.mod))


#cross validation and find best lamba value
set.seed(123)
cv.lasso = cv.glmnet(train.x,train.y,alpha = 1,family="binomial")
plot(cv.lasso)
(bestlam <- cv.lasso$lambda.min)
(cv.lasso$lambda.1se)
model <- glmnet(train.x, train.y, alpha = 1, family = "binomial",
                lambda = bestlam)
coef(model)

# simpler model with higher lambda value
model.1se <- glmnet(train.x, train.y, alpha = 1, family = "binomial",
                    lambda = 0.001)
coef(model.1se)

# Make predictions on the test data
probabilities <- predict(model,type = 'response',newx = test.x)
predicted.classes <- ifelse(probabilities >= 0.5, "Approved", "Declined")
# Model accuracy
observed.classes <- test.y
table(predicted.classes,observed.classes)
mean(predicted.classes == observed.classes)
# set criteria to 70%
predicted.classes70 <- ifelse(probabilities > 0.7, "Approved", "Declined")
table(predicted.classes70,observed.classes)
mean(predicted.classes70 == observed.classes)
# set criteria to 80%
predicted.classes80 <- ifelse(probabilities > 0.8, "Approved", "Declined")
table(predicted.classes80,observed.classes)
mean(predicted.classes80 == observed.classes)
# set criteria to 90%
predicted.classes90 <- ifelse(probabilities > 0.9, "Approved", "Declined")
table(predicted.classes90,observed.classes)
mean(predicted.classes90 == observed.classes)


# Make predictions on the test data with simpler model
probabilities.1se <- predict(model.1se,type = 'response',newx = test.x)
predicted.1se <- ifelse(probabilities.1se > 0.5, "Approved", "Declined")
# Model accuracy
observed.classes <- test.y
mean(predicted.1se == observed.classes) # 1% less than best lambda

# #predict with different size of test data set, 9986 rows
# set.seed(123)
# test1 <- data[sample(nrow(data),10000),]
# test1 <- test1[complete.cases(test1),] #remove missing value rows, 9986 left
# test1.x <- model.matrix(LoanStatus~.,test1)[,-1]
# test1.y <- test1$LoanStatus
# test1.prob <- predict(model,type = 'response',newx = test1.x)
# test1.pred <- ifelse(test1.prob>0.5, "Approved", "Declined")
# table(test1.pred,test1.y)
# mean(test1.pred==test1.y)


######################
#random forest
library(randomForest)
set.seed(123)
bag.cfe.full <- randomForest(data$LoanStatus~., data = data, subset = train, mtry = 27, importance = T)
bag.cfe <- randomForest(data$LoanStatus~., data = data, subset = train, mtry = 6, importance = T)
plot(bag.cfe)
importance(bag.cfe)
varImpPlot(bag.cfe)
predicted.rf <- predict(bag.cfe, data[-train,], type = "class")
table(predicted.rf, data[-train,]$LoanStatus)
mean(predicted.rf == data[-train,]$LoanStatus) # accuracy 86%

#######################
#Boosting
data <- read.csv('filted_train.csv')
library(gbm)
set.seed(123)
#creare LoanStatus to binary value
data$LoanStatus <- unclass(data$LoanStatus)
data$LoanStatus <- ifelse(data$LoanStatus==1, 1, 0)
boost.cfe.1 <- gbm(data[train,]$LoanStatus~., data=data[train,], distribution = 'bernoulli', n.trees = 5000, interaction.depth = 5, cv.folds = 5, shrinkage = 0.001) 
summary(boost.cfe.1)
plot(boost.cfe.1, i = 'ModifiedCreditScore')
plot(boost.cfe.1, i = 'DTI')
#prediction
predicted.boost <- predict(boost.cfe.1, newdata = data[-train,], n.trees = 5000, type="response")
predicted.boost <- ifelse(predicted.boost >= 0.5, 1, 0)
table(predicted.boost, data[-train,]$LoanStatus)
mean(predicted.boost == data[-train,]$LoanStatus)  #accuracy 88.38%, so far the best

#adjust lambda value from default 0.001 to 0.2
boost.cfe.2 <- gbm(data[train,]$LoanStatus~., data=data[train,], distribution = 'bernoulli', n.trees = 5000, interaction.depth = 5,
                   shrinkage=0.2) 
summary(boost.cfe.2)
predicted.boost2 <- predict(boost.cfe.2, newdata = data[-train,], n.trees = 5000, type="response")
predicted.boost2 <- ifelse(predicted.boost >= 0.5, 1, 0)
table(predicted.boost2, data[-train,]$LoanStatus)
mean(predicted.boost == data[-train,]$LoanStatus)  #accuracy 87.5%
#adjust depth to 4
boost.cfe.3 <- gbm(data[train,]$LoanStatus~., data=data[train,], distribution = 'bernoulli', n.trees = 5000, interaction.depth = 4)
summary(boost.cfe.3)
predicted.boost3 <- predict(boost.cfe.3, newdata = data[-train,], n.trees = 5000, type="response")
predicted.boost3 <- ifelse(predicted.boost >= 0.5, 1, 0)
table(predicted.boost3, data[-train,]$LoanStatus)
mean(predicted.boost3 == data[-train,]$LoanStatus)  #accuracy 87.5%
#adjust depth to 6
boost.cfe.4 <- gbm(data[train,]$LoanStatus~., data=data[train,], distribution = 'bernoulli', n.trees = 5000, interaction.depth = 6)
summary(boost.cfe.4)
predicted.boost4 <- predict(boost.cfe.4, newdata = data[-train,], n.trees = 5000, type="response")
predicted.boost4 <- ifelse(predicted.boost >= 0.5, 1, 0)
table(predicted.boost4, data[-train,]$LoanStatus)
mean(predicted.boost4 == data[-train,]$LoanStatus)  #accuracy 87.5%

##################test


boost.cfe.1 <- gbm(data[train,]$LoanStatus~., data=data[train,], distribution = 'bernoulli', n.trees = 5000, interaction.depth = 5, cv.folds = 5, shrinkage = 0.01) 
summary(boost.cfe.1)
sqrt(min(boost.cfe.1$cv.error))
gbm.perf(boost.cfe.1, method = "cv")


# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(0.1, 0.01),
  interaction.depth = c(3, 5),
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)

#boost.cfe.1 <- gbm(data[train,]$LoanStatus~., data=data[train,], 
#distribution = 'bernoulli', n.trees = 2000, interaction.depth = 1, cv.folds = 5) 
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

library(magrittr)
hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

########## for final submission


########set number of tree to 10000 first, and use cv plot to find a suitable tree number, it takes around 20 mins to run 
library(gbm)
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

###########final solution boosting model: number of trees 7000, shrinkage 0.01, tree depth 5
boost.cfe <- gbm(data$LoanStatus~., data=data, distribution = 'bernoulli', n.trees = 7000, interaction.depth = 5, shrinkage = 0.01)
summary(boost.cfe)
predicted.boost <- predict(boost.cfe, newdata = data.test, n.trees = 7000, type="response")
predicted.boost <- ifelse(predicted.boost >= 0.5, 1, 0)
table(predicted.boost, data.test$LoanStatus)
mean(predicted.boost == data.test$LoanStatus) 

############solution with different threshold 70%, to get lower false positive rate
boost.cfe.1 <- gbm(data$LoanStatus~., data=data, distribution = 'bernoulli', n.trees = 7000, interaction.depth = 5, shrinkage = 0.01)
summary(boost.cfe1)
predicted.boost1 <- predict(boost.cfe1, newdata = data.test, n.trees = 7000, type="response")
predicted.boost1 <- ifelse(predicted.boost1 >= 0.7, 1, 0)
table(predicted.boost1, data.test$LoanStatus)
mean(predicted.boost1 == data.test$LoanStatus) 

###############

#final parameters: number of trees 7000, shrinkage 0.01, tree depth 5
#validation on train set with 50% threshold
boost.cfe <- gbm(data[train,]$LoanStatus~., data=data[train,], distribution = 'bernoulli', n.trees = 7000, interaction.depth = 5, shrinkage = 0.01)
summary(boost.cfe)
predicted.boost <- predict(boost.cfe, newdata = data[-train,], n.trees = 7000, type="response")
predicted.boost <- ifelse(predicted.boost >= 0.5, 1, 0)
table(predicted.boost, data[-train,]$LoanStatus)
mean(predicted.boost == data[-train,]$LoanStatus) 
#different threshold 70%, to get lower false positive rate
boost.cfe.1 <- gbm(data[train,]$LoanStatus~., data=data[train,], distribution = 'bernoulli', n.trees = 7000, interaction.depth = 5, shrinkage = 0.01)
summary(boost.cfe1)
predicted.boost1 <- predict(boost.cfe1, newdata = data[-train,], n.trees = 7000, type="response")
predicted.boost1 <- ifelse(predicted.boost1 >= 0.7, 1, 0)
table(predicted.boost1, data[-train,]$LoanStatus)
mean(predicted.boost1 == data[-train,]$LoanStatus) 

