# Created by: justin barry, yuan shao, junshuai feng
# Created on: 1/15/19

############################## SETUP ENVIRONMENT #############################

setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/data"))
data <- read.csv('train.csv')
data.t <- read.csv('test.csv')


############################## ENSURE ALL COLUMNS ARE NUMERIC #############################
data$CoMonthlyLiability <- as.numeric(as.character(data$CoMonthlyLiability))
data$PrimeMonthlyLiability <- as.numeric(as.character(data$PrimeMonthlyLiability))
data$PrimeMonthlyRent <- as.numeric(as.character(data$PrimeMonthlyRent))
data$CoMonthlyRent <- as.numeric(as.character(data$CoMonthlyRent))
data$LTV <- as.numeric(as.character(data$LTV))
data$DTI <- as.numeric(as.character(data$DTI))
data$Loanterm<- as.numeric(as.character(data$Loanterm))
data$EstimatedMonthlyPayment<- as.numeric(as.character(data$EstimatedMonthlyPayment))
data$NumberOfOpenRevolvingAccounts<- as.numeric(as.character(data$NumberOfOpenRevolvingAccounts))


################# VIEW MISSING DATA ##############################
# Generate missing-value graph for numeric values only.
library(VIM)
library(dplyr)
data.imp = select(data, LTV, DTI, CoMonthlyLiability, PrimeMonthlyLiability,
                  PrimeMonthlyRent, CoMonthlyRent, Loanterm, EstimatedMonthlyPayment,
                  NumberOfOpenRevolvingAccounts)

# View missing data
mice_plot <- aggr(data.imp, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data.imp), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


################# IMPUTATION ########################################################
data$CoMonthlyLiability [is.na(data$CoMonthlyLiability )] <- 0.0
data$CoMonthlyRent [is.na(data$CoMonthlyRent)] <- 0.0
data$LTV[is.na(data$LTV)] <- 1.000
data$DTI[is.na(data$DTI)] <- 0.243
data$Loanterm[is.na(data$Loanterm)] <- 72.00
data$EstimatedMonthlyPayment[is.na(data$EstimatedMonthlyPayment)] <- 345.83
data$NumberOfOpenRevolvingAccounts[is.na(data$NumberOfOpenRevolvingAccounts)] <- 3.00


################ VIEW OUTLIERS #######################################################
# This section creates box plots to view outliers and places them into groups of size six.
par(mfrow=c(2, 3))
boxplot(data$LTV, main="LTV")
boxplot(data$DTI, main="DTI")
boxplot(data$Loanterm, main="Loanterm")
boxplot(data$ModifiedCreditScore, main="ModifiedCreditScore")
boxplot(data$PrevEmployedMonths, main="PrevEmployedMonths")
boxplot(data$PrimeMonthlyIncome, main="PrimeMonthlyIncome")
boxplot(data$CoMonthlyRent, main="CoMonthlyRent")
boxplot(data$VehicleMileage, main="VehicleMileage")
boxplot(data$DownPayment, main="DownPayment")
boxplot(data$EstimatedMonthlyPayment, main="EstimatedMonthlyPayment")
boxplot(data$ModifiedBankruptcyScore, main="ModifiedBankruptcyScore")
boxplot(data$CoEmployedMonths, main="CoEmployedMonths")
boxplot(data$CototalMonthlyIncome, main="CototalMonthlyIncome")
boxplot(data$CoMonthlyLiability, main="CoMonthlyLiability")
boxplot(data$TotalMonthlyDebtBeforeLoan, main="TotalMonthlyDebtBeforeLoan")
boxplot(data$TotalVehicleValue, main="TotalVehicleValue")
boxplot(data$NumberOfOpenRevolvingAccounts, main="NumberOfOpenRevolvingAccounts")
boxplot(data$EmployedMonths, main="EmployedMonths")
boxplot(data$CoPrevEmployedMonths, main="CoPrevEmployedMonths")
boxplot(data$TotalMonthlyIncome, main="TotalMonthlyIncome")
boxplot(data$PrimeMonthlyRent, main="PrimeMonthlyRent")
boxplot(data$VehicleYear, main="VehicleYear")
boxplot(data$AmountRequested, main="AmountRequested")
boxplot(data$OccupancyDuration, main="OccupancyDuration")


################ VIEW HISTOGRAMS OF NUMERICAL DATA #################################################
# This section furthers data validation by displaying histograms of the numerical data.
library(gridExtra)
library(ggplot2)
attach(data)
# Creating temp variable to remove outliers from DTI. Then will create a histogram
# based off of this new feature to get a general idea of the the Approved/Denied status
# of the distribution of DTI.
data$temp_dti = data$DTI
data$temp_dti[data$DTI < -.5] <- NA
data$temp_dti[data$DTI >= 1] <- NA
p1 <- ggplot(data, aes(data$temp_dti, fill=LoanStatus)) +
  geom_histogram(bins=10) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + xlab("DTI")

# LTV
data$temp_ltv = data$LTV
data$temp_ltv[data$LTV >= 2] <- NA
p2 <- ggplot(data, aes(data$temp_ltv, fill=LoanStatus)) +
  geom_histogram(bins=20) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + xlab("LTV")

# ModifiedCreditScore
data$temp_mcs = data$ModifiedCreditScore
data$temp_mcs[data$ModifiedCreditScore < 400] <- NA
data$temp_mcs[data$ModifiedCreditScore > 900] <- NA
p3 <- ggplot(data, aes(data$temp_mcs, fill=LoanStatus)) +
  geom_histogram(bins=20) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + xlab("ModifiedCreditScore")

# AmountRequested
data$temp_ar = data$AmountRequested
data$temp_ar[data$AmountRequested > 100000] <- NA
p4 <- ggplot(data, aes(data$temp_ar, fill=LoanStatus)) +
  geom_histogram(bins=30) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + xlab("AmountRequested")

# Display histograms in a plot of four.
grid.arrange(p1, p2, p3, p4, nrow=2)

# VehicleMileage
data$temp_vm = data$VehicleMileage
data$temp_vm[data$VehicleMileage > 150000] <- NA
p5 <- ggplot(data, aes(data$temp_vm, fill=LoanStatus)) +
  geom_histogram(bins=30) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_y_continuous(trans='log10') + xlab("VehicleMileage")

# EstimatedMonthlyPayment
data$temp_emp = data$EstimatedMonthlyPayment
data$temp_emp[data$EstimatedMonthlyPayment > 1500] <- NA
p6 <- ggplot(data, aes(data$temp_emp, fill=LoanStatus)) +
  geom_histogram(bins=30) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + xlab("EstimatedMonthlyPayment") +
  scale_y_continuous(trans='log10')

# TotalMonthlyIncome
data$temp_tmi = data$TotalMonthlyIncome
data$temp_tmi[data$TotalMonthlyIncome > 15000] <- NA
p7 <- ggplot(data, aes(data$temp_tmi, fill=LoanStatus)) +
  geom_histogram(bins=30) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + xlab("TotalMonthlyIncome") +
  scale_y_continuous(trans='log10')

# Loanterm
data$temp_lt = data$Loanterm
data$temp_lt[data$Loanterm > 90] <- NA
p8 <- ggplot(data, aes(data$temp_lt, fill=LoanStatus)) +
  geom_histogram(bins=30) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_y_continuous(trans='log10') + xlab("Loanterm")

# Display histograms in a plot of four.
grid.arrange(p5, p6, p7, p8, nrow=2)

# ModifiedBankruptcyScore
data$temp_mbs = data$ModifiedBankruptcyScore
data$temp_mbs[data$ModifiedBankruptcyScore > 600] <- NA
p9 <- ggplot(data, aes(data$temp_mbs, fill=LoanStatus)) +
  geom_histogram(bins=30) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + xlab("ModifiedBankruptcyScore")

# NumberOfOpenRevolvingAccounts
data$temp_nora = data$NumberOfOpenRevolvingAccounts
data$temp_nora[data$NumberOfOpenRevolvingAccounts > 30] <- NA
p10 <- ggplot(data, aes(data$temp_nora, fill=LoanStatus)) +
  geom_histogram(bins=30) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) + xlab("NumberOfOpenRevolvingAccounts") +
  scale_y_continuous(trans='log10')

# TotalMonthlyDebtBeforeLoan
data$temp_tmdbl = data$TotalMonthlyDebtBeforeLoan
data$temp_tmdbl[data$TotalMonthlyDebtBeforeLoan > 6000] <- NA
p11 <- ggplot(data, aes(data$temp_tmdbl, fill=LoanStatus)) +
  geom_histogram(bins=30) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_y_continuous(trans='log10') + xlab("TotalMonthlyDebtBeforeLoan")

# CoEmployedMonths
data$temp_em = data$CoEmployedMonths
data$temp_em[data$CoEmployedMonths > 200] <- NA
p12 <- ggplot(data, aes(data$temp_em, fill=LoanStatus)) +
  geom_histogram(bins=30) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  scale_y_continuous(trans='log10') + xlab("CoEmployedMonths")

# Display histograms in a plot of four.
grid.arrange(p9, p10, p11, p12, nrow=2)

# Remove all temp columns
data <- data[,!(names(data) %in% c("temp_ltv", "temp_ar", "temp_dti", "temp_mcs",
                                   "temp_vm", "temp_emp", "temp_tmi", "temp_lt",
                                   "temp_mbs", "temp_nora", "temp_em", "temp_tmdbl"))]



############### CATEGORICAL FEATURES ANALYSIS AND PLOT ########################################
# plot application number on timeline with train and test(without labels) to check if data sets were randomly
# selected from same time range
library(ggplot2)
data$AppReceiveDate <- as.Date( data$AppReceiveDate, '%m/%d/%Y')
data.t$AppReceiveDate <- as.Date( data.t$AppReceiveDate, '%m/%d/%Y')
ggplot(data, aes(x = AppReceiveDate)) + geom_histogram(binwidth = 2, fill = 'grey')
ggplot(data.t, aes(x = AppReceiveDate)) + geom_histogram(binwidth = 2, fill = 'grey')

# LoanStatus
summary(data$LoanStatus)

# LoanStatus vs Source plot
data$Source <- as.factor(data$Source)
table(data$Source,data$LoanStatus)
prop.table(table(data$Source,data$LoanStatus))
par(col.lab="red")
plot(data$LoanStatus~data$Source,col=terrain.colors(3))

# EmploymentStatus vs LoanStatus
table(data$EmploymentStatus, data$LoanStatus)
plot(data$LoanStatus~data$EmploymentStatus,col=terrain.colors(3))

# VehicleMake vs LoanStatus
table(data$VehicleMake, data$LoanStatus)

# RequestType vs LoanStatus
table(data$RequestType, data$LoanStatus)
plot(data$LoanStatus~data$RequestType,col=terrain.colors(3))

# OccupancyStatus vs LoanStatus
table(data$OccupancyStatus, data$LoanStatus)
plot(data$LoanStatus~data$OccupancyStatus,col=terrain.colors(3))

# LoanStatus vs MemberIndicator
table(data$MemberIndicator,data$LoanStatus)
plot(data$LoanStatus~data$MemberIndicator,col=terrain.colors(3))

# LoanStatus vs CoApplicantIndicator
table(data$LoanStatus, data$CoApplicantIndicator)
plot(data$LoanStatus~data$CoApplicantIndicator, col=terrain.colors(3))

par(mfrow=c(1, 1))

################ FEATURE ENGINEERING #################################################
# Add New Features.
# Create isPaymentDeficit.
data$CashFlow <- data$TotalMonthlyIncome - data$TotalMonthlyDebtBeforeLoan
data$isPaymentDeficit <- data$AmountRequested
data$isPaymentDeficit[data$EstimatedMonthlyPayment <= data$CashFlow] <- 0
data$isPaymentDeficit[data$EstimatedMonthlyPayment > data$CashFlow] <- 1
data$isPaymentDeficit <- as.factor(as.character(data$isPaymentDeficit))

# Create extra column hasEnoughMoney for intuition. This column will help in
# determining the accuracy of is isPaymentDeficit.
data$hasEnoughMoney[data$isPaymentDeficit == 0] <- 1
data$hasEnoughMoney[data$isPaymentDeficit == 1] <- 0
# Display a table comparing hasEnoughMoney to LoanStatus.
table(data$hasEnoughMoney, data$LoanStatus)
# Remove hasEnoughMoney immediately.
data <- data[,!(names(data) %in% c("hasEnoughMoney"))]

# Create DownToAmountRequested
data$DownToAmountRequested <- data$DownPayment / data$AmountRequested



####################### CORRELATION #############################
# Create temporary data frame for computing correlation.
data.cor <- data
# Remove categorical before performing correlation.
data.cor <- data.cor[,!(names(data.cor) %in% c("VehicleMake", "RequestType", "OccupancyStatus",
                                   "isNewVehicle", "EmploymentStatus", "MemberIndicator",
                                   "CoApplicantIndicator", "Source", "LoanStatus", "AppReceiveDate", "LoanNumber",
                                    "isPaymentDeficit", "hasEnoughMoney",
                                   "DownToAmountRequested"))]
# Create correlation matrix.
cor.matrix = cor(data.cor)
library(corrplot)
corrplot(cor.matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.7)

#######################  DELETE NON-RELEVENT COLUMNS #############################
data <- data[,!(names(data) %in% c("AppReceiveDate", "LoanNumber"))]

# Remove correlated features
data <- data[, !(names(data) %in% c("PrimeMonthlyLiability","CoMonthlyLiability",
                                    "PrimeMonthlyRent", "CoMonthlyRent",
                                    "CototalMonthlyIncome", "PrimeMonthlyIncome", "VehicleMake"))]
data <- data[, !(names(data) %in% c('TotalMonthlyExpenses', 'CashFlow'))]

##################### REMOVE ROWS WITH MISSING VALUE #####################
dim(data[!complete.cases(data),])
data <- data[complete.cases(data),]


############################## CREATE TRAIN/TEST #############################
# Following split is 50/50, which can save some time for training
set.seed(1)
train <- sample(1:nrow(data),nrow(data)/2)
test <- (-train)

# Use split below(95/5), if you want to get higher accuracy. It will take longer to train.
set.seed(123)
smp_size <- floor(0.95 * nrow(data))
train <- sample(seq_len(nrow(data)), size = smp_size)
test <- (-train)

################ LOGISTIC REGRESSION WITH LASSO #############################
# Accuracy 80%, it took several minitues to run
# Create x matrix and y vector
train.x <- model.matrix(LoanStatus~.,data[train,])[,-1]
train.y <- ifelse(data[train,]$LoanStatus=='Approved',1,0)
test.x <- model.matrix(LoanStatus~.,data[test,])[,-1]
test.y <- data[test,]$LoanStatus

library(glmnet)
# Cross validation and find best lamba value
set.seed(123)
cv.lasso = cv.glmnet(train.x,train.y,alpha = 1,family="binomial")
plot(cv.lasso)
(bestlam <- cv.lasso$lambda.min)
model <- glmnet(train.x, train.y, alpha = 1, family = "binomial",
                lambda = bestlam)
coef(model)
# Make predictions on the test data
probabilities <- predict(model,type = 'response',newx = test.x)
predicted.classes <- ifelse(probabilities >= 0.5, "Approved", "Declined")
# Model accuracy
observed.classes <- test.y
table(predicted.classes,observed.classes)
mean(predicted.classes == observed.classes)
# Set criteria to 70%
predicted.classes70 <- ifelse(probabilities > 0.7, "Approved", "Declined")
table(predicted.classes70,observed.classes)
mean(predicted.classes70 == observed.classes)


############################## CHANGE LOANSTATUS TO BINARY VALUE #############################
data$LoanStatus <- unclass(data$LoanStatus)
data$LoanStatus <- ifelse(data$LoanStatus==1, 1, 0)



################ RANDOM FORESTS ######################
# Accuracy 86%, it took 20 mins to run

# Run following line first
data$LoanStatus <- as.factor(as.character(data$LoanStatus))

library(randomForest)
set.seed(123)
bag.cfe.full <- randomForest(data$LoanStatus~., data = data, subset = train, mtry = 27, importance = T)
bag.cfe <- randomForest(data$LoanStatus~., data = data, subset = train, mtry = 6, importance = T)
plot(bag.cfe)
importance(bag.cfe)
varImpPlot(bag.cfe)
predicted.rf <- predict(bag.cfe, data[-train,], type = "class")
table(predicted.rf, data[-train,]$LoanStatus)
mean(predicted.rf == data[-train,]$LoanStatus)

################ BOOSTING #########################
# Highest Acuracy 89%, it took around 20 mins to run to build one model

# Be sure to run following line first
data$LoanStatus <- unclass(data$LoanStatus)
data$LoanStatus <- ifelse(data$LoanStatus==1, 1, 0)

library(gbm)
boost.cfe.1 <- gbm(data[train,]$LoanStatus~., data=data[train,],
                   distribution = 'bernoulli', n.trees = 10000,
                   interaction.depth = 3, cv.folds = 3, shrinkage = 0.1)
summary(boost.cfe.1)
sqrt(min(boost.cfe.1$cv.error))
gbm.perf(boost.cfe.1, method = "cv")  #get optimal trees number with lowest cv error
# Prediction with test split
predicted.b <- predict(boost.cfe.1, newdata = data[-train,], n.trees = 7000, type="response")
predicted.b <- ifelse(predicted.b >= 0.5, 1, 0)
table(predicted.b, data[-train,]$LoanStatus)
mean(predicted.b == data[-train,]$LoanStatus)

# Code to tune lambda and depth.
# Create hyperparameter grid.
hyper_grid <- expand.grid(
  shrinkage = c(0.1, 0.01),
  interaction.depth = c(3, 5),
  optimal_trees = 0,               # a place to save results
  min_MSE = 0                      # a place to save results
)

# total number of combinations
nrow(hyper_grid)

# Following block of code run around 1hour
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

# Check cv errors of different parameter combinations, and chose shrinkage 0.01, depth 5 for final solution
hyper_grid




############################ FINAL SOLUTION WITH BOOSTING ###################################
# Final model parameters: number of trees 7000, shrinkage 0.01, tree depth 5
# Train on whole training set, and test on you testing data with lables, it would take around 40 mins to build the model
# Please make sure you use filtered and cleaned data, which should removed missing rows and with 29 features
############## RELOAD AND CLEAN TRAIN DATA################################################
library(gbm)
data <- read.csv('train.csv')

data$LoanStatus <- unclass(data$LoanStatus)
data$LoanStatus <- ifelse(data$LoanStatus==1, 1, 0)
data$CoMonthlyLiability <- as.numeric(as.character(data$CoMonthlyLiability))
data$PrimeMonthlyLiability <- as.numeric(as.character(data$PrimeMonthlyLiability))
data$PrimeMonthlyRent <- as.numeric(as.character(data$PrimeMonthlyRent))
data$CoMonthlyRent <- as.numeric(as.character(data$CoMonthlyRent))
data$LTV <- as.numeric(as.character(data$LTV))
data$DTI <- as.numeric(as.character(data$DTI))
data$Loanterm<- as.numeric(as.character(data$Loanterm))
data$EstimatedMonthlyPayment<- as.numeric(as.character(data$EstimatedMonthlyPayment))
data$NumberOfOpenRevolvingAccounts<- as.numeric(as.character(data$NumberOfOpenRevolvingAccounts))
data$CoMonthlyLiability [is.na(data$CoMonthlyLiability )] <- 0.0
data$CoMonthlyRent [is.na(data$CoMonthlyRent)] <- 0.0
data$LTV[is.na(data$LTV)] <- 1.000
data$DTI[is.na(data.test$DTI)] <- median(data$DTI)
data$Loanterm[is.na(data$Loanterm)] <- median(data$Loanterm)
data$EstimatedMonthlyPayment[is.na(data$EstimatedMonthlyPayment)] <- median(data$EstimatedMonthlyPayment)
data$NumberOfOpenRevolvingAccounts[is.na(data$NumberOfOpenRevolvingAccounts)] <- median(data$NumberOfOpenRevolvingAccounts)
################## ADD NEW FEATURES #######################
# Create isPaymentDeficit.
data$CashFlow <- data$TotalMonthlyIncome - data$TotalMonthlyDebtBeforeLoan
data$isPaymentDeficit <- data$AmountRequested
data$isPaymentDeficit[data$EstimatedMonthlyPayment <= data$CashFlow] <- 0
data$isPaymentDeficit[data$EstimatedMonthlyPayment > data$CashFlow] <- 1
data$isPaymentDeficit <- as.factor(as.character(data$isPaymentDeficit))
# Create DownToAmountRequested
data$DownToAmountRequested <- data$DownPayment / data$AmountRequested
#######################  DELETE NON-RELEVENT COLUMNS #############################
data <- data[,!(names(data) %in% c("AppReceiveDate", "LoanNumber"))]
# Remove correlated features
data <- data[, !(names(data) %in% c("PrimeMonthlyLiability","CoMonthlyLiability",
                                                   "PrimeMonthlyRent", "CoMonthlyRent",
                                                   "CototalMonthlyIncome", "PrimeMonthlyIncome", "VehicleMake"))]
data <- data[, !(names(data) %in% c('TotalMonthlyExpenses', 'CashFlow'))]


#######delete#################
set.seed(123)
smp_size <- floor(0.95 * nrow(data))
train <- sample(seq_len(nrow(data)), size = smp_size)
test <- (-train)

data.test <- data[-train,]
data.test <- data.test[,!(names(data.test) %in% c('LoanStatus'))]

data <- data[train,]

#######

########################### TRAIN MODEL ##########################################
final.cfe <- gbm(data$LoanStatus~., data=data, distribution = 'bernoulli', n.trees = 7000, interaction.depth = 5, shrinkage = 0.01)
summary(final.cfe)


######################## CLEAN THE TEST DATA#############################
# Run following code to load and clean your test data like we did on the training data, before prediction
######################## CHANGE THE NAME TO YOUR TEST DATA FILE NAME #######################
data.test <- read.csv('your test data name')

data.test$LoanStatus <- unclass(data.test$LoanStatus)
data.test$LoanStatus <- ifelse(data.test$LoanStatus==1, 1, 0)
data.test$CoMonthlyLiability <- as.numeric(as.character(data.test$CoMonthlyLiability))
data.test$PrimeMonthlyLiability <- as.numeric(as.character(data.test$PrimeMonthlyLiability))
data.test$PrimeMonthlyRent <- as.numeric(as.character(data.test$PrimeMonthlyRent))
data.test$CoMonthlyRent <- as.numeric(as.character(data.test$CoMonthlyRent))
data.test$LTV <- as.numeric(as.character(data.test$LTV))
data.test$DTI <- as.numeric(as.character(data.test$DTI))
data.test$Loanterm<- as.numeric(as.character(data.test$Loanterm))
data.test$EstimatedMonthlyPayment<- as.numeric(as.character(data.test$EstimatedMonthlyPayment))
data.test$NumberOfOpenRevolvingAccounts<- as.numeric(as.character(data.test$NumberOfOpenRevolvingAccounts))
data.test$CoMonthlyLiability [is.na(data.test$CoMonthlyLiability )] <- 0.0
data.test$CoMonthlyRent [is.na(data.test$CoMonthlyRent)] <- 0.0
data.test$LTV[is.na(data.test$LTV)] <- 1.000
data.test$DTI[is.na(data.test$DTI)] <- median(data.test$DTI)
data.test$Loanterm[is.na(data.test$Loanterm)] <- median(data.test$Loanterm)
data.test$EstimatedMonthlyPayment[is.na(data.test$EstimatedMonthlyPayment)] <- median(data.test$EstimatedMonthlyPayment)
data.test$NumberOfOpenRevolvingAccounts[is.na(data.test$NumberOfOpenRevolvingAccounts)] <- median(data.test$NumberOfOpenRevolvingAccounts)
################## ADD NEW FEATURES #######################
# Create isPaymentDeficit.
data.test$CashFlow <- data.test$TotalMonthlyIncome - data.test$TotalMonthlyDebtBeforeLoan
data.test$isPaymentDeficit <- data.test$AmountRequested
data.test$isPaymentDeficit[data.test$EstimatedMonthlyPayment <= data.test$CashFlow] <- 0
data.test$isPaymentDeficit[data.test$EstimatedMonthlyPayment > data.test$CashFlow] <- 1
data.test$isPaymentDeficit <- as.factor(as.character(data.test$isPaymentDeficit))
# Create DownToAmountRequested
data.test$DownToAmountRequested <- data.test$DownPayment / data.test$AmountRequested
#######################  DELETE NON-RELEVENT COLUMNS #############################
data.test <- data.test[,!(names(data.test) %in% c("AppReceiveDate", "LoanNumber"))]
# Remove correlated features
data.test <- data.test[, !(names(data.test) %in% c("PrimeMonthlyLiability","CoMonthlyLiability",
                                    "PrimeMonthlyRent", "CoMonthlyRent",
                                    "CototalMonthlyIncome", "PrimeMonthlyIncome", "VehicleMake"))]
data.test <- data.test[, !(names(data.test) %in% c('TotalMonthlyExpenses', 'CashFlow'))]


####################### PREDICTION ON CLEANED TEST DATA##################
predicted.boost <- predict(final.cfe, newdata = data.test, n.trees = 7000, type="response")
predicted.boost <- ifelse(predicted.boost >= 0.5, 1, 0)
table(predicted.boost, data.test$LoanStatus)
mean(predicted.boost == data.test$LoanStatus)

# Different threshold 70%, to get lower false positive rate
predicted.boost.1 <- predict(final.cfe, newdata = data.test, n.trees = 7000, type="response")
predicted.boost.1 <- ifelse(predicted.boost.1 >= 0.7, 1, 0)
table(predicted.boost.1, data.test$LoanStatus)
mean(predicted.boost.1 == data.test$LoanStatus)

####################### END OF FINAL MODEL #######################################
#################################################################################


###################### NN: NeuralNet #########################

setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/data"))
data <- read.csv('train.csv')
data$LoanStatus <- unclass(data$LoanStatus)
data$LoanStatus <- ifelse(data$LoanStatus==1, 1, 0)

data$CoMonthlyLiability <- as.numeric(as.character(data$CoMonthlyLiability))
data$PrimeMonthlyLiability <- as.numeric(as.character(data$PrimeMonthlyLiability))
data$PrimeMonthlyRent <- as.numeric(as.character(data$PrimeMonthlyRent))
data$CoMonthlyRent <- as.numeric(as.character(data$CoMonthlyRent))
data$LTV <- as.numeric(as.character(data$LTV))
data$DTI <- as.numeric(as.character(data$DTI))
data$Loanterm<- as.numeric(as.character(data$Loanterm))
data$EstimatedMonthlyPayment<- as.numeric(as.character(data$EstimatedMonthlyPayment))
data$NumberOfOpenRevolvingAccounts<- as.numeric(as.character(data$NumberOfOpenRevolvingAccounts))

data$CoMonthlyLiability [is.na(data$CoMonthlyLiability )] <- 0.0
data$CoMonthlyRent [is.na(data$CoMonthlyRent)] <- 0.0
data$LTV[is.na(data$LTV)] <- 1.000
data$DTI[is.na(data$DTI)] <- 0.243
data$Loanterm[is.na(data$Loanterm)] <- 72.00
data$EstimatedMonthlyPayment[is.na(data$EstimatedMonthlyPayment)] <- 345.83
data$NumberOfOpenRevolvingAccounts[is.na(data$NumberOfOpenRevolvingAccounts)] <- 3.00

data <- data[,!(names(data) %in% c("AppReceiveDate", "LoanNumber"))]

# Remove correlated features
data <- data[, !(names(data) %in% c("PrimeMonthlyLiability","CoMonthlyLiability",
                                    "PrimeMonthlyRent", "CoMonthlyRent",
                                    "CototalMonthlyIncome", "PrimeMonthlyIncome", "VehicleMake"))]
data <- data[, !(names(data) %in% c('TotalMonthlyExpenses', 'CashFlow'))]


# remove outliers
library(scales)
db <- data
numColOnly <- sapply(db, class)!="factor"
db[numColOnly] <- sapply(db[numColOnly], function(x) squish(x, quantile(x, c(0, 0.95))))
db$CoPrevEmployedMonths <- data$CoPrevEmployedMonths #tricy one

# Here provides toy data option for faster training
set.seed(2)
nnData <- db
nnData <- nnData[sample(nrow(nnData), 1000), ] # select 1000 applicants randomly

# normalization
normalize <- function(x) { return ((x - min(x,na.rm=T)) / (max(x,na.rm=T) - min(x,na.rm=T))) }
numColOnly <- sapply(nnData, class)!="factor"
nnData[, numColOnly] <- sapply(nnData[, numColOnly], normalize)

#Convert categorical features to dummy variables
col_list <- paste(c(colnames(nnData[,-1])),collapse="+")
f <- formula(paste0("~LoanStatus+",col_list))
nnData <- data.frame(model.matrix(f, data = nnData)[,-1])


#Cross validation
library(caret)
set.seed(2)
trainIndex <- createDataPartition(nnData$LoanStatus, p=0.8, list = FALSE)
training_DB = nnData[trainIndex,]
testing_DB = nnData[-trainIndex,]

# start training model
library(neuralnet)
col_list <- paste(c(colnames(training_DB[,-1])),collapse="+")
f <- formula(paste0("LoanStatus~",col_list))
nn <- neuralnet(f, data = training_DB, hidden=c(5), err.fct = "sse",
                lifesign="minimum", linear.output=F, stepmax=1e6, rep=2) 

plot(nn, show.weights = F, fontsize = 10) #plot nn

NNoutput <- compute(nn, testing_DB[,-1])
pred <- ifelse(NNoutput$net.result > 0.5, 1, 0)
nnCM <- table(pred, data.matrix(testing_DB[,1])) #confusion matrix
print('Confusion matrix for NN:')
print(nnCM)
NNaccuracy <- sum(diag(nnCM))/sum(nnCM) #accuracy
cat('Accuracy for NN: ', NNaccuracy*100, "%")