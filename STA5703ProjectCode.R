setwd("your directory")
data <- read.csv('train.csv')
dim(data)
names(data)

#feature preparation
str(data)
data$LTV <- as.numeric(as.character(data$LTV))
data$DTI <- as.numeric(data$DTI)

# Select only LTV and DTI
library(VIM)
library(dplyr)
data.imp = select(data, LTV, DTI)

# View missing data
mice_plot <- aggr(data.imp, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data.imp), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# Impute missing data
library(mice)
imputed_Data <- mice(data.imp, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(data.imp$DTI)

# Replace missing values in datafram data.imp with imputed values
completeData <- complete(imputed_Data, 2)

# Drop columns from data frame
data = subset(data, select = -c(LTV, DTI))

# Combine data and completedData side-by-side
data = cbind(data, completeData)

data$CoMonthlyRent <- as.numeric(data$CoMonthlyRent)
data$CoMonthlyLiability <- as.numeric(data$CoMonthlyLiability)

#delete non-relevent columns
data <- data[,!(names(data) %in% c("AppReceiveDate", "LoanNumber"))]

#remove rows with missing value
dim(data[!complete.cases(data),])
data <- data[complete.cases(data),]

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
dim(data)

#handling multicolinearity, only for numeric features
cor(data[, sapply(data, class) != "factor"])

#some features have high(more than 0.9) colinearity, remove those features
#PrimeMonthlyLiability and CoMonthlyRent
data <- data[,-which(names(data) %in% c("PrimeMonthlyLiability","CoMonthlyRent"))]


#split into train and test data
set.seed(1)
train <- sample(1:nrow(data),nrow(data)/2)
test <- (-train)

#lasso
#create x matrix and y vector
train.x <- model.matrix(LoanStatus~.,data[train,])[,-1]
train.y <- ifelse(data[train,]$LoanStatus=='Approved',1,0)
test.x <- model.matrix(LoanStatus~.,data[test,])[,-1]
test.y <- data[test,]$LoanStatus

library(glmnet)
#cross validation and find best lamba value
set.seed(123)
cv.lasso = cv.glmnet(train.x,train.y,alpha = 1,family="binomial")
plot(cv.lasso)
(bestlam <- cv.lasso$lambda.min)
(cv.lasso$lambda.1se)
model <- glmnet(train.x, train.y, alpha = 1, family = "binomial",
                lambda = bestlam)
coef(model)

# Make predictions on the test data
probabilities <- predict(model,type = 'response',newx = test.x)
predicted.classes <- ifelse(probabilities > 0.5, "Approved", "Declined")
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


