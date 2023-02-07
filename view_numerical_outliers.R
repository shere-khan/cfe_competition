# Created by: justin
# Created on: 1/20/19

# setwd("/home/justin/pycharmprojects/cfe_competition_2018/data/")
setwd("C:\\Users\\JustinBarry\\Documents\\projects\\cfe_competition\\data")
data <- read.csv('train.csv')

# Remove categorical before performing correlation
data <- data[,!(names(data) %in% c("VehicleMake", "RequestType", "OccupancyStatus",
                                   "isNewVehicle", "EmploymentStatus", "MemberIndicator",
                                   "CoApplicantIndicator", "Source", "LoanStatus", "AppReceiveDate", "LoanNumber"))]

# Ensure all columns are numeric
data$CoMonthlyLiability <- as.numeric(as.character(data$CoMonthlyLiability))
data$PrimeMonthlyLiability <- as.numeric(as.character(data$PrimeMonthlyLiability))
data$PrimeMonthlyRent <- as.numeric(as.character(data$PrimeMonthlyRent))
data$CoMonthlyRent <- as.numeric(as.character(data$CoMonthlyRent))
data$LTV <- as.numeric(as.character(data$LTV))
data$DTI <- as.numeric(as.character(data$DTI))
data$Loanterm<- as.numeric(as.character(data$Loanterm))
data$EstimatedMonthlyPayment<- as.numeric(as.character(data$EstimatedMonthlyPayment))
data$NumberOfOpenRevolvingAccounts<- as.numeric(as.character(data$NumberOfOpenRevolvingAccounts))
data$VehicleMileageAmountRequestedRatio <- as.numeric(as.character(data$VehicleMileageAmountRequestedRatio ))
data$VehicleMileageLoantermRatio <- as.numeric(as.character(data$VehicleMileageLoantermRatio ))

################# IMPUTATION #################
data$CoMonthlyLiability [is.na(data$CoMonthlyLiability )] <- 0.0
data$CoMonthlyRent [is.na(data$CoMonthlyRent)] <- 0.0
data$LTV[is.na(data$LTV)] <- 1.022
data$DTI[is.na(data$DTI)] <- 0.243
data$Loanterm[is.na(data$Loanterm)] <- 72.00
data$EstimatedMonthlyPayment[is.na(data$EstimatedMonthlyPayment)] <- 345.83
data$NumberOfOpenRevolvingAccounts[is.na(data$NumberOfOpenRevolvingAccounts)] <- 3.00

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
# outvals = boxplot(x)$data
# which(x %in% outvals)
names(data)
