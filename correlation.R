# Title     : TODO
# Objective : TODO
# Created by: justin
# Created on: 1/20/19

setwd("/home/justin/pycharmprojects/cfe_competition_2018/data/")
# setwd("C:\\Users\\Justin\\PycharmProjects\\cfe_competition_2018\\data")
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

################ ADD NEW FEATURES #############
# Create isPaymentDeficit
data$TotalMonthlyLiabilities <- data$PrimeMonthlyLiability + data$CoMonthlyLiability
data$TotalMonthlyRent <- data$PrimeMonthlyRent + data$CoMonthlyRent
data$TotalMonthlyExpenses <- data$TotalMonthlyLiabilities + data$TotalMonthlyRent
data$CashFlow <- data$TotalMonthlyIncome - data$TotalMonthlyExpenses
data$isPaymentDeficit <- data$AmountRequested
data$isPaymentDeficit[data$EstimatedMonthlyPayment <= data$CashFlow] <- 0
data$isPaymentDeficit[data$EstimatedMonthlyPayment > data$CashFlow] <- 1

# Brief validation and correlation of subset of data
library(dplyr)
data.sub = select(data, OccupancyDuration, EmployedMonths, ModifiedCreditScore, ModifiedBankruptcyScore, EmployedMonths, PrevEmployedMonths)
data.sub = select(data, isPaymentDeficit, LoanStatus)
                  
data.sub = select(data, OccupancyDuration, EmployedMonths, ModifiedCreditScore, ModifiedBankruptcyScore, EmployedMonths, PrevEmployedMonths,
                  CoEmployedMonths, CoPrevEmployedMonths, PrimeMonthlyIncome, CototalMonthlyIncome, TotalMonthlyIncome, PrimeMonthlyLiability,
                  CoMonthlyLiability, PrimeMonthlyRent, CoMonthlyRent, TotalMonthlyDebtBeforeLoan, VehicleYear, VehicleMileage,
                  TotalVehicleValue, AmountRequested, DownPayment, Loanterm, OccupancyDuration, EstimatedMonthlyPayment,
                  NumberOfOpenRevolvingAccounts, LTV, DTI)

# Various correlation methods on entire dataset
sink("svm_output.txt", append=TRUE, split=FALSE)
cor.matrix = cor(data)
library(corrplot)
corrplot(cor.matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45, tl.cex = 0.7)
# corrplot(cor.matrix, method="circle")

# Exploring the addtion of new features
library(dplyr)
# LTV is equal to AmountRequested/TotalVehicleValue
data.sub = select(data, LTV, AmountRequested, TotalVehicleValue)
data$LoanToValue <- data$AmountRequested / data$TotalVehicleValue
# DTI is equal to TotalMonthlyDebtBeforeLoan/TotalMonthlyIncome
data.sub = select(data, DTI, TotalMonthlyDebtBeforeLoan, TotalMonthlyIncome)
data$LoanToValue <- data$AmountRequested / data$TotalVehicleValue
# Calculate RepaymentOverflow
data$LTV[is.na(data$LTV)] <- 1.022
data$DTI[is.na(data$DTI)] <- 0.243
data$Loanterm[is.na(data$Loanterm)] <- 72.00
data$EstimatedMonthlyPayment[is.na(data$EstimatedMonthlyPayment)] <- 345.83
data$NumberOfOpenRevolvingAccounts[is.na(data$NumberOfOpenRevolvingAccounts)] <- 3.00

# Isolate NA value (EstimatedMonthlyPayment)
data$PrimeMonthlyLiability[800:811]
data$CoMonthlyLiability[800:811]
data$PrimeMonthlyRent[800:811]
data$CoMonthlyRent[800:811]
data$AmountRequested[800:811]
data$EstimatedMonthlyPayment[800:811]

# Create isPaymentDeficit
data$TotalMonthlyLiabilites <- data$PrimeMonthlyLiability + data$CoMonthlyLiability
data$TotalMonthlyRent <- data$PrimeMonthlyRent + data$CoMonthlyRent
data$TotalMonthlyExpenses <- data$TotalMonthlyLiabilites + data$TotalMonthlyRent
data$CashFlow <- data$TotalMonthlyIncome - data$TotalMonthlyExpenses
data$isPaymentDeficit <- data$AmountRequested
data$isPaymentDeficit[data$EstimatedMonthlyPayment <= data$CashFlow] <- 1
data$isPaymentDeficit[data$EstimatedMonthlyPayment > data$CashFlow] <- 0
