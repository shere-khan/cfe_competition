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

################# Create Histograms for Data Validation #################
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

grid.arrange(p9, p10, p11, p12, nrow=2)
