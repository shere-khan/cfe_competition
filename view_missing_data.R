# Title     : TODO
# Objective : TODO
# Created by: justin
# Created on: 1/20/19

setwd(paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/data"))
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

library(VIM)
library(dplyr)
data.imp = select(data, LTV, DTI)

# View missing data
mice_plot <- aggr(data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

################# IMPUTATION #################
data$CoMonthlyLiability [is.na(data$CoMonthlyLiability )] <- 0.0
data$CoMonthlyRent [is.na(data$CoMonthlyRent)] <- 0.0
data$LTV[is.na(data$LTV)] <- 1.022
data$DTI[is.na(data$DTI)] <- 0.243
data$Loanterm[is.na(data$Loanterm)] <- 72.00
data$EstimatedMonthlyPayment[is.na(data$EstimatedMonthlyPayment)] <- 345.83
data$NumberOfOpenRevolvingAccounts[is.na(data$NumberOfOpenRevolvingAccounts)] <- 3.00
