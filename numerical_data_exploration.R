library(dplyr)
library(polycor)
library(pracma)

setwd("/home/justin/pycharmprojects/cfe_competition_2018/data")
train <- read.csv('train.csv')
attach(train)

# Change all number columns to numeric
train$CoMonthlyRent <- as.numeric(CoMonthlyRent)
train$ModifiedCreditScore<- as.numeric(ModifiedCreditScore)
train$ModifiedBankruptcyScore <- as.numeric(ModifiedBankruptcyScore)
train$EmployedMonths <- as.numeric(EmployedMonths)
train$PrevEmployedMonths <- as.numeric(PrevEmployedMonths)
train$CoEmployedMonths <- as.numeric(CoEmployedMonths)
train$PrimeMonthlyIncome <- as.numeric(PrimeMonthlyIncome)
train$PrimeMonthlyRent <- as.numeric(PrimeMonthlyRent)
train$CoMonthlyRent <- as.numeric(CoMonthlyRent)
train$TotalMonthlyDebtBeforeLoan <- as.numeric(TotalMonthlyDebtBeforeLoan)
train$VehicleYear <- as.numeric(VehicleYear)
train$VehicleMileage <- as.numeric(VehicleMileage)
train$TotalVehicleValue <- as.numeric(TotalVehicleValue)
train$AmountRequested <- as.numeric(AmountRequested)
train$DownPayment <- as.numeric(DownPayment)
train$Loanterm <- as.numeric(Loanterm)
train$OccupancyDuration <- as.numeric(OccupancyDuration)
train$EstimatedMonthlyPayment <- as.numeric(EstimatedMonthlyPayment)
train$NumberOfOpenRevolvingAccounts <- as.numeric(NumberOfOpenRevolvingAccounts)
train$LTV <- as.numeric(LTV)

# Change categrical columns to factor type
train$VehicleMake <- as.factor(VehicleMake)
train$EmploymentStatus <- as.factor(EmploymentStatus)
train$isNewVehicle <- as.factor(train$isNewVehicle)
train$OccupancyStatus <- as.factor(OccupancyStatus)
train$LoanStatus <- as.factor(LoanStatus)

setwd("/home/justin/pycharmprojects/cfe_competition_2018/images")
pdf("boxplot_modified_credit_score")
boxplot(train$LoanStatus, train$ModifiedCreditScore, xlab="LoanStatus", ylab="ModifiedCreditScore")
dev.off()
pdf("boxplot_modified_bankruptcy_score")
boxplot(train$LoanStatus, train$ModifiedBankruptcyScore, xlab="LoanStatus", ylab="ModifiedBankruptcyScore")
dev.off()

setwd("/home/justin/pycharmprojects/cfe_competition_2018")
sink("boxplot_modified_credit_score.txt", append=FALSE, split=FALSE)
summary(train[,"ModifiedCreditScore"])
sink("boxplot_modified_bankruptcy_score.txt", append=FALSE, split=FALSE)
summary(train[,"ModifiedBankruptcyScore"])
sink()

# Take sample of size 200 from train
train <- train[sample(nrow(train), 1000), ]

# split training data into numrical columns only
num_cols_only <- select(train, ModifiedCreditScore, ModifiedBankruptcyScore, EmployedMonths, PrevEmployedMonths, CoEmployedMonths, PrimeMonthlyIncome, CototalMonthlyIncome, TotalMonthlyIncome, PrimeMonthlyIncome, PrimeMonthlyLiability, PrimeMonthlyRent, TotalMonthlyDebtBeforeLoan, VehicleYear, VehicleMileage, TotalVehicleValue, AmountRequested, DownPayment, Loanterm, OccupancyDuration, EstimatedMonthlyPayment, NumberOfOpenRevolvingAccounts, LTV, DTI)

# reduce training data even more for testing
reduced_set <- select(train, EstimatedMonthlyPayment, NumberOfOpenRevolvingAccounts, LTV)

# Change values of num_cols_only to numeric.
# Shouldn't have to do this since it was done for train already
num_cols_only$CoMonthlyRent <- as.numeric(CoMonthlyRent)
num_cols_only$ModifiedCreditScore<- as.numeric(ModifiedCreditScore)
num_cols_only$ModifiedBankruptcyScore <- as.numeric(ModifiedBankruptcyScore)
num_cols_only$EmployedMonths <- as.numeric(EmployedMonths)
num_cols_only$PrevEmployedMonths <- as.numeric(PrevEmployedMonths)
num_cols_only$CoEmployedMonths <- as.numeric(CoEmployedMonths)
num_cols_only$PrimeMonthlyIncome <- as.numeric(PrimeMonthlyIncome)
num_cols_only$PrimeMonthlyRent <- as.numeric(PrimeMonthlyRent)
num_cols_only$CoMonthlyRent <- as.numeric(CoMonthlyRent)
num_cols_only$TotalMonthlyDebtBeforeLoan <- as.numeric(TotalMonthlyDebtBeforeLoan)
num_cols_only$VehicleYear <- as.numeric(VehicleYear)
num_cols_only$VehicleMileage <- as.numeric(VehicleMileage)
num_cols_only$TotalVehicleValue <- as.numeric(TotalVehicleValue)
num_cols_only$AmountRequested <- as.numeric(AmountRequested)
num_cols_only$DownPayment <- as.numeric(DownPayment)
num_cols_only$Loanterm <- as.numeric(Loanterm)
num_cols_only$OccupancyDuration <- as.numeric(OccupancyDuration)
num_cols_only$EstimatedMonthlyPayment <- as.numeric(EstimatedMonthlyPayment)
num_cols_only$NumberOfOpenRevolvingAccounts <- as.numeric(NumberOfOpenRevolvingAccounts)

# Calculate an inferred realtionship between each numerical predictor and LoanStatus.
sink("polyserial.txt", append=FALSE, split=FALSE)
print("CoMonthlyRent")
polyserial(train[, "CoMonthlyRent"], train[, "LoanStatus"])
print("ModifiedCreditScore")
polyserial(train[, "ModifiedCreditScore"], train[, "LoanStatus"])
print("ModifiedBankruptcyScore")
polyserial(train[, "ModifiedBankruptcyScore"], train[, "LoanStatus"])
print("EmployedMonths")
polyserial(train[, "EmployedMonths"], train[, "LoanStatus"])
print("PrevEmployedMonths")
polyserial(train[, "PrevEmployedMonths"], train[, "LoanStatus"])
print("CoEmployedMonths")
polyserial(train[, "CoEmployedMonths"], train[, "LoanStatus"])
print("PrimeMonthlyIncome")
polyserial(train[, "PrimeMonthlyIncome"], train[, "LoanStatus"])
print("PrimeMonthlyRent")
polyserial(train[, "PrimeMonthlyRent"], train[, "LoanStatus"])
print("CoMonthlyRent")
polyserial(train[, "CoMonthlyRent"], train[, "LoanStatus"])
print("TotalMonthlyDebtBeforeLoan")
polyserial(train[, "TotalMonthlyDebtBeforeLoan"], train[, "LoanStatus"])
print("VehicleYear")
polyserial(train[, "VehicleYear"], train[, "LoanStatus"])
print("VehicleMileage")
polyserial(train[, "VehicleMileage"], train[, "LoanStatus"])
print("TotalVehicleValue")
polyserial(train[, "TotalVehicleValue"], train[, "LoanStatus"])
print("AmountRequested")
polyserial(train[, "AmountRequested"], train[, "LoanStatus"])
print("DownPayment")
polyserial(train[, "DownPayment"], train[, "LoanStatus"])
print("Loanterm")
polyserial(train[, "Loanterm"], train[, "LoanStatus"])
print("OccupancyDuration")
polyserial(train[, "OccupancyDuration"], train[, "LoanStatus"])
print("EstimatedMonthlyPayment")
polyserial(train[, "EstimatedMonthlyPayment"], train[, "LoanStatus"])
print("NumberOfOpenRevolvingAccounts")
polyserial(train[, "NumberOfOpenRevolvingAccounts"], train[, "LoanStatus"])

# Sample data to reduce size
reduced_set <- reduced_set[sample(nrow(reduced_set), 300), ]
num_cols_only <- num_cols_only[sample(nrow(num_cols_only ), 200), ]


# TEST: Loop to plot all columns against each other in separate plots
for (m in names(reduced_set)) {
  for (n in names(reduced_set)) {
    if(!strcmp(m, n)) {
      x <- reduced_set[, m]
      y <- reduced_set[, n]
      plot(x, y, xlab=m, ylab=n)
    }
  }
}

# Loop to plot all columns against each other in separate plots
for (m in names(num_cols_only)) {
  for (n in names(num_cols_only)) {
    if(!strcmp(m, n)) {
      title <- paste(m, n, sep="_v_")
      title_ext <- paste(title, ".pdf", sep="")
      pdf(title_ext)
      x <- num_cols_only[, m]
      y <- num_cols_only[, n]
      plot(x, y, xlab=m, ylab=n)
      dev.off()
    }
  }
}

# poly serial of each column against LoanStatus
setwd("/home/justin/pycharmprojects/cfe_competition_2018")
sink("polyserial.txt", append=FALSE, split=FALSE)
for (m in names(num_cols_only)) {
  print(m)
}
for (i in 1:ncol(num_cols_only)) {
  polyserial(num_cols_only[, i], train[,"LoanStatus"])
}
sink()

# num_cols_only$LTV <- as.numeric(LTV)
sink("correlation.txt", append=FALSE, split=FALSE)
cor(num_cols_only)
sink()
