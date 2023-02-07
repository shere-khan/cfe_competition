setwd("/Users/yuan/Desktop/2018CFE/")
#library(tidyverse)
train <- read.csv('DataSet_UCF Dataset 2018 - Training set.csv')
dim(train)
names(train)
attach(train)
test <- read.csv('DataSet_UCF Dataset 2018 - Testing set.csv')
dim(test)

train$LoanStatus <- as.factor(LoanStatus)
# data description
# skip variables which seems important: LoanNumber, AppReceivedData

#categorical variables

#Source, 'CONSUMER' has higher chance to get declined
train$Source <- as.factor(Source)#need to use 'train$', otherwise no update of var type
table(Source,LoanStatus)
prop.table(table(Source,LoanStatus))
par(col.lab="red")
#compare LoanStatus for each variables
plot(LoanStatus~Source,col=terrain.colors(3))
#put two plots side by side
par(mfrow=c(1,2))
plot(Source[which(LoanStatus=='Approved')],col=terrain.colors(3),las=2)
plot(Source[which(LoanStatus=='Declined')],col=terrain.colors(3),las=2) 
#reset par
par(mfrow=c(1,1))


#EmploymentStatus, some effects
train$EmploymentStatus <- as.factor(EmploymentStatus)
table(EmploymentStatus,LoanStatus)
prop.table(table(EmploymentStatus,LoanStatus))
plot(LoanStatus~EmploymentStatus,col=terrain.colors(3))
#side by side plots
par(mfrow=c(1,2))
plot(EmploymentStatus[which(LoanStatus=='Approved')],col=terrain.colors(3),las=2)
plot(EmploymentStatus[which(LoanStatus=='Declined')],col=terrain.colors(3),las=2)
par(mfrow=c(1,1))


#VehicleMake
train$VehicleMake <- as.factor(VehicleMake)
#from the table we can see, the Mark which are not so popular(less than 10,20 
#applications) and not luxury were tend to be declined.
table(VehicleMake,LoanStatus)
plot(LoanStatus~VehicleMake,col=terrain.colors(3))


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


#CoApplicantIndicator, N has slightly higher Declined rate, but it seems trivial
train$CoApplicantIndicator <- as.factor(CoApplicantIndicator)
table(CoApplicantIndicator,LoanStatus)
plot(LoanStatus~CoApplicantIndicator,col=terrain.colors(3))

#CoMonthlyLiability, when equal to 'NULL', has little higher change to fail
plot(LoanStatus[which(CoMonthlyLiability=='0')]~CoMonthlyLiability[which(CoMonthlyLiability=='0')],col=terrain.colors(3),las=2)
plot(LoanStatus[which(CoMonthlyLiability=='NULL')]~CoMonthlyLiability[which(CoMonthlyLiability=='NULL')],col=terrain.colors(3),las=2)

#CoMonthlyRent, when equal to 'NULL', has little higher change to fail
plot(LoanStatus[which(CoMonthlyRent=='NULL')]~CoMonthlyRent[which(CoMonthlyRent=='NULL')],col=terrain.colors(3),las=2)
plot(LoanStatus[which(CoMonthlyRent=='0')]~CoMonthlyRent[which(CoMonthlyRent=='0')],col=terrain.colors(3),las=2)


