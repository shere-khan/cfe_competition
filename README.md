# Final submission:

### R solution code:
- code use instructions(in report)
- Migrate everything in one file 
- Testing
	
### Report:
Note: Some tasks might involve more people to collaborate together, so no strict roles on the labeled names.

- Exploratory Data Analysis(including plots)
    - Data validation
        - Numerical features
            - histograms App/Den
            - imputation
            - Correlation
        - Categorical features
        - Test Data
- Feature Engineering
    - Overview of created features and their potential contributions
    - Revisit Correlation
- Algorithms implementation
    - Random Forest
    - Boosting
    - Lasso (Logistic Reg / Feature Selection)
    - Decision Tree
    - NN
- Conclusion

### Presentation slides

----------------------------

### Update:
- created a filted data set('filted_train.csv'), which exclude some features such as Comonthlyincome, Comonthlyrent(as there is feature Totalyincome include whole income of applicant and coapplicant if any). LTD and DTI were impute with their median value instead of mean, as there are some outstanding outliers. At end outliers were also removed.
- RF was applied and 86% accuracy is achieved
- Boosting was implemented and accuracy was improved to 88%
- Change data types to numeric/factor
- Create Training/Test data
- Plot
    - Filter outliers
    - Impute missing data
- Feature Selection
    - forward/backward selection
    - log
    - kernel
    - remove correlated features
    - dim reduction
        - PCA
        - PCR
    - lasso regression
    - random forest


### Report
1. Find key numeric features that have relationships with response (LoanStatus) -- J
2. Find other numeric features that are correlated to the key numeric features -- J (I already found these with the correlation table)
3. Find key categorical features that have relationships with response (LoanStatus) -- Y
4. Find other categorical features that are correlated to the key categorical features --Y
5. include correlation table output  -- J
6. include polyserial correlation output -- J
7. lasso -- Y

```
Columns With Missing Data:
OccupancyStatus (cat)
RequestType (cat)
Loanterm
EstimatedMonthlyPayment
NumberOfOpenRevolvingAccounts
DTI

Missing Values:
CoMontlyLiability 80689 NULL
CoMonthlyRent 80689 NULL
Loanterm NA 27
EstimatedMonthlyPayment NA 198
NumberofOpenRevolvingAccounts NA 198
RequestType (cat) NA 198


Numerical

 ModifiedCreditScore
 ModifiedBankruptcyScore"
 "EmployedMonths
 "PrevEmployedMonths"
 "CoEmployedMonths
 "CoPrevEmployedMonths
 PrimeMonthlyIncome"
 "CototalMonthlyIncome
 "TotalMonthlyIncome
 PrimeMonthlyLiability"
 "CoMonthlyLiability
 "PrimeMonthlyRent
 CoMonthlyRent"
 "TotalMonthlyDebtBeforeLoan
 VehicleYear
 "VehicleMileage
 TotalVehicleValue"
 "AmountRequested
 DownPayment
 Loanterm"
 OccupancyDuration
 EstimatedMonthlyPayment"
 "NumberOfOpenRevolvingAccounts
 LTV
 RequestType
 "DTI

Categorical
 "Source"
 "EmploymentStatus
 "MemberIndicator
 CoApplicantIndicator"
 "OccupancyStatus
 isNewVehicle
 "VehicleMake"


Notes on potential feature engineering:
 TotalMonthlyIncome/CoTotalMonthlyIncome
 LTV: Create a new feature VehicleValue/AmountRequested
 DTI: TotalMonthlyDebtBeforeLoan / TotalMonthlyIncome
 TotalMonthlyLiability: MonthlyLiability + CoMonthlyLiability
 TotalMonthlyRent: MonthlyRent + CoMonthlyRent

new feature that says whether there is an error (missing not at random) in the row

numerical prices. 2.49 fractional part 0.49

LoanTerm to employed months. Will the applicant be likely to switch jobs before the loan term is up? Does this have an effect?
AmountRequested/DownPayment


Correlation
    TotalMonthlyExpenses/PrimeMonthlyIncome/PrimeMonthlyliability 0.7/0.9
    TotalMonthlyIncome/Prime-CoMonthly Income/CashFlow 0.7/0.6/.98
    PrimeMonthlyLiability/TotalMonthlyDebtBeforeLoan  0.95
    TotalMonthlyLIabilities/TotalMonthlyDebtBeforeLoan  0.95


0.8843983 
[1] "LoanStatus"                    "Source"                        "ModifiedCreditScore"          
 [4] "ModifiedBankruptcyScore"       "EmploymentStatus"              "EmployedMonths"               
 [7] "PrevEmployedMonths"            "CoEmployedMonths"              "CoPrevEmployedMonths"         
[10] "TotalMonthlyIncome"            "TotalMonthlyDebtBeforeLoan"    "VehicleYear"                  
[13] "VehicleMileage"                "isNewVehicle"                  "TotalVehicleValue"            
[16] "AmountRequested"               "DownPayment"                   "Loanterm"                     
[19] "OccupancyStatus"               "OccupancyDuration"             "EstimatedMonthlyPayment"      
[22] "NumberOfOpenRevolvingAccounts" "LTV"                           "RequestType"                  
[25] "DTI"                           "MemberIndicator"               "CoApplicantIndicator"         
[28] "TotalMonthlyLiabilites"        "TotalMonthlyRent"              "isPaymentDeficit" 
-------------------------------
0.8834881
 [1] "LoanStatus"                    "Source"                       
 [3] "ModifiedCreditScore"           "ModifiedBankruptcyScore"      
 [5] "EmploymentStatus"              "EmployedMonths"               
 [7] "PrevEmployedMonths"            "CoEmployedMonths"             
 [9] "CoPrevEmployedMonths"          "TotalMonthlyIncome"           
[11] "TotalMonthlyDebtBeforeLoan"    "VehicleYear"                  
[13] "VehicleMileage"                "isNewVehicle"                 
[15] "TotalVehicleValue"             "AmountRequested"              
[17] "DownPayment"                   "Loanterm"                     
[19] "OccupancyStatus"               "OccupancyDuration"            
[21] "EstimatedMonthlyPayment"       "NumberOfOpenRevolvingAccounts"
[23] "LTV"                           "RequestType"                  
[25] "DTI"                           "MemberIndicator"              
[27] "CoApplicantIndicator"          "TotalMonthlyLiabilites"       
[29] "TotalMonthlyRent"              "TotalMonthlyExpenses"         
[31] "CashFlow"                      "isPaymentDeficit"   
------------------
0.8842163
 [1] "LoanStatus"                    "Source"                        "ModifiedCreditScore"          
 [4] "ModifiedBankruptcyScore"       "EmploymentStatus"              "EmployedMonths"               
 [7] "PrevEmployedMonths"            "CoEmployedMonths"              "CoPrevEmployedMonths"         
[10] "TotalMonthlyIncome"            "TotalMonthlyDebtBeforeLoan"    "VehicleYear"                  
[13] "VehicleMileage"                "isNewVehicle"                  "TotalVehicleValue"            
[16] "AmountRequested"               "DownPayment"                   "Loanterm"                     
[19] "OccupancyStatus"               "OccupancyDuration"             "EstimatedMonthlyPayment"      
[22] "NumberOfOpenRevolvingAccounts" "LTV"                           "RequestType"                  
[25] "DTI"                           "MemberIndicator"               "CoApplicantIndicator"         
[28] "TotalMonthlyLiabilites"        "TotalMonthlyRent"              "isPaymentDeficit"             
[31] "DownToAmountRequested"

-----------------------------------------------
0.8843983 - isPotentiallyUnstableAdded
[1] "LoanStatus"                    "Source"                        "ModifiedCreditScore"
 [4] "ModifiedBankruptcyScore"       "EmploymentStatus"              "EmployedMonths"
 [7] "PrevEmployedMonths"            "CoEmployedMonths"              "CoPrevEmployedMonths"
[10] "TotalMonthlyIncome"            "TotalMonthlyDebtBeforeLoan"    "VehicleYear"
[13] "VehicleMileage"                "isNewVehicle"                  "TotalVehicleValue"
[16] "AmountRequested"               "DownPayment"                   "Loanterm"
[19] "OccupancyStatus"               "OccupancyDuration"             "EstimatedMonthlyPayment"
[22] "NumberOfOpenRevolvingAccounts" "LTV"                           "RequestType"
[25] "DTI"                           "MemberIndicator"               "CoApplicantIndicator"
[28] "TotalMonthlyLiabilites"        "TotalMonthlyRent"              "isPaymentDeficit"
[31] "isPotentiallyUnstable"
-----------------------------------------------
[1] 0.8854906
> names(data)
 [1] "LoanStatus"                    "Source"                        "ModifiedCreditScore"
 [4] "ModifiedBankruptcyScore"       "EmploymentStatus"              "EmployedMonths"
 [7] "PrevEmployedMonths"            "CoEmployedMonths"              "CoPrevEmployedMonths"
[10] "TotalMonthlyIncome"            "TotalMonthlyDebtBeforeLoan"    "VehicleYear"
[13] "VehicleMileage"                "isNewVehicle"                  "TotalVehicleValue"
[16] "AmountRequested"               "DownPayment"                   "Loanterm"
[19] "OccupancyStatus"               "OccupancyDuration"             "EstimatedMonthlyPayment"
[22] "NumberOfOpenRevolvingAccounts" "LTV"                           "RequestType"
[25] "DTI"                           "MemberIndicator"               "CoApplicantIndicator"
[28] "TotalMonthlyLiabilites"        "TotalMonthlyRent"              "isPaymentDeficit"
[31] "PaidOffRatio"                  "isPotentiallyUnstable"
-------------------------------------------------
> table(predicted.boost, test$LoanStatus)
predicted.boost    0    1
              0 2653  298
              1  332 2210
> mean(predicted.boost == test$LoanStatus)  #accuracy 88%
[1] 0.8853086
> names(data)
 [1] "LoanStatus"                    "Source"                        "ModifiedCreditScore"
 [4] "ModifiedBankruptcyScore"       "EmploymentStatus"              "EmployedMonths"
 [7] "PrevEmployedMonths"            "CoEmployedMonths"              "CoPrevEmployedMonths"
[10] "TotalMonthlyIncome"            "TotalMonthlyDebtBeforeLoan"    "VehicleYear"
[13] "VehicleMileage"                "isNewVehicle"                  "TotalVehicleValue"
[16] "AmountRequested"               "DownPayment"                   "Loanterm"
[19] "OccupancyStatus"               "OccupancyDuration"             "EstimatedMonthlyPayment"
[22] "NumberOfOpenRevolvingAccounts" "LTV"                           "RequestType"
[25] "DTI"                           "MemberIndicator"               "CoApplicantIndicator"
[28] "TotalMonthlyLiabilites"        "TotalMonthlyRent"              "isPaymentDeficit"
[31] "PaidOffRatio"                  "isPotentiallyUnstable"
-----------------------------------------
> boost.cfe.1 <- gbm(train$LoanStatus~., data=train, distribution = 'bernoulli',
+                    n.trees = 5000, interaction.depth = 3)
predicted.boost    0    1
              0 2627  287
              1  338 2239
> mean(predicted.boost == test$LoanStatus)
[1] 0.8861774

 [1] "LoanStatus"                    "Source"                        "ModifiedCreditScore"          
 [4] "ModifiedBankruptcyScore"       "EmploymentStatus"              "EmployedMonths"               
 [7] "PrevEmployedMonths"            "CoEmployedMonths"              "CoPrevEmployedMonths"         
[10] "TotalMonthlyIncome"            "TotalMonthlyDebtBeforeLoan"    "VehicleYear"                  
[13] "VehicleMileage"                "isNewVehicle"                  "TotalVehicleValue"            
[16] "AmountRequested"               "DownPayment"                   "Loanterm"                     
[19] "OccupancyStatus"               "OccupancyDuration"             "EstimatedMonthlyPayment"      
[22] "NumberOfOpenRevolvingAccounts" "LTV"                           "RequestType"                  
[25] "DTI"                           "MemberIndicator"               "CoApplicantIndicator"         
[28] "TotalMonthlyLiabilities"       "TotalMonthlyRent"              "isPaymentDeficit"             
[31] "DownToAmountRequested" 


Boosting [1] 0.8785727 without imputation
> summary(boost.cfe.1)
                                                        var     rel.inf
ModifiedCreditScore                     ModifiedCreditScore 51.73876506
DTI                                                     DTI 12.27256064
EstimatedMonthlyPayment             EstimatedMonthlyPayment  4.86844852
LTV                                                     LTV  4.33912543
AmountRequested                             AmountRequested  4.13999749
VehicleMileage                               VehicleMileage  3.51582559
MemberIndicator                             MemberIndicator  2.07319786
TotalMonthlyIncome                       TotalMonthlyIncome  2.05314216
RequestType                                     RequestType  1.97109220
Loanterm                                           Loanterm  1.68711350
OccupancyStatus                             OccupancyStatus  1.37168388
ModifiedBankruptcyScore             ModifiedBankruptcyScore  1.36145719
NumberOfOpenRevolvingAccounts NumberOfOpenRevolvingAccounts  1.19589740
TotalMonthlyDebtBeforeLoan       TotalMonthlyDebtBeforeLoan  1.16001783
CoEmployedMonths                           CoEmployedMonths  1.12432237
TotalVehicleValue                         TotalVehicleValue  1.09130985
EmployedMonths                               EmployedMonths  1.01978364
VehicleYear                                     VehicleYear  0.86604340
OccupancyDuration                         OccupancyDuration  0.63304419
Source                                               Source  0.36117694
DownPayment                                     DownPayment  0.33399435
CoPrevEmployedMonths                   CoPrevEmployedMonths  0.27136426
PrevEmployedMonths                       PrevEmployedMonths  0.24763699
CoApplicantIndicator                   CoApplicantIndicator  0.15922157
EmploymentStatus                           EmploymentStatus  0.12880758
isNewVehicle                                   isNewVehicle  0.01497012
```

