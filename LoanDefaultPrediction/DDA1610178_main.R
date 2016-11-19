setwd("/home/aiswarya/upgradDataAnalytics/Course3/Assignment/Graded/LogisticRegression/")

library(data.table)
library(ggplot2)
library(moments)
library(car)
#Import dataset
german_credit<-fread("german.csv",stringsAsFactors = T)
str(german_credit) #1000 observations, 21 variables
View(german_credit)

#Renaming the column names(as column names contain characters like ? and replaces space by _)
colnames(german_credit)<-c("Status_of_existing_checking_account","Duration_in_month","Credit_history","Purpose","Credit_amount","Savings_accountorbonds","Present_employment_since","Installment_rate_in_percentage_of_disposable_income","Personal_status_and_sex",
                           "Other_debtors_or_guarantors","Present_residence_since","Property","Age_in_Years","Other_installment_plans","Housing","Number_of_existing_credits_at_this_bank","Job_status","Number_of_people_being_liable_to_provide_maintanance_for","Telephone","foreign_worker","Default_status")



#Default_Status is the target varible

#----------------------UNIVARIATE ANALYSIS--------------------------------------------------

#Status of existing checking account
ggplot(german_credit,aes(x=Status_of_existing_checking_account,fill=Status_of_existing_checking_account))+geom_bar()
#A14 us the most common status. This indicates that most of the users have no checking account.

#How does default vary with Status of existing checking account?
ggplot(german_credit,aes(x=Status_of_existing_checking_account,fill=as.factor(Default_status)))+geom_bar(position = "fill")
#50% of the customers with chcking account with <0 DM are defaulters. 
#around 10% of the people with no checking account default.

#Duration in month
table(german_credit$Duration_in_month)
#this is between 4 to 72 months. Binning may be possible for this field.
ggplot(german_credit,aes(x=Duration_in_month))+geom_bar(fill="blue")
#Very few credit > 60 months.12 and 2ggplot(german_credit,aes(x=`Status of existing checking account`,fill=`Status of existing checking account`))+geom_bar()4 months have highest credit duration.
#at multiples of 12, which is an year there is jump in the plot, indicating that most of the customers, take credit for a period in multiples of an year
#Default Vary with Duration?
ggplot(german_credit,aes(x=Duration_in_month,fill=as.factor(Default_status)))+geom_bar(position="dodge")
#As duration of credit increase the default rate also increases.
#Loan taken for beyond 5 years, sure to default.

#Credit history
table(german_credit$Credit_history)
ggplot(german_credit,aes(x=Credit_history,fill=Credit_history))+geom_bar()
#Most of the customers have their existing credits paid back duly till now

#How default status changes with Credit History?
ggplot(german_credit,aes(x=Credit_history,fill=as.factor(Default_status)))+geom_bar(position = "fill")
#Surpsingly those who have delay in paying of credit in past or in critical account tend to default less

#Purpose
table(german_credit$Purpose)/nrow(german_credit)
ggplot(german_credit,aes(x=Purpose,fill=Purpose))+geom_bar()
#51% of the loans have been taken for A40(buyin new car) and A43(buyin radio/tv)
ggplot(german_credit,aes(x=Purpose,fill=as.factor(Default_status)))+geom_bar(position="fill")

#Credit Amount
boxplot(german_credit$Credit_amount) #Outliers are present
#Right skewed data,
ggplot(german_credit,aes(x=Credit_amount))+geom_histogram()
#Let see if log transformation make it normal
ggplot(german_credit,aes(x=log(Credit_amount)))+geom_histogram()
boxplot(log(german_credit$Credit_amount))
#After log tranformation, we cap ony one outlier below the lower quartile.

#How does credit amount effect Default_Status
ggplot(german_credit,aes(x=as.factor(Default_status),y=Credit_amount))+geom_boxplot()
#The range of credit amout is higher for Defaulters as compared to non defaulters

#SavingsAccounts/Bonds
ggplot(german_credit,aes(x=Savings_accountorbonds))+geom_bar()
#~60% of the saving accounts/bonds has less than 100DM.

#Default Status with Saving Accounts/Bonds?
ggplot(german_credit,aes(x=Savings_accountorbonds,fill=as.factor(Default_status)))+geom_bar(position="fill")

#Present employment since\xa0
ggplot(german_credit,aes(x=Present_employment_since))+geom_bar()
#33% if the customers have work experinece of between 1 and 4 years.
ggplot(german_credit,aes(x=Present_employment_since,fill=as.factor(Default_status)))+geom_bar(position="fill")
#A72 has the highest default rate.

#Installment_rate_in_percentage_of_disposable_income
#This is a factor variable.So COnvert into factor
german_credit$Installment_rate_in_percentage_of_disposable_income<-as.factor(german_credit$Installment_rate_in_percentage_of_disposable_income)
ggplot(german_credit,aes(x=Installment_rate_in_percentage_of_disposable_income))+geom_bar(fill="royalblue4")
#Most of the credit aare for 4% intrest rate
ggplot(german_credit,aes(x=Installment_rate_in_percentage_of_disposable_income,fill=as.factor(Default_status)))+geom_bar(position="dodge")

#Personal_status_and_sex
ggplot(german_credit,aes(x=Personal_status_and_sex))+geom_bar(fill="royalblue4")
#Male and Single (A93)=> More credits taken
#Surprisingly, No Single Females taking loans? (A95 is missing)
ggplot(german_credit,aes(x=Personal_status_and_sex,fill=as.factor(Default_status)))+geom_bar(position="fill")

#Other_debtors_or_guarantors
ggplot(german_credit,aes(x=Other_debtors_or_guarantors))+geom_bar()
#Most of them are not debtors or guarntors
ggplot(german_credit,aes(x=Other_debtors_or_guarantors,fill=as.factor(Default_status)))+geom_bar(position="fill")
#If you are a coapplicant ~50% chance of defaulting

#Present_residence_since
#Convert into factorVariable
german_credit$Present_residence_since<-as.factor(german_credit$Present_residence_since)
ggplot(german_credit,aes(x=Present_residence_since))+geom_bar()

ggplot(german_credit,aes(x=Present_residence_since,fill=as.factor(Default_status)))+geom_bar(position = "fill")
#Almost same default rate

#Property 
ggplot(german_credit,aes(x=Property))+geom_bar()
ggplot(german_credit,aes(x=Property,fill=as.factor(Default_status)))+geom_bar(position = "fill")
#No Property (A124)=> Higher default

#Age_in_Years 
ggplot(german_credit,aes(x=Age_in_Years))+geom_histogram(col="royalblue4",fill="lightblue")
#Right skewed.
skewness(german_credit$Age_in_Years)
kurtosis(german_credit$Age_in_Years)

ggplot(german_credit,aes(x=Age_in_Years,fill=as.factor(Default_status)))+geom_histogram(position="dodge")

#Other_installment_plans
ggplot(german_credit,aes(x=Other_installment_plans))+geom_bar(fill="royalblue4")
#Most of the customers do not have any installment plans
ggplot(german_credit,aes(x=Other_installment_plans,fill=as.factor(Default_status)))+geom_bar(position = "fill")
#People with no installment plans have lower default rates.

#Housing
ggplot(german_credit,aes(x=Housing))+geom_bar(fill="royalblue4")
#Most of the customers who take credit have own houses.
ggplot(german_credit,aes(x=Housing,fill=as.factor(Default_status)))+geom_bar(position="fill")
#Own House => Lower Default Rate

#Number_of_existing_credits_at_this_bank
#Convert to factor
german_credit$Number_of_existing_credits_at_this_bank<-as.factor(german_credit$Number_of_existing_credits_at_this_bank)
ggplot(german_credit,aes(x=Number_of_existing_credits_at_this_bank))+geom_bar()
#~60% of the customers have only 1 credit
ggplot(german_credit,aes(x=Number_of_existing_credits_at_this_bank,fill=as.factor(Default_status)))+geom_bar(position="fill")
#If you have 1 or 4 exsting credit, then ypu have higher default rate

#Number_of_people_being_liable_to_provide_maintanance_for
#Convert to factor
german_credit$Number_of_people_being_liable_to_provide_maintanance_for<-as.factor(german_credit$Number_of_people_being_liable_to_provide_maintanance_for)
ggplot(german_credit,aes(x=Number_of_people_being_liable_to_provide_maintanance_for))+geom_bar()
#Most have 1 
ggplot(german_credit,aes(x=Number_of_people_being_liable_to_provide_maintanance_for,fill=as.factor(Default_status)))+geom_bar(position = "fill")
#Both grps have equal default rate.

#Telephone
ggplot(german_credit,aes(x=Telephone))+geom_bar()
#~60% of the customers dnt own a telephone
ggplot(german_credit,aes(x=Telephone,fill=as.factor(Default_status)))+geom_bar(position = "fill")
#No tellephone, slightly higher default rate

#foreign_worker
ggplot(german_credit,aes(x=foreign_worker))+geom_bar()
#>90% are foreign workers
ggplot(german_credit,aes(x=foreign_worker,fill=as.factor(Default_status)))+geom_bar(position = "fill")
#If you are not a foriegn worker, you have less default rate

#Default_status
ggplot(german_credit,aes(x=as.factor(Default_status)))+geom_bar()
table(german_credit$Default_status)
#If we predict 0 for all the values => we should get a accuracy of 70%.
#Thus, Baseline accuracy is 70%

#------------------Data Cleaning and Transformation-------------------------------------------------

#Are there any missing values?
sum(is.na(german_credit)) #No missing values

#Outlier Detection

#Credit Amount
german_credit$Credit_amount<-as.numeric(german_credit$Credit_amount)
#Lets apply log transformation on Credit_amount to get a normal distribution 
#For further analysis, we will use the log transformed value for analysi
german_credit$Credit_amount<-log(german_credit$Credit_amount)

boxplot(german_credit$Credit_amount)
quantile(german_credit$Credit_amount,probs=seq(0,1,0.01))
#Between the o% and 1% there is a 0.5 increase in the log tranformed variable of Credit Amount.
#So let us floor the value at 1%

onePercentile_value<-quantile(german_credit$Credit_amount,probs=0.01)
onePercentile_value
german_credit[Credit_amount<onePercentile_value,Credit_amount:=onePercentile_value]
boxplot(log(german_credit$Credit_amount))

#Age

quantile(german_credit$Age_in_Years,probs=seq(0,1,0.01))
quantile(german_credit$Age_in_Years,probs=seq(0.95,1,0.001))

ggplot(german_credit,aes(x=Age_in_Years,fill=as.factor(Default_status)))+geom_histogram(position = "dodge")
#Anything above 97.7%(>64 years of age ) can be capped
capValue_Age<-64
capValue_Age
german_credit[Age_in_Years>capValue_Age,Age_in_Years:=capValue_Age]

str(german_credit)

#Duration of Month. 

ggplot(german_credit,aes(x=Duration_in_month))+geom_histogram(fill="royalblue4")

#Binning can be done
table(german_credit$Duration_in_month)
#<1year,=1year,<=1.5year,<2year,=2year,<=3years,>3years
german_credit[Duration_in_month<=11,Duration:="LessThan1Yr"]
german_credit[Duration_in_month==12,Duration:="OneYr"]

german_credit[Duration_in_month>12 & Duration_in_month <=18, Duration:="LessThan1.5Yr"]
german_credit[Duration_in_month>18 & Duration_in_month<24,Duration:="LessThan2Yr"]
german_credit[Duration_in_month==24,Duration:="TwoYr"]
german_credit[Duration_in_month>24 & Duration_in_month<=36,Duration:="LessThanOrEqual3Yr"]
#german_credit[Duration_in_month==36,Duration:="ThreeYr"]
german_credit[Duration_in_month>36,Duration:="GreaterThan3Yr"]
levels(german_credit$Duration)
#Convert Duration to factor
german_credit$Duration<-factor(german_credit$Duration,levels = c("LessThan1Yr","OneYr","LessThan1.5Yr","LessThan2Yr","TwoYr","LessThanOrEqual3Yr","GreaterThan3Yr"))

ggplot(german_credit,aes(x=Duration,fill=as.factor(Default_status)))+geom_bar(position = "fill")
#Least Default Rate is for Loans Taken Less than 1 Year

#Create Dummy Variablesnfor factor Variables
#Create dummy variables for the factor levels
factor_cols<-colnames(german_credit[,sapply(german_credit, is.factor),with=F])
factor_cols
factor.credit<-subset(german_credit,select = factor_cols)
dummies<-sapply(factor.credit,function(x) model.matrix(~x-1,data=factor.credit)[,-1])
str(dummies)
dummies<-data.frame(dummies)

other_cols<-subset(german_credit,select=! colnames(german_credit) %in% factor_cols)
str(other_cols)
credit<-cbind(dummies,other_cols)
str(credit)
#61 variables and 1000 observations

#Drop Duration_in_month variable from credit variables
credit$Duration_in_month<-NULL

#Split credit data into train and test data, in 70:30 ration
library(caTools)

set.seed(100)
split_credit = sample.split(credit$Default_status, SplitRatio = 0.7)
train = credit[split_credit,]
test =credit[!(split_credit),]

#----------------------------End of Data Preparation-------------------------------------------------------


#---------------------------Data Modelliing--------------------------------------------------------------------------#

model_1<-glm(Default_status~.,data=train,family = "binomial")
summary(model_1) #734.73

#Apply stepwise selection on model_1
model_2<-step(model_1,direction="both")
summary(model_2)#684.69

sort(vif(model_2))
#Credit History A32 has high VIF and high significance. Lets not remove it and start removing insiginificant variables
#Number_of_existing_credits_at_this_bank.x2  Remove
model_3<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
               Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
               Credit_history.xA33 + Credit_history.xA34 + Purpose.xA41 + 
               Purpose.xA42 + Purpose.xA43 + Purpose.xA45 + Purpose.xA49 + 
               Savings_accountorbonds.xA64 + Savings_accountorbonds.xA65 + 
               Present_employment_since.xA74 + Installment_rate_in_percentage_of_disposable_income.x4 + 
               Personal_status_and_sex.xA93 + Other_debtors_or_guarantors.xA103 + 
               Present_residence_since.x2 + Other_installment_plans.xA143 + 
               Housing.xA152  + 
               Duration.xOneYr + Duration.xLessThan1.5Yr + Duration.xTwoYr + 
               Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
             family = "binomial", data = train)
summary(model_3) #684.92
sort(vif(model_3))

#Remove Purpose.xA45 
model_4<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
               Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
               Credit_history.xA33 + Credit_history.xA34 + Purpose.xA41 + 
               Purpose.xA42 + Purpose.xA43 + Purpose.xA49 + 
               Savings_accountorbonds.xA64 + Savings_accountorbonds.xA65 + 
               Present_employment_since.xA74 + Installment_rate_in_percentage_of_disposable_income.x4 + 
               Personal_status_and_sex.xA93 + Other_debtors_or_guarantors.xA103 + 
               Present_residence_since.x2 + Other_installment_plans.xA143 + 
               Housing.xA152  + 
               Duration.xOneYr + Duration.xLessThan1.5Yr + Duration.xTwoYr + 
               Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
             family = "binomial", data = train)
summary(model_4) #685.46
sort(vif(model_4))

#Purpose.xA49   
model_5<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
               Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
               Credit_history.xA33 + Credit_history.xA34 + Purpose.xA41 + 
               Purpose.xA42 + Purpose.xA43 + 
               Savings_accountorbonds.xA64 + Savings_accountorbonds.xA65 + 
               Present_employment_since.xA74 + Installment_rate_in_percentage_of_disposable_income.x4 + 
               Personal_status_and_sex.xA93 + Other_debtors_or_guarantors.xA103 + 
               Present_residence_since.x2 + Other_installment_plans.xA143 + 
               Housing.xA152  + 
               Duration.xOneYr + Duration.xLessThan1.5Yr + Duration.xTwoYr + 
               Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
             family = "binomial", data = train)
summary(model_5) #685.91
sort(vif(model_5))
#Remove Purpose.xA42 
model_6<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
               Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
               Credit_history.xA33 + Credit_history.xA34 + Purpose.xA41 + Purpose.xA43 + 
               Savings_accountorbonds.xA64 + Savings_accountorbonds.xA65 + 
               Present_employment_since.xA74 + Installment_rate_in_percentage_of_disposable_income.x4 + 
               Personal_status_and_sex.xA93 + Other_debtors_or_guarantors.xA103 + 
               Present_residence_since.x2 + Other_installment_plans.xA143 + 
               Housing.xA152  + 
               Duration.xOneYr + Duration.xLessThan1.5Yr + Duration.xTwoYr + 
               Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
             family = "binomial", data = train)
summary(model_6) #686.61
sort(vif(model_6))

#Purpose.xA43 
model_7<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
               Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
               Credit_history.xA33 + Credit_history.xA34 + Purpose.xA41 + 
               Savings_accountorbonds.xA64 + Savings_accountorbonds.xA65 + 
               Present_employment_since.xA74 + Installment_rate_in_percentage_of_disposable_income.x4 + 
               Personal_status_and_sex.xA93 + Other_debtors_or_guarantors.xA103 + 
               Present_residence_since.x2 + Other_installment_plans.xA143 + 
               Housing.xA152  + 
               Duration.xOneYr + Duration.xLessThan1.5Yr + Duration.xTwoYr + 
               Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
             family = "binomial", data = train)
summary(model_7) #686.39
sort(vif(model_7))

#Present_employment_since.xA74  
model_8<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
               Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
               Credit_history.xA33 + Credit_history.xA34 + Purpose.xA41 + 
               Savings_accountorbonds.xA64 + Savings_accountorbonds.xA65 +Installment_rate_in_percentage_of_disposable_income.x4 + 
               Personal_status_and_sex.xA93 + Other_debtors_or_guarantors.xA103 + 
               Present_residence_since.x2 + Other_installment_plans.xA143 + 
               Housing.xA152  + 
               Duration.xOneYr + Duration.xLessThan1.5Yr + Duration.xTwoYr + 
               Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
             family = "binomial", data = train)
summary(model_8) #687.84
sort(vif(model_8))

#Purpose.xA41
model_9<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
               Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
               Credit_history.xA33 + Credit_history.xA34 +
               Savings_accountorbonds.xA64 + Savings_accountorbonds.xA65 +Installment_rate_in_percentage_of_disposable_income.x4 + 
               Personal_status_and_sex.xA93 + Other_debtors_or_guarantors.xA103 + 
               Present_residence_since.x2 + Other_installment_plans.xA143 + 
               Housing.xA152  + 
               Duration.xOneYr + Duration.xLessThan1.5Yr + Duration.xTwoYr + 
               Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
             family = "binomial", data = train)
summary(model_9) #689.73
sort(vif(model_9))

#Savings_accountorbonds.xA64
model_10<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
               Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
               Credit_history.xA33 + Credit_history.xA34 +Savings_accountorbonds.xA65 +Installment_rate_in_percentage_of_disposable_income.x4 + 
               Personal_status_and_sex.xA93 + Other_debtors_or_guarantors.xA103 + 
               Present_residence_since.x2 + Other_installment_plans.xA143 + 
               Housing.xA152  + 
               Duration.xOneYr + Duration.xLessThan1.5Yr + Duration.xTwoYr + 
               Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
             family = "binomial", data = train)
summary(model_10) #692.87
sort(vif(model_10))

#Other_debtors_or_guarantors.xA103  
model_11<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
                Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
                Credit_history.xA33 + Credit_history.xA34 +Savings_accountorbonds.xA65 +Installment_rate_in_percentage_of_disposable_income.x4 + 
                Personal_status_and_sex.xA93 + 
                Present_residence_since.x2 + Other_installment_plans.xA143 + 
                Housing.xA152  + 
                Duration.xOneYr + Duration.xLessThan1.5Yr + Duration.xTwoYr + 
                Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
              family = "binomial", data = train)
summary(model_11) #695.13
sort(vif(model_11))

#Savings_accountorbonds.xA65 
model_12<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
                Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
                Credit_history.xA33 + Credit_history.xA34 +Installment_rate_in_percentage_of_disposable_income.x4 + 
                Personal_status_and_sex.xA93 + 
                Present_residence_since.x2 + Other_installment_plans.xA143 + 
                Housing.xA152  + 
                Duration.xOneYr + Duration.xLessThan1.5Yr + Duration.xTwoYr + 
                Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
              family = "binomial", data = train)
summary(model_12) #697.49
sort(vif(model_12))

#Duration.xOneYr 
model_13<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
                Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
                Credit_history.xA33 + Credit_history.xA34 +Installment_rate_in_percentage_of_disposable_income.x4 + 
                Personal_status_and_sex.xA93 + 
                Present_residence_since.x2 + Other_installment_plans.xA143 + 
                Housing.xA152   + Duration.xLessThan1.5Yr + Duration.xTwoYr + 
                Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
              family = "binomial", data = train)
summary(model_13) #700.24
sort(vif(model_13))

#Duration.xLessThan1.5Yr 
model_14<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
                Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
                Credit_history.xA33 + Credit_history.xA34 +Installment_rate_in_percentage_of_disposable_income.x4 + 
                Personal_status_and_sex.xA93 + 
                Present_residence_since.x2 + Other_installment_plans.xA143 + 
                Housing.xA152   +Duration.xTwoYr + 
                Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
              family = "binomial", data = train)
summary(model_14) #701.52
sort(vif(model_14))

#Duration.xTwoYr
model_15<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
                Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
                Credit_history.xA33 + Credit_history.xA34 +Installment_rate_in_percentage_of_disposable_income.x4 + 
                Personal_status_and_sex.xA93 + 
                Present_residence_since.x2 + Other_installment_plans.xA143 + 
                Housing.xA152   +
                Duration.xLessThanOrEqual3Yr + Duration.xGreaterThan3Yr, 
              family = "binomial", data = train)
summary(model_15) #702.37
sort(vif(model_15))

#All VIFS are below 3.

#Duration.xLessThanOrEqual3Yr
model_16<-glm(formula = Default_status ~ Status_of_existing_checking_account.xA13 + 
                Status_of_existing_checking_account.xA14 + Credit_history.xA32 + 
                Credit_history.xA33 + Credit_history.xA34 +Installment_rate_in_percentage_of_disposable_income.x4 + 
                Personal_status_and_sex.xA93 + 
                Present_residence_since.x2 + Other_installment_plans.xA143 + 
                Housing.xA152   +Duration.xGreaterThan3Yr, 
              family = "binomial", data = train)
summary(model_16) #703.41
sort(vif(model_16))



model_final<-model_16



#--------------------------Model Evaluation---------------------------------------------------------

library(Hmisc)
#C Statistic

#on train data
train$predicted_prob = predict(model_final,  type = "response")
rcorr.cens(train$predicted_prob,train$Default_status)
#0.797 is the c index on training set

test$predicted_prob = predict(model_final,newdata = test,type = "response")
rcorr.cens(test$predicted_prob,test$Default_status)
#0.705 is the c index on test data

#KS-Statistic
library(ROCR)

model_score <- prediction(train$predicted_prob,train$Default_status)

model_perf <- performance(model_score, "tpr", "fpr")

ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])

ks = max(ks_table)
ks #MAximum cumutable==lativ diff
which(ks_table == ks)
which(ks_table==ks)/nrow(train) #0.19, 2nd decile

#KS Statistic is in the first decile for train data, indicating a very good model
model_score_test <- prediction(test$predicted_prob,test$Default_status)

model_perf_test <- performance(model_score_test, "tpr", "fpr")

ks_table_test <- attr(model_perf_test, "y.values")[[1]] - (attr(model_perf_test, "x.values")[[1]])

ks = max(ks_table_test)
ks #MAximum cumutable==lativ diff
which(ks_table_test == ks)
which(ks_table_test==ks)/nrow(train) #0.13,2nd decile

#The model has KS statistic in 3rd decile for both train and test data

#ROC CURVE
plot(model_perf,col = "red")

#AUC
auc<-performance(model_score,measure="auc")
auc<-auc@y.values[[1]]
auc #0.79727838


#Lets calculate the COnfusion Matrix for threshold 0.16, to maximise sensitivity
library(caret)
confusionMatrix(as.numeric(train$predicted_prob >0.16),train$Default_status, positive = "1")


confusionMatrix(as.numeric(test$predicted_prob > 0.16),test$Default_status, positive = "1")


#To maximise accuracy, threshold=0.64
confusionMatrix(as.numeric(train$predicted_prob >0.64),train$Default_status, positive = "1")


confusionMatrix(as.numeric(test$predicted_prob > 0.64),test$Default_status, positive = "1")



