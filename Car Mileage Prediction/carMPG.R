
# In this project, Have implemented Linear Regression to predict the car mileage.
# Have identified the best subset of variables to predict the mileage.
#Used VIF() to remove multicollinearity

library(data.table)
library(ggplot2)
library(moments)

#AIM : Predict car mileage based on car features=2

#Requirements: 
#1.Model contain maximum of 5 variable
#2.VIF=2
#3.R2>80%

#Import Dataset
carmileage<-read.csv("carMPG.csv",na.strings = c("?",""," ",NA),stringsAsFactors = F)
str(carmileage)
#---------------------------------------DATA PREPARATION BEGINS--------------------------------------------------------------------------------------
#----------------------------Variable Formatting--------------------------------------------------------------------------------
#Convert Cylinders,Origin and Year into factors.
carmileage$Origin<-as.factor(carmileage$Origin)
carmileage$Model_year<-as.factor(carmileage$Model_year)
carmileage$Cylinders<-as.factor(carmileage$Cylinders)

#Let us bin Model_year into 3groups , <=2005, <=2010, <=2015 as old,new,latest
levels(carmileage$Model_year)[1:3]<-"old"
levels(carmileage$Model_year)[2:6]<-"new"

levels(carmileage$Model_year)[3:7]<-"latest"

#Create dummy variables for the factor levels
factor_cols<-colnames(carmileage[,sapply(carmileage, is.factor)])
factor_cols
factor.carmileage<-subset(carmileage,select = factor_cols)
dummies<-sapply(factor.carmileage,function(x) data.frame(model.matrix(~x-1,data=factor.carmileage))[,-1])
other_cols<-subset(carmileage,select=! colnames(carmileage) %in% factor_cols)
carmileage<-cbind(other_cols,dummies)
str(carmileage)
#---------------------------------End of Variable Formatting----------------------------------------------------------------------

#-----------------------------------Data Cleaning--------------------------------------------------------------------------------

#-----------------------------MISSING VALUES---------------------------------------------------
#Are there any missing values?
sum(is.na(carmileage)) #6 missing values
#Which columns have missing values?
colSums(is.na(carmileage))
#HorsePower Column has all 6 missing values

#Lets look at the distribution of HorsePower
ggplot(carmileage,aes(x=Horsepower))+geom_histogram(aes(y=..density..),fill="royalblue",col="lightblue")+geom_density(aes(y=..density..))
#Postive skewed nd looks like it  is bimodal and has outliers
skewness(carmileage$Horsepower,na.rm = T)#1.083
kurtosis(carmileage$Horsepower,na.rm=T)#3.6
summary(carmileage$Horsepower)
#Because it is skewed,lets replace the missing values by the median

median_horsepower<-median(carmileage$Horsepower,na.rm = T)
median_horsepower#93.5

carmileage$Horsepower[which(is.na(carmileage$Horsepower))]<-median_horsepower

#------------------------END OF MISSING VALUES-------------------------------------------------------

#---------------------OUTLIER TREATMENT-------------------------------------------------------

#MPG
ggplot(carmileage,aes(x=MPG))+geom_histogram(aes(y=..density..),fill="royalblue",col="lightblue")+geom_density(aes(y=..density..))
summary(carmileage$MPG)
boxplot(carmileage$MPG)

#There are no outliers in MPG

#Displacement
ggplot(carmileage,aes(x=Displacement))+geom_histogram(aes(y=..density..),fill="royalblue",col="lightblue")+geom_density(aes(y=..density..))
summary(carmileage$Displacement)
boxplot(carmileage$Displacement)
quantile(carmileage$Displacement,probs=seq(0,1,0.01))
#There are no outliers in Displacement

#Horsepower
summary(carmileage$Horsepower)
boxplot(carmileage$Horsepower)
quantile(carmileage$Horsepower,probs=seq(0,1,0.01))
#From 90 to 100 percentile we have a jump in horsepower.
#Number of data points with horsepower greater than 155.9
carmileage[carmileage$Horsepower>155.9,] #264 entries
#66% of data have horsepower > 155.9 and cannot be considered as outliers
quantile(carmileage$Horsepower,probs=seq(0.9,1,0.01))
carmileage[carmileage$Horsepower>200,] #117 data points with value greater than 200
#There is jump from 198 to 230 between 97 and 100 percentile. Let cap the values above 198
carmileage$Horsepower[which(carmileage$Horsepower>198)]<-198


#Weight
ggplot(carmileage,aes(x=Weight))+geom_histogram(aes(y=..density..),fill="royalblue",col="lightblue")+geom_density(aes(y=..density..))
#Positive Skewed Data
summary(carmileage$Weight)
boxplot(carmileage$Weight)

#No outliers in Weight

#Acceleration
ggplot(carmileage,aes(x=Acceleration))+geom_histogram(aes(y=..density..),fill="royalblue",col="lightblue")+geom_density(aes(y=..density..))
#Almost Normal
skewness(carmileage$Acceleration)
summary(carmileage$Acceleration)
boxplot(carmileage$Acceleration)
quantile(carmileage$Acceleration,probs=seq(0,1,0.1))
#No sudden jumps in quantile distribution, so no outlier treatment required.
quantile(carmileage$Acceleration,probs=seq(0.9,1,0.01))

#No outlier treatment for Acceleration

#------------------------END OUTLIER TREATMENT-------------------------------------------------------------------

#------------------------VARIABLE TRANSFORMATION----------------------------------------------


head(carmileage$Car_Name)
#Car Name is of the format "Company Model ModelNo(optional)"
#Lets extract Company from the Car_Name
rexp <- "^(\\w+)\\s?(.*)$"
carmileage$Company<-sub(rexp,"\\1",carmileage$Car_Name)
unique(carmileage$Company)

#chevrolet is represented by chevrolet,chevy or chevroelt
#Lets transform all forms of chevrolet to chevy
carmileage$Company[carmileage$Company=="chevy"]<-"chevrolet"
carmileage$Company[carmileage$Company=="chevroelt"]<-"chevrolet"

#convert vw and vokswagen to volkswagen
carmileage$Company[carmileage$Company=="vokswagen"|carmileage$Company=="vw"]<-"volkswagen"

#Parent company of chrysler is Fiat. So change chrysler to fiat
#carmileage$Company[carmileage$Company=="chrysler"]<-"fiat"

#toyouta is same as toyota
carmileage$Company[carmileage$Company=="toyouta"]<-"toyota"

#maxda is same as mazda
carmileage$Company[carmileage$Company=="maxda"]<-"mazda"


#Unique levels of company is 
unique(carmileage$Company)#30
#Convert Company to factor
carmileage$Company<-as.factor(carmileage$Company)
#Create dummy variables for Company
dummy_company<-data.frame(model.matrix(~Company-1,data=carmileage))[,-1]
head(dummy_company)


carmileage_1<-cbind(carmileage,dummy_company)
carmileage_1$Company<-NULL

head(carmileage_1)
str(carmileage_1)
#For Model Building we do not need Car_Name, so let us drop the carName
carmileage_1$Car_Name<-NULL



#----------------------END Variable Transformation--------------------------------------------------------------------------------------


#Divide the data set into train and test
set.seed(100)
indices= sample(1:nrow(carmileage_1), 0.7*nrow(carmileage_1))

train=carmileage_1[indices,]
test=carmileage_1[-indices,]

#---------------------------------------------END DATA PREPARATION--------------------------------------------------------------------------------

#--------------------------------------------BEGIN MODEL BUILDING-------------------------------------------------------------------------------

model_1<-lm(MPG~.,train)
summary(model_1)
#There are NA is model estimates indicating that there is a relationship with other variables

#Let us apply step wise approach

library(MASS)
library(car)
step<-stepAIC(model_1,direction = "both") #AIC=597.17
step

model_2<-lm(formula = MPG ~ Horsepower + Weight + Cylinders.x4 + Cylinders.x5 + 
              Cylinders.x6 + Cylinders.x8 + Model_year.xnew + Model_year.xlatest + 
              Origin.x3 + Companychrysler + Companydatsun + Companydodge + 
              Companyford + Companyplymouth + Companypontiac + Companytriumph + 
              Companyvolkswagen, data = train)
summary(model_2)

sort(vif(model_2))

#Remove cylinder.x4

model_3<-lm(formula = MPG ~ Horsepower + Weight + Cylinders.x5 + 
              Cylinders.x6 + Cylinders.x8 + Model_year.xnew + Model_year.xlatest + 
              Origin.x3 + Companychrysler + Companydatsun + Companydodge + 
              Companyford + Companyplymouth + Companypontiac + Companytriumph + 
              Companyvolkswagen, data = train)
summary(model_3)
sort(vif(model_3))

#Cylinders.x8 not significant and has high VIF can be removed
model_4<-lm(formula = MPG ~ Horsepower + Weight + Cylinders.x5 + 
              Cylinders.x6 + Model_year.xnew + Model_year.xlatest + 
              Origin.x3 + Companychrysler + Companydatsun + Companydodge + 
              Companyford + Companyplymouth + Companypontiac + Companytriumph + 
              Companyvolkswagen, data = train)
summary(model_4)
sort(vif(model_4))

#Weight,Horsepower and ModelYear_xlatest all have high VIF and high significance.
#But, Horsepower has the lowest p value among them, so removing Horsepower
model_5<-lm(formula = MPG ~ Weight + Cylinders.x5 + 
              Cylinders.x6 + Model_year.xnew + Model_year.xlatest + 
              Origin.x3 + Companychrysler + Companydatsun + Companydodge + 
              Companyford + Companyplymouth + Companypontiac + Companytriumph + 
              Companyvolkswagen, data = train)
summary(model_5)
sort(vif(model_5))

#All VIF < 2. Lets start to remove insignificant variables

#Cylinders.x5 has highest p value. Remove it
model_6<-lm(formula = MPG ~ Weight + 
              Cylinders.x6 + Model_year.xnew + Model_year.xlatest + 
              Origin.x3 + Companychrysler + Companydatsun + Companydodge + 
              Companyford + Companyplymouth + Companypontiac + Companytriumph + 
              Companyvolkswagen, data = train)
summary(model_6)

#Companychrysler remove
model_7<-lm(formula = MPG ~ Weight + 
              Cylinders.x6 + Model_year.xnew + Model_year.xlatest + 
              Origin.x3 + Companydatsun + Companydodge + 
              Companyford + Companyplymouth + Companypontiac + Companytriumph + 
              Companyvolkswagen, data = train)
summary(model_7)

#Companydodge remove
model_8<-lm(formula = MPG ~ Weight + 
              Cylinders.x6 + Model_year.xnew + Model_year.xlatest + 
              Origin.x3 + Companydatsun + 
              Companyford + Companyplymouth + Companypontiac + Companytriumph + 
              Companyvolkswagen, data = train)
summary(model_8)

#Origin.x3 remove
model_9<-lm(formula = MPG ~ Weight + 
              Cylinders.x6 + Model_year.xnew + Model_year.xlatest + Companydatsun + 
              Companyford + Companyplymouth + Companypontiac + Companytriumph + 
              Companyvolkswagen, data = train)
summary(model_9)

#Companytriumph remove
model_10<-lm(formula = MPG ~ Weight + 
              Cylinders.x6 + Model_year.xnew + Model_year.xlatest + Companydatsun + 
              Companyford + Companyplymouth + Companypontiac + 
              Companyvolkswagen, data = train)
summary(model_10)

#Companyford  remove
model_11<-lm(formula = MPG ~ Weight + 
               Cylinders.x6 + Model_year.xnew + Model_year.xlatest + Companydatsun  + Companyplymouth + Companypontiac + 
               Companyvolkswagen, data = train)
summary(model_11)

#Companydatsun remove
model_12<-lm(formula = MPG ~ Weight + 
               Cylinders.x6 + Model_year.xnew + Model_year.xlatest  + Companyplymouth + Companypontiac + 
               Companyvolkswagen, data = train)
summary(model_12)

#Companyplymouth remove
model_13<-lm(formula = MPG ~ Weight + 
               Cylinders.x6 + Model_year.xnew + Model_year.xlatest+ Companypontiac + 
               Companyvolkswagen, data = train)
summary(model_13)

#comany pontiac remove
model_14<-lm(formula = MPG ~ Weight + 
               Cylinders.x6 + Model_year.xnew + Model_year.xlatest + 
               Companyvolkswagen, data = train)
summary(model_14)
#Negative coeffieicnts indicating negative coraltion??
cor(train$Cylinders.x6,train$MPG)
cor(train$Weight,train$MPG) #-0.8251443
cor(train$Model_year.xnew,train$MPG)
cor(train$Model_year.xlatest,train$MPG)
cor(train$Companyvolkswagen,train$MPG)


#We will use model_14 as we have all significant variables and R2 is 0.8414 and Adjusted R2 is 0.8385
Predict<-predict(model_14,test[,-1])
test_R2<-(cor(Predict,test$MPG))^2
test_R2 #0.8108102

test<-cbind(test,Predict)
str(test)
library(ggplot2)
ggplot(test,aes(x=MPG,y=Predict))+geom_point()+xlab("Actual MPG ")+ylab("Predicted MPG")+ggtitle("Actual vs Predicted on Test Data")+stat_smooth(method = "lm", col = "red")

#layout(matrix(c(1,2,3,4),2,2))
par(mfrow=c(2,2))
plot(model_14)

