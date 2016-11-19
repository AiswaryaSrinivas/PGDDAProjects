# Set Working Directory
#setwd("C:\\Personal\\DataScience\\IIITB\\lab\\course3\\telecomchurn")

setwd("/home/aiswarya/upgradDataAnalytics/Course3/TelecomChurnPredctionCaseStudy/")
# Install and Load the required packages
library(MASS)
library(car)
library(e1071)
library(ROCR)
library(caret)
library(data.table)
library(ggplot2)
library(data.table)
library(dummies)
library(Hmisc)
library(caTools)

#----------------------------------------------------------------#
#Checkpoint-1: Data Understanding and Preparation of Master File.
#----------------------------------------------------------------#

# Load the given files.
internet_data<-fread("internet_data.csv")
churn_data<-fread("churn_data.csv")
customer_data<-fread("customer_data.csv")

#let us have a quick view at the files
View(internet_data)
View(churn_data)
View(customer_data)

#all the 3 data sets having a common column customerID.
#all 3 data sets have same no of observations # 7043

# Collate the 3 files in a single file.

#before we collate all these 3 files, lets understand the granualarity of each data set
length(internet_data$customerID)-length(unique(internet_data$customerID)) #0
length(churn_data$customerID)-length(unique(churn_data$customerID)) #0
length(customer_data$customerID)-length(unique(customer_data$customerID))#0

#Good! Customer ID is not repeating, can be considered as granualarity. 
length(unique(internet_data$customerID))== length(unique(customer_data$customerID)) # TRUE
length(unique(churn_data$customerID))== length(unique(customer_data$customerID)) # TRUE


#lets us merge internet data and customer data. We need all the columns from both the data sets.
churn<-merge(customer_data,internet_data,by="customerID",all=T)
#now merge churn data also to the master frame. We need all the columns from both the data sets.
churn<-merge(churn,churn_data,by="customerID",all=T)

# Understand the structure of the collated file.
str(churn)
# 7043 observations, 21 variables.


#Lets us convert the data into the correct dataTypes
churn$gender<-as.factor(churn$gender)
#churn$SeniorCitizen <- as.factor(churn$SeniorCitizen)
churn$Partner<-as.factor(churn$Partner)
churn$Dependents<-as.factor(churn$Dependents)
churn$PhoneService<-as.factor(churn$PhoneService)
churn$MultipleLines<-as.factor(churn$MultipleLines)
churn$InternetService<-as.factor(churn$InternetService)
churn$OnlineSecurity<-as.factor(churn$OnlineSecurity)
churn$OnlineBackup<-as.factor(churn$OnlineBackup)

churn$DeviceProtection<-as.factor(churn$DeviceProtection)
churn$TechSupport<-as.factor(churn$TechSupport)
churn$StreamingTV<-as.factor(churn$StreamingTV)
churn$StreamingMovies<-as.factor(churn$StreamingMovies)
churn$Contract<-as.factor(churn$Contract)
churn$PaperlessBilling<-as.factor(churn$PaperlessBilling)
churn$PaymentMethod<-as.factor(churn$PaymentMethod)
churn$Churn<-as.factor(churn$Churn)
str(churn)

#We have created Master Dataset, so let us remove the individula dataset to release memory.
rm(internet_data)
rm(customer_data)
rm(churn_data)


#-------------------------------------------------------------#
#  Checkpoint -2: Exploratory Data Analysis
#-------------------------------------------------------------#

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

churn.barPlot.dist <- function(ds,var.name,title,xlabel,ylabel="Number of Observations",percentage=FALSE){
  
  p1 <- ggplot(ds,aes(var.name,fill=var.name))+geom_bar(position = "stack", width = .5)+ggtitle(title)
  p1 <- p1+xlab(xlabel)+ylab(ylabel)+scale_fill_manual(values=alpha(c("orchid2", "slateblue2","midnightblue","maroon"),.8))
  if(percentage){
  p1 <-p1+geom_text(aes(label=paste(round((..count../sum(..count..)*100)),"%",sep="")),stat= "count",vjust=1.8)
  }else{
  p1 <-p1+geom_text(aes(label = ..count..),stat= "count",position=position_dodge(.9),vjust = -1)
  }
  p1 <- p1+ theme(legend.title = element_blank())
  return(p1)
  
}

churn.barPlot.withTarget <- function(ds,var.name,title,xlabel,ylabel="Number of Observations",percentage=FALSE,position="stack",grid=TRUE){
  
  p1 <- ggplot(ds,aes(var.name,fill=Churn))+geom_bar(position = position, width = .5)+ggtitle(title)
  if(percentage){
    p1 <-p1+geom_text(aes(label=paste(round((..count../sum(..count..)*100)),"%",sep="")),stat= "count",position=position_dodge(.9),vjust = -1, hjust =.3)
    ylabel="observations in %"
  }else{
  p1 <-p1+geom_text(aes(label = ..count..),stat= "count",position=position_dodge(.9),vjust = -1, hjust =.3)
  }
  p1 <- p1+xlab(xlabel)+ylab(ylabel)+scale_fill_manual(values=alpha(c("orchid2", "slateblue3"),.8))
  p1 <- p1+ theme(plot.title=element_text(size=14, face="bold"))
  if(!grid){
    p1 <- p1+ theme(panel.grid=element_blank(),axis.line = element_line(colour = "black"))
  }
  return(p1)
  
}

#Gender
table(churn$gender,churn$Churn)/nrow(churn)
plot_gender<- churn.barPlot.dist(churn,churn$gender,"Distribution of Gender","Gender")
plot_gender

plot_gender_churn<- churn.barPlot.withTarget(churn,churn$gender,"Gender vs Churn","Gender",position = "dodge")
plot_gender_churn
#Almost equal % of males and females churn. 

#Senior Citizen
table(churn$SeniorCitizen)
plot_senior<-churn.barPlot.dist(churn,churn$SeniorCitizen,"Distribution of Gender","Senior Citizen",percentage = TRUE)
plot_senior  
#~16% of the total customers are senior citizens

plot_senior_churn<- churn.barPlot.withTarget(churn,churn$SeniorCitizen,"Senior Citizen vs Churn","Senior Citizen",position = "dodge",percentage = TRUE,grid = FALSE)
plot_senior_churn
#Churn is around 45% in Senior Citizens. Senior Citizen have higher tendency to churn.


#Partner
table(churn$Partner)
plot_partner<-churn.barPlot.dist(churn,churn$Partner,"Distribution of Customers With and Without Partners","Has Partner",percentage = TRUE)
plot_partner

plot_partner_churn<- churn.barPlot.withTarget(churn,churn$Partner,"Partner vs Churn","Has Partner",position = "dodge",percentage = TRUE)
plot_partner_churn
#Churn Rate is higher for those, who do not have a partner


#Dependents
table(churn$Dependents)
plot_dependents<-churn.barPlot.dist(churn,churn$Dependents,"Distribution of Customers with and without Dependents",xlabel = "Has Dependents",percentage = TRUE)
plot_dependents

plot_dependents_churn<-churn.barPlot.withTarget(churn,churn$Dependents,"Dependents vs Churn","Has Dependents",position = "dodge",percentage = TRUE)
plot_dependents_churn
#Lower churn rate for those with dependents

#Tenure
tenure_hist_plot <- ggplot(churn,aes(x=tenure))
tenure_hist_plot <- tenure_hist_plot+geom_histogram(aes(y=..count..),binwidth = 1, colour="royalblue4", fill="lightblue")
tenure_hist_plot <- tenure_hist_plot+geom_density(aes(y = ..count..*1),alpha=.1, fill="blue") 
tenure_hist_plot <- tenure_hist_plot+labs(x="Tenure")+ggtitle("Distribution of Tenure")
tenure_hist_plot <- tenure_hist_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
tenure_hist_plot <- tenure_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
tenure_hist_plot

plot_tenure_churn<-ggplot(churn,aes(x=Churn, y=tenure, col= Churn)) +geom_violin(aes(y=tenure))+ stat_summary(fun.y = "mean", geom= "point")+scale_color_brewer(palette = "Paired") +stat_summary(fun.y = "median", geom = "point", col="black", shape = "M")
plot_tenure_churn<-plot_tenure_churn+ggtitle("Churn for Different Tenures")
plot_tenure_churn
#Lower Tenure, higher the churn
#The mean and mmedian tenure for those who churn is much lower than that of non churners


#PhoneService
plot_phoneservice <- churn.barPlot.dist(churn,churn$PhoneService,"Distribution of Customers with and without Phone Service",xlabel = "PhoneService", ylabel = "Percentage of Customers",percentage = TRUE)
plot_phoneservice
#around 90% of the people have Phone Service

plot_phoneservice_churn<-churn.barPlot.withTarget(churn,churn$PhoneService,"PhoneService vs Churn","Has PhoneService",position = "dodge",percentage = TRUE,grid = FALSE)
plot_phoneservice_churn
#Churn Rate is high for people who opted phone service than who didnt opt 


#MultipleLines
plot_multiplelines <- churn.barPlot.dist(churn,churn$MultipleLines,"Distribution of Customers with and without multiplelines",xlabel = "plot_multiplelines", ylabel = "Percentage of Customers",percentage = TRUE)
plot_multiplelines

plot_multiplelines_churn<-churn.barPlot.withTarget(churn,churn$MultipleLines,"MultipleLines vs Churn","Has MultipleLines",position = "dodge",percentage = TRUE)
plot_multiplelines_churn
#Higher churn rate among those with Multiple Lines. Churn rate is 25% for users with no MultipleLines and No PhoneService

#We can remove the column PhoneService, because If MultipleLine is No=> Have PhoneService But only one Line
#Is Multiline is Yes=> Have PhoneService, if MultiLines in NoPhoneService=> No phone Service

#InternetService
plot_internet <- churn.barPlot.dist(churn,churn$InternetService,"Distribution InternetService",xlabel = "InternetService", ylabel = "Percentage of Customers",percentage = TRUE)
plot_internet
#Approximately 44% of the people have Opted for fibre optic

plot_internet_churn<-churn.barPlot.withTarget(churn,churn$InternetService,"InternetService vs Churn","InternetService",position = "dodge",percentage = TRUE)
plot_internet_churn
#Higher churn rate among Customers who opted for Fibre Optic

#OnlineSecurity
plot_onlinesecurity <- churn.barPlot.dist(churn,churn$OnlineSecurity,"Distribution OnlineSecurity",xlabel = "OnlineSecurity", ylabel = "Percentage of Customers",percentage = TRUE)
plot_onlinesecurity
#~50% of the customers have not opted for OnlineSecurity

plot_onlinesecurity_churn<-churn.barPlot.withTarget(churn,churn$OnlineSecurity,"OnlineSecurity vs Churn","OnlineSecurity",position = "dodge",percentage = TRUE)
plot_onlinesecurity_churn
#customers with no Online Security are Churn more.

#We can ignore InternetService as, Online Security represents people with no Online Security.  

#OnlineBackup
plot_onlinebackup <- churn.barPlot.dist(churn,churn$OnlineBackup,"Distribution OnlineBackup",xlabel = "OnlineBackup", ylabel = "Percentage of Customers",percentage = TRUE)
plot_onlinebackup
#Almost 44% of the customers,do not opt for Online Backup.

plot_onlinebackup_churn<-churn.barPlot.withTarget(churn,churn$OnlineBackup,"OnlineBackup vs Churn","OnlineBackup",position = "dodge",percentage = TRUE)
plot_onlinebackup_churn
#Higher churn rate among people with No Online Backup.

----------------------------------------------------------------------------------------------------------------------------------------
  #Just for the heck of it.. What proportion of people with No Online Security, prefer OnlineBackup?
  ggplot(churn[OnlineSecurity=="No",],aes(x=OnlineBackup))+geom_bar(fill="royalblue4",aes(y=(..count..)/sum(..count..)))
#~60% of the people with No Online Security,Do not take OnlineBackup.
-----------------------------------------------------------------------------------------------------------------------------------------
#DeviceProtection
plot_deviceprotection <- churn.barPlot.dist(churn,churn$DeviceProtection,"Distribution DeviceProtection",xlabel = "DeviceProtection", ylabel = "Percentage of Customers",percentage = TRUE)
plot_deviceprotection
#44% of the custimer have not opted for Device Protection
plot_deviceprotection_churn<-churn.barPlot.withTarget(churn,churn$DeviceProtection,"DeviceProtection vs Churn","DeviceProtection",position = "dodge",percentage = TRUE)
plot_deviceprotection_churn
#Higher churn rate for users with No Device Protection

----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  #Just for the heck for it.. What proportion of people with NoOnlineBackup,opted for DeviceProtection
  ggplot(churn[OnlineBackup=="No",],aes(x=DeviceProtection))+geom_bar(fill="royalblue4",aes(y=(..count..)/sum(..count..)))+ylab("Percentage")
table(churn[OnlineBackup=="No",DeviceProtection])/nrow(churn[OnlineBackup=="No"])
#~64% of the people with No Online Backup, doo not opt for device protection
----------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #TechSupport
plot_techsupport <- churn.barPlot.dist(churn,churn$TechSupport,"Distribution TechSupport",xlabel = "TechSupport", ylabel = "Percentage of Customers",percentage = TRUE)
plot_techsupport
#49% of the customers, have opted for no tech support
plot_techsupport_churn<-churn.barPlot.withTarget(churn,churn$TechSupport,"TechSupport vs Churn","TechSupport",position = "dodge",percentage = TRUE)
plot_techsupport_churn
#Higher churn rate for users who didnt opt TechSupport

#StreamingTV 
plot_tv <- churn.barPlot.dist(churn,churn$StreamingTV,"Distribution StreamingTV",xlabel = "StreamingTV", ylabel = "Percentage of Customers",percentage = TRUE)
plot_tv
#38% opted for streaming TV

plot_tv_churn<-churn.barPlot.withTarget(churn,churn$StreamingTV,"StreamingTV vs Churn","StreamingTV",position = "dodge",percentage = TRUE)
plot_tv_churn
#Churn Rate is almost equal for yes and no StreamingTV.

#StreamingMovies
plot_movies <- churn.barPlot.dist(churn,churn$StreamingMovies,"Distribution StreamingMovies",xlabel = "StreamingMovies", ylabel = "Percentage of Customers",percentage = TRUE)
plot_movies

plot_movies_churn<-churn.barPlot.withTarget(churn,churn$StreamingMovies,"StreamingMovies vs Churn","StreamingMovies",position = "dodge",percentage = TRUE)
plot_movies_churn

#Contract
plot_contract <- churn.barPlot.dist(churn,churn$Contract,"Distribution of Contract",xlabel = "Contract", ylabel = "Percentage of Customers",percentage = TRUE)
plot_contract
#Most of them have taken a Month-Month Contract

plot_contract_churn<-churn.barPlot.withTarget(churn,churn$Contract,"Contract vs Churn","Contract",position = "dodge",percentage = TRUE)
plot_contract_churn
#Highest Churn Rate for Month-to-Month Contract

#PaperlessBilling
plot_paperless <- churn.barPlot.dist(churn,churn$PaperlessBilling,"Distribution of PaperlessBilling",xlabel = "PaperlessBilling", ylabel = "Percentage of Customers",percentage = TRUE)
plot_paperless
#60% of the people have opted for Paperless Billing
plot_paperless_churn<-churn.barPlot.withTarget(churn,churn$PaperlessBilling,"PaperlessBilling vs Churn","PaperlessBilling",position = "dodge",percentage = TRUE)
plot_paperless_churn
#~50% of the People who opted for PaperlessBilling,churn.

#PaymentMethod
plot_payment <- churn.barPlot.dist(churn,churn$PaymentMethod,"Distribution of PaymentMethod",xlabel = "PaymentMethod", ylabel = "Percentage of Customers",percentage = TRUE)
plot_payment
#~34% of the PaymentMethod is via Electronic Check
plot_payment_churn<-churn.barPlot.withTarget(churn,churn$PaymentMethod,"PaymentMethod vs Churn","PaymentMethod",position = "dodge",percentage = TRUE)
plot_payment_churn
#~80% churn Rate for Electroic Check

#MonthlyCharges
boxplot(churn$MonthlyCharges)
#No outliers, and it is skewed. 
#MonthlyCharges
monthlyCharges_hist_plot <- ggplot(churn,aes(x=MonthlyCharges))
monthlyCharges_hist_plot <- monthlyCharges_hist_plot+geom_histogram(aes(y=..count..),binwidth = 1, colour="royalblue4", fill="lightblue")
monthlyCharges_hist_plot <- monthlyCharges_hist_plot+geom_density(aes(y = ..count..*1),alpha=.1, fill="blue") 
monthlyCharges_hist_plot <- monthlyCharges_hist_plot+labs(x="MonthlyCharges")+ggtitle("Distribution of MonthlyCharges")
monthlyCharges_hist_plot <- monthlyCharges_hist_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
monthlyCharges_hist_plot <- monthlyCharges_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
monthlyCharges_hist_plot


plot_monthlycharges_churn<-ggplot(churn,aes(x=Churn,y=MonthlyCharges,col=Churn))+geom_violin(aes(y=MonthlyCharges))+ stat_summary(fun.y = "mean", geom= "point")+scale_color_brewer(palette = "Paired") +stat_summary(fun.y = "median", geom = "point", col="black", shape = "M")
plot_monthlycharges_churn
#The minimum, monhtly charges for people who churn is higher. 
#The median monthly charges is higher for those who churn
#People who pay higher, tend to churn more

#TotalCharges
boxplot(churn$TotalCharges)
#Right Skewed.No Outliers.

#TotalCharges
TotalCharges_hist_plot <- ggplot(churn,aes(x=TotalCharges))
TotalCharges_hist_plot <- TotalCharges_hist_plot+geom_histogram(aes(y=..count..),binwidth = 150, colour="royalblue4", fill="lightblue")
TotalCharges_hist_plot <- TotalCharges_hist_plot+geom_density(aes(y = ..count..*150),alpha=.1, fill="blue") 
TotalCharges_hist_plot <- TotalCharges_hist_plot+labs(x="TotalCharges")+ggtitle("Distribution of TotalCharges")
TotalCharges_hist_plot <- TotalCharges_hist_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
TotalCharges_hist_plot <- TotalCharges_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
TotalCharges_hist_plot

plot_totalcharges_churn<-ggplot(churn,aes(x=Churn,y=TotalCharges,col=Churn))+geom_violin(aes(y=TotalCharges))+ stat_summary(fun.y = "mean", geom= "point")+scale_color_brewer(palette = "Paired") +stat_summary(fun.y = "median", geom = "point", col="black", shape = "M")
plot_totalcharges_churn
#People who churn, pay lower Total Charges.

#Churn
plot_churn<-ggplot(churn,aes(x=Churn,fill=Churn))+geom_bar(aes(y=(..count..)/sum(..count..)))+scale_fill_brewer(palette = "Paired")+ylab("Percentage")+ggtitle("Distribution of Churn Rate")
plot_churn
#~25% of the people churn.

TotalCharges_Churn <- ggplot(churn,aes(x=TotalCharges, fill = Churn))
TotalCharges_Churn <- TotalCharges_Churn+geom_histogram(aes(y=..count..),binwidth = 150)
TotalCharges_Churn <- TotalCharges_Churn+scale_fill_manual(values=alpha(c("orchid2", "slateblue2"),.8))
TotalCharges_Churn

MonthlyCharges_Churn <- ggplot(churn,aes(x=MonthlyCharges, fill = Churn))
MonthlyCharges_Churn <- MonthlyCharges_Churn+geom_histogram(aes(y=..count..),binwidth = 4)
MonthlyCharges_Churn <- MonthlyCharges_Churn+scale_fill_manual(values=alpha(c("orchid2", "slateblue2"),.8))
MonthlyCharges_Churn

tenure_Churn <- ggplot(churn,aes(x=tenure, fill = Churn))
tenure_Churn <- tenure_Churn+geom_histogram(aes(y=..count..),binwidth = 1)
tenure_Churn <- tenure_Churn+scale_fill_manual(values=alpha(c("orchid2", "slateblue2"),.8))
tenure_Churn

#grid
library(gridExtra)
grid.arrange(plot_gender,plot_senior,plot_dependents,plot_partner,ncol=2)

grid.arrange(plot_gender_churn,plot_senior_churn,plot_dependents_churn,plot_partner_churn,
             plot_multiplelines_churn,plot_internet_churn,plot_onlinesecurity_churn,plot_onlinebackup_churn,ncol=2)

grid.arrange(plot_deviceprotection_churn,plot_techsupport_churn,plot_tv_churn,plot_movies_churn,
             plot_phoneservice_churn,plot_contract_churn,plot_paperless_churn,plot_payment_churn,ncol=2)

grid.arrange(TotalCharges_Churn,MonthlyCharges_Churn,tenure_Churn,ncol=1)


#-------------------------------------END OF EDA-----------------------------------------------------------#

#-------------------------------------------------------------#
#  Checkpoint -3: Data Preparation
#-------------------------------------------------------------#


# Check Duplication 
sum(duplicated(churn$customerID)) # which one is correct?????
sum(duplicated(churn))
# No Duplicated Records!


#-------------MISSING VALUE AND OUTLIER TREATMENT----------------------#


#Missing Values
colSums(is.na(churn))
#11 missing values are in  Total Charges

#let us see the records for where Total Charges value is NA
churn[is.na(TotalCharges)]
# Notice all the of observations tenure is 0.
churn[tenure==0]
# we can clearly see that for new customers( whose tenure is zero) Total Charges value is NA.

# May be we can replace Total Charges missing values with zero. (with an assumption that they have not paid anything yet)

churn[is.na(TotalCharges),TotalCharges:=0]
sum(is.na(churn$TotalCharges))
summary(churn$TotalCharges)

colSums(is.na(churn))

#Outlier treatment
#--------------------

#Tenure
tenure_hist_plot <- ggplot(churn,aes(x=tenure))
tenure_hist_plot <- tenure_hist_plot+geom_histogram(aes(y=..count..),binwidth = 1, colour="royalblue4", fill="lightblue")
tenure_hist_plot <- tenure_hist_plot+geom_density(aes(y = ..count..*1),alpha=.1, fill="blue") 
tenure_hist_plot <- tenure_hist_plot+labs(x="Tenure")+ggtitle("Distribution of Tenure")
tenure_hist_plot <- tenure_hist_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
tenure_hist_plot <- tenure_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
tenure_hist_plot

boxplot(churn$tenure)
quantile(churn$tenure,probs=seq(0,1,0.01))
#There are no outliers in Tenure


#MonthlyCharges
monthlyCharges_hist_plot <- ggplot(churn,aes(x=MonthlyCharges))
monthlyCharges_hist_plot <- monthlyCharges_hist_plot+geom_histogram(aes(y=..count..),binwidth = 1, colour="royalblue4", fill="lightblue")
monthlyCharges_hist_plot <- monthlyCharges_hist_plot+geom_density(aes(y = ..count..*1),alpha=.1, fill="blue") 
monthlyCharges_hist_plot <- monthlyCharges_hist_plot+labs(x="MonthlyCharges")+ggtitle("Distribution of MonthlyCharges")
monthlyCharges_hist_plot <- monthlyCharges_hist_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
monthlyCharges_hist_plot <- monthlyCharges_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
monthlyCharges_hist_plot


boxplot(churn$MonthlyCharges)
quantile(churn$MonthlyCharges,probs=seq(0,1,0.01))
#There are no outliers 

#TotalCharges
TotalCharges_hist_plot <- ggplot(churn,aes(x=TotalCharges))
TotalCharges_hist_plot <- TotalCharges_hist_plot+geom_histogram(aes(y=..count..),binwidth = 150, colour="royalblue4", fill="lightblue")
TotalCharges_hist_plot <- TotalCharges_hist_plot+geom_density(aes(y = ..count..*150),alpha=.1, fill="blue") 
TotalCharges_hist_plot <- TotalCharges_hist_plot+labs(x="TotalCharges")+ggtitle("Distribution of TotalCharges")
TotalCharges_hist_plot <- TotalCharges_hist_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
TotalCharges_hist_plot <- TotalCharges_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
TotalCharges_hist_plot

boxplot(churn$TotalCharges)
quantile(churn$TotalCharges,probs=seq(0,1,0.01))
#No outliers

#With this we are done with numarical variable outliers , let us see categorical if there are any outliers
no.of.rows <- nrow(churn)
table(churn$gender)*100/no.of.rows # All levels are in significant ratio
table(churn$SeniorCitizen)*100/no.of.rows # All levels are in significant ratio
table(churn$Partner)*100/no.of.rows # All levels are in significant ratio
table(churn$Dependents)*100/no.of.rows # All levels are in significant ratio
table(churn$MultipleLines)*100/no.of.rows # All levels are in significant ratio
table(churn$InternetService)*100/no.of.rows # All levels are in significant ratio
table(churn$OnlineSecurity)*100/no.of.rows # All levels are in significant ratio
table(churn$OnlineBackup)*100/no.of.rows # All levels are in significant ratio
table(churn$DeviceProtection)*100/no.of.rows # All levels are in significant ratio
table(churn$TechSupport)*100/no.of.rows # All levels are in significant ratio
table(churn$StreamingTV)*100/no.of.rows # All levels are in significant ratio
table(churn$StreamingMovies)*100/no.of.rows # All levels are in significant ratio
table(churn$PhoneService)*100/no.of.rows # All levels are in significant ratio
table(churn$Contract)*100/no.of.rows # All levels are in significant ratio
table(churn$PaperlessBilling)*100/no.of.rows # All levels are in significant ratio
table(churn$PaymentMethod)*100/no.of.rows # All levels are in significant ratio

#-----------------------------------------------------------------------------#
# Checkpoint 4: Model Building
#-----------------------------------------------------------------------------#


## 1. Model - K-NN:
#------------------

# Data Preparation for K-NN Model:

# our dataset contains, both numarical and categorical variables. so first convert categorical variables to numeric
# lets us create dummy variables.
factor_cols<-colnames(churn[,sapply(churn, is.factor),with=F])
factor_cols

factor.churn<-churn[,colnames(churn) %in% factor_cols,with=F]
str(factor.churn)

other_cols<-churn[,!colnames(churn) %in% colnames(factor.churn),with=F]
head(other_cols)


#Lets create dummy variables for the factor_cols.
dummies<-sapply(factor.churn,function(x) model.matrix(~x-1,data=factor.churn)[,-1])
str(dummies)
dummies<-data.frame(dummies)
#View(dummies)

#After creating dummies, we will hava lot of repeated columns,
#For example DeviceProtection.xNoPhoneService is same as OnlineBackup.xNoPhoneService.
#Need to remove those columns
col<-"InternetService.xNo"
all(dummies[,col]== dummies$DeviceProtection.xNo.internet.service)
all(dummies[,col]==dummies$OnlineSecurity.xNo.internet.service)
all(dummies[,col]==dummies$OnlineBackup.xNo.internet.service)
all(dummies[,col]==dummies$TechSupport.xNo.internet.service)
all(dummies[,col]==dummies$StreamingTV.xNo.internet.service)
all(dummies[,col]==dummies$StreamingMovies.xNo.internet.service)
#We can see all xNo.internet.service has same values, so we can remove those columns, leaving only InternetService.xNo
removeCols<-c("DeviceProtection.xNo.internet.service","OnlineSecurity.xNo.internet.service","OnlineBackup.xNo.internet.service","TechSupport.xNo.internet.service","StreamingTV.xNo.internet.service","StreamingMovies.xNo.internet.service")
dummies<-dummies[,!colnames(dummies) %in% removeCols]
str(dummies)

View(dummies)

all(dummies[,"MultipleLines.xNo.phone.service"]==dummies$PhoneService)#TRUE
#We can remove Phone Service because, Multiple also represents users with No Phone Service
dummies<-dummies[,!colnames(dummies) %in% c("PhoneService")]
str(dummies)

#Let us combine the dummies and other cols from churn data 
churn_new<-cbind(other_cols,dummies)
churn_new<-data.table(churn_new)
str(churn_new)

# Target vaiable should be of factor type.
churn_new$Churn <- as.factor(churn_new$Churn)
levels(churn_new$Churn)

# Scaling :
#-----------
#numerics columns are at different ranges. TotalCharges variable has high range and other 2 variables's range is low.
#Scale the numerics columns
churn_new$MonthlyCharges <- scale(churn_new$MonthlyCharges)
churn_new$TotalCharges <- scale(churn_new$TotalCharges)
churn_new$tenure <- scale(churn_new$tenure)

#SPLIT DATA INTO TRAIN AND TEST:
#-------------------------------
# split the data into 70:30
library(caret)
library(caTools)
set.seed(100)
indices<-sample.split(churn_new$Churn,SplitRatio = 0.7)
train<-churn_new[indices,]
test<-churn_new[indices==F,]

# Modelling and Evaluation
#----------------------------

# True class labels of training data
cl <- train$Churn
#Training and testing data without the true labels
data_train <- train[,.SD,.SDcols=-c(1,25)]
data_test1 <- test[,.SD,.SDcols=-c(1,25)]

# Finding the  optimal K. ( perefer always k to be odd no to have better classification power)
model <- train(
  Churn~., 
  data=train[,.SD,.SDcols=-c(1)],
  method='knn',
  tuneGrid=expand.grid(.k=seq(1, 99, 2)),
  metric='Accuracy',
  trControl=trainControl(
    method='repeatedcv', 
    number=10, 
    repeats=15))

#Generating the plot of the model
model
plot(model)

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was k = 37.

#KNn with 37 NN
library(class)
impknn <- knn(data_train,data_test1, cl, k = 37,prob = TRUE)
table(impknn,test$Churn)
confusionMatrix(impknn, test$Churn, positive ="1" )

#performance metrics - Accuracy, Sensitivity, Specificity and AUC.
#----------------------------------------------------------------
#  Accuracy - 0.7956513
#  Sensitivity - 0.5348
#  Specificity - 0.8898

#calculating the values for ROC curve

pred <- prediction(attr(impknn,"prob"),test$Churn)
perf <- performance(pred,"tpr","fpr")


# changing params for the ROC plot - width, etc
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)

# plotting the ROC curve
plot(perf,col="black",lty=3, lwd=3)

# Area under the curve
auc <- performance(pred,"fpr","tpr",measure = "auc")
auc <- 1-unlist(auc@y.values)
auc # 0.753081
# in general auc value 0.75 is a good score

# ------------------------END OF K-NN ----------------------------------------#

#---------------------------------------------------------------------------#
# Naive Bayes Model:
#---------------------------------------------------------------------------#
str(train)
#ID col not used for analysis, so lets remove the id col
train1<-train[,-1,with=F]
test1<-test[,-1,with=F]
str(test1)

#test1 remove the predictor column 
test1<-test1[,-24,with=F]

train1$Churn<-as.factor(train1$Churn)

nb_model<-naiveBayes(Churn~.,data=train1)
pred<-predict(nb_model,test1,type = "raw")
head(pred)
predprob<-pred[,2] #Probability of Churn[ the 1st column is the probability fo non churning and 2nd column is the probability og churning]
head(predprob)
realvec<-test$Churn

pr<-prediction(predprob,realvec)
prf<-performance(pr,'tpr',"fpr")
plot(prf)
auc<-performance(pr,measure = "auc")
auc<-auc@y.values[[1]]
auc #0.8268274


confusionMatrix(as.numeric(predprob > 0.3),test$Churn, positive = "1")
confusionMatrix(as.numeric(predprob > 0.5),test$Churn, positive = "1")
confusionMatrix(as.numeric(predprob > 0.7),test$Churn, positive = "1")





# ------------------------END OF Naive Bayes ----------------------------------------#

#-------------------------------------------------------------------------------------#
#    LOGISTIC REGRESSION
#-------------------------------------------------------------------------------------#


#prepare the initial model
LR_model_1 <- glm(Churn~., family = binomial, data = train1)
summary(LR_model_1)

# use step to find the ideal model
step <- step(LR_model_1, direction = "both")
# View step object to get the equation for optimised equation
step

library(car)
#Stepwise Selection process for regression
LR_model_2 <- glm(formula = Churn ~ SeniorCitizen + tenure + MonthlyCharges + 
                    TotalCharges + MultipleLines.xNo.phone.service + MultipleLines.xYes + 
                    InternetService.xFiber.optic + InternetService.xNo + OnlineBackup.xYes + 
                    DeviceProtection.xYes + StreamingTV.xYes + StreamingMovies.xYes + 
                    Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                    PaymentMethod.xElectronic.check, family = binomial, data = train1)


summary(LR_model_2)
sort(vif(LR_model_2))

# removing MultipleLines.xNo.phone.service based on VIF value and low significance of p-value (2 stars)
LR_model_3 <- glm(formula = Churn ~ SeniorCitizen + tenure + MonthlyCharges + 
                    TotalCharges + MultipleLines.xYes + 
                    InternetService.xFiber.optic + InternetService.xNo + OnlineBackup.xYes + 
                    DeviceProtection.xYes + StreamingTV.xYes + StreamingMovies.xYes + 
                    Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                    PaymentMethod.xElectronic.check, family = binomial, data = train1)


summary(LR_model_3)
sort(vif(LR_model_3))

#check for multicollinearity
cor_cols <- train1[, colnames(train1) %in% c("MonthlyCharges","TotalCharges", "tenure"), with = F]
# check the correlation between 3 variables "MonthlyCharges","TotalCharges", "tenure"
cor(cor_cols)
# correlation between TotalCharges and tenure is very high, value = 0.8256

# Need to remove any one of TotalCharges or tenure
# removing TotalCharges because of relatively high P-value

LR_model_4 <- glm(formula = Churn ~ SeniorCitizen + tenure + MonthlyCharges + 
                    MultipleLines.xYes + 
                    InternetService.xFiber.optic + InternetService.xNo + OnlineBackup.xYes + 
                    DeviceProtection.xYes + StreamingTV.xYes + StreamingMovies.xYes + 
                    Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                    PaymentMethod.xElectronic.check, family = binomial, data = train1)


summary(LR_model_4)
sort(vif(LR_model_4))

# All variables with VIF above 2 are significant as per p-value

# again check for collinearity between variables : MonthlyCharges, InternetService.xFiber.optic, InternetService.xNo

cor_cols1 <- train1[, colnames(train1) %in% c("MonthlyCharges","InternetService.xFiber.optic", "InternetService.xNo"), with = F]

#correlation values
cor(cor_cols1)

# MonthlyCharges is highly positive correlated with InternetService.xFiber.optic. value = 0.7836
# MonthlyCharges is highly negative correlated with InternetService.xNo. value = -0.7621

# p-value for MonthlyCharges is relatively less than InternetService.xFiber.optic. value and InternetService.xNo

# Removing MonthlyCharges based on multicollinearity and p-value

LR_model_5 <- glm(formula = Churn ~ SeniorCitizen + tenure + 
                    MultipleLines.xYes + 
                    InternetService.xFiber.optic + InternetService.xNo + OnlineBackup.xYes + 
                    DeviceProtection.xYes + StreamingTV.xYes + StreamingMovies.xYes + 
                    Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                    PaymentMethod.xElectronic.check, family = binomial, data = train1)


summary(LR_model_5)
sort(vif(LR_model_5))

# All variables VIF is below 2.
# Removing non significant variables based on low significance of p-value

# removing DeviceProtection.xYes

LR_model_6 <- glm(formula = Churn ~ SeniorCitizen + tenure + 
                    MultipleLines.xYes + 
                    InternetService.xFiber.optic + InternetService.xNo + OnlineBackup.xYes + 
                    StreamingTV.xYes + StreamingMovies.xYes + 
                    Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                    PaymentMethod.xElectronic.check, family = binomial, data = train1)


summary(LR_model_6)
sort(vif(LR_model_6))

# removing OnlineBackup.xYes

LR_model_7 <- glm(formula = Churn ~ SeniorCitizen + tenure + 
                    MultipleLines.xYes + 
                    InternetService.xFiber.optic + InternetService.xNo +  
                    StreamingTV.xYes + StreamingMovies.xYes + 
                    Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                    PaymentMethod.xElectronic.check, family = binomial, data = train1)


summary(LR_model_7)
sort(vif(LR_model_7))

# removing StreamingTV.xYes
LR_model_8 <- glm(formula = Churn ~ SeniorCitizen + tenure + 
                    MultipleLines.xYes + 
                    InternetService.xFiber.optic + InternetService.xNo +  
                    StreamingMovies.xYes + 
                    Contract.xOne.year + Contract.xTwo.year + PaperlessBilling + 
                    PaymentMethod.xElectronic.check, family = binomial, data = train1)


summary(LR_model_8)
sort(vif(LR_model_8))

# take model 8
model_final <- LR_model_8
#-----------------------
#### Model Evaluation
# c-statistic and KS -statistic
library(Hmisc)
#C- statistics--Training dataset ---------------------------------------------------
train1$predicted_prob = predict(model_final,  type = "response")
rcorr.cens(train1$predicted_prob,train$Churn) # 1st argument is your vector of predicted probabilities, 2nd observed values of outcome variable
# C Index : 8.424092e-01
# C- statistics-----test dataset--------------------------
test1$predicted_prob <- predict(model_final, newdata = test1 ,type = "response")
rcorr.cens(test1$predicted_prob,test$Churn)
# C Index : 8.375956e-01

# K S Statistic --Training dataset ---------------------------------------------------
library(ROCR)

model_score <- prediction(train1$predicted_prob ,train1$Churn)
model_perf <-  performance(model_score, "tpr", "fpr")
plot(model_perf)
ks_table <- attr(model_perf, "y.values")[[1]] - (attr(model_perf, "x.values")[[1]])

ks = max(ks_table)
ks # 0.5268551
which(ks_table == ks) # 854

ks_decile_train <- which(ks_table == ks)/nrow(train1)
ks_decile_train # 0.1732252

#--------KS Statistic -----test dataset--------------------------

test_model_score <- prediction(test1$predicted_prob ,test$Churn)
test_model_perf <-  performance(test_model_score, "tpr", "fpr")
test_ks_table <- attr(test_model_perf, "y.values")[[1]] - (attr(test_model_perf, "x.values")[[1]])
test_ks = max(test_ks_table)
test_ks # 0.5228858
which(test_ks_table == test_ks) # 600

ks_decile_test <- which(test_ks_table == test_ks)/nrow(test1) 
ks_decile_test # 0.2839565
plot(model_final)

View(test1)
# Selecting threshold value
# Predict probabilities of test data
predictions_data <- predict(model_final, newdata = test[,-24,with=F], type="response")
#Create a 'predictions' object using the predicted probabilities and the test labels
pred_object <- prediction(predictions_data, test$Churn)
# creating the ROC curve - creating a 'performance' object
performance_measures <- performance(pred_object, measure = "tpr", x.measure = "fpr")
plot(performance_measures)

# Area under the curve
auc <- performance(pred_object, measure = "auc")
auc <- unlist(auc@y.values)
auc # 0.8375956
# according to theory auc value 0.70 is a good sign

# ROC curve
plot(model_perf,col = "red", lab = c(10,10,10))

library(caret)
library(e1071)

# Train
confusionMatrix(as.numeric(train1$predicted_prob > 0.3), train1$Churn, positive = "1")
confusionMatrix(as.numeric(train1$predicted_prob > 0.5), train1$Churn, positive = "1")
confusionMatrix(as.numeric(train1$predicted_prob > 0.7), train1$Churn)




##--------------END OF LOGISTIC REGRESSION----------------------------------------------------------------------------------------


#---------------------------SVM--------------------------------------------------------------------------------------------


#Lets find the optimal cost using Cross Validation
train1<-train[,-1,with=F]
test1<-test[,-1,with=F]

#test1 remove the predictor column 
test1<-test1[,-24,with=F]

train1$Churn<-as.factor(train1$Churn)

tune.svm = tune(svm, Churn ~., data =train1, kernel = "linear", ranges = list(cost = c(0.001, 0.01, 0.1, 0.5, 1, 10, 100)))
summary(tune.svm)

#The best cost value is : 0.1
svmmodel<-svm(Churn~., data=train1,kernel="linear",cost =0.1, probability=TRUE) 

svmmodel

ypred = predict(svmmodel, test1,decision.values = T)
pred.probs<-attr(ypred,"decision.values")
#ypred probability assumes that Churn=0 is the positive class.. But, we want P(churn=1), hence 1-probability
pred.probs<-1-pred.probs

#ROC ANALYSIS
svmmodel.prediction<-prediction(pred.probs,test$Churn)
svmmodel.performance<-performance(svmmodel.prediction,"tpr","fpr")
plot(svmmodel.performance)
svmmodel.auc<-performance(svmmodel.prediction,"auc")@y.values[[1]]
svmmodel.auc #0.8213707

table(predicted = ypred, truth = test$Churn)
confusionMatrix(as.numeric(pred.probs > 0.3), test$Churn,positive ="1") #79.46 accuracy
confusionMatrix(as.numeric(pred.probs > 0.5), test$Churn,positive ="1")
confusionMatrix(as.numeric(pred.probs > 0.7), test$Churn,positive ="1")


#---------------------___END OF SVM------------------------------------------------------------------------------------