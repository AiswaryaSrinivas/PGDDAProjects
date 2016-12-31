

library(data.table)
library(ggplot2)
library(moments)
library(plotly)
library(gridExtra)

#Sys.setenv(JAVA_HOME="C:\\Program Files (x86)\\Java\\jre7\\")
#options(java.home="C:\\Program Files (x86)\\Java\\jre7\\")
#library(rJava)
#library(xlsx)

#install.packages("rJava")
#------------------Checkpoint-1: Data Cleaning and preparation------------------------

#Read the Loan Data
loans_dt <-fread("input/loan.csv",stringsAsFactors = F )
str(loans_dt)

#Extract only driver variables.
cols<-c("annual_inc","loan_amnt","funded_amnt","int_rate","grade","dti","emp_length","purpose","home_ownership","loan_status")
loans_dt<-loans_dt[,cols,with=F]
str(loans_dt)
summary(loans_dt)
#42542 observations and 10 variable

#Impute NA Values
#-------------------------------Missing Value Treatment------------------------------------------------#

# 1. loan status

sum(is.na(loans_dt$loan_status))
#there are no NA values for loan status
loans_dt$loan_status<-as.factor(loans_dt$loan_status)
summary(loans_dt$loan_status)
#there are 7 rows, where loan_status is empty. Let us see those observations.
loans_dt[loan_status==""]
#all of the columns are empty, so let us remove those observations
loans_dt <- loans_dt[loan_status!=""]

# 2. annual_inc

sum(is.na(loans_dt$annual_inc))
loans_dt[is.na(annual_inc)==T]
#There are 4 more NA vlaues in the annual_inc column
boxplot.stats(loans_dt$annual_inc)
loans_dt[is.na(annual_inc)==TRUE]
#all observations loan_status is Fully Paid. Do we really need to impute them??
#Lets see how we impute these null values
#What is the distribution of annual_inc?
boxplot(loans_dt$annual_inc)
length(boxplot.stats(loans_dt$annual_inc)$out)
#There are about 2032 outliers in our data.

nrow(loans_dt[annual_inc>2500000])
#There are only 2 observations with income is >2.5 million , let us remove those????
#Let us understand the distribution of annual_inc with respect to loan_status

p1 <- ggplot(loans_dt[annual_inc<2500000],aes(x=loan_status,annual_inc,fill=loan_status))+geom_boxplot()+xlab("")
pl <- p1+scale_x_discrete(labels = c("Fully Paid"="Fully Paid",
                                     "Charged Off"="Charged Off",
                                     "Current"="Current",
                                     "In Grace Period"="In Grace Period",
                                     "Late (31-120 days)"="Late (31-120 days)",
                                     "Late (16-30 days)"="Late (16-30 days)",
                                     "Default"="Default",
                                     "Does not meet the credit policy. Status:Fully Paid"="DMCP -Fully Pid",
                                     "Does not meet the credit policy. Status:Charged Off"="DMCP-Cahrged Off"
                                     ))
p1
#Other than for those with loan_status Late, all others have outliers in income data
summary(loans_dt$annual_inc)
#59000 is the median, while Mean is 69140

loans_dt[is.na(annual_inc),] 
#For all the missing value in annual_inc loan_status is "Does not meet the credit policy. Status:Fully Paid"

#Because data is skewed, replace the missing values by the median.
loans_dt = loans_dt[is.na(annual_inc),annual_inc:=median(loans_dt$annual_inc,na.rm = T)]
str(loans_dt)
sum(is.na(loans_dt$annual_inc))

sum(is.na(loans_dt))
#There no more NAs in data set.


View(loans_dt)
# There are few values with n/a in emp_length
loans_dt$emp_length <- as.factor(loans_dt$emp_length)
summary(loans_dt$emp_length)

#There are 1112 observations with emp_length as n/a - That could be self employeed or business people. So better consider
# them as seperate category and proceed

loans_dt[emp_length=="n/a",(.N),by=loan_status]
#805 observations have loaan status charged off, but remaining 288 have other loan_status.
#We will not remove these observations and consider n/a as another category of emp_length

############-----End of Missing Value Treatment----------------------------------_###################


#----------------Data Prepartion on Loan Status------------------------#

#----------------Loan Status------------------------#

#Change Does Not Meet Credit Policy : Charged Off to Charged off and Does not meet Credit Policy:Fully Paid to Fully Paid
loans_dt[loan_status=="Does not meet the credit policy. Status:Charged Off",loan_status:="Charged Off"]
loans_dt[loan_status=="Does not meet the credit policy. Status:Fully Paid",loan_status:="Fully Paid"]

#for our analysis we dont need observations with "fully paid" lets remove them.
loans_dt <- loans_dt[loan_status!="Fully Paid"]


#Drop unused Factor Levels
#loans_dt$loan_status<-factor(loans_dt$loan_status)#This drops the unused factor levels

status_group_index <- c("Current","In Grace Period","Default","Charged Off","Late (16-30 days)","Late (31-120 days)")
status_group_Values <- c("Current_New","Current_New","Default_New","Default_New","Late","Late")
loans_dt$loan_status_1 <- status_group_Values[match(loans_dt$loan_status,status_group_index)]

#Create a column loan_status_1.
#system.time(loans_dt$loan_status_1<-ifelse((loans_dt$loan_status=="Current" | loans_dt$loan_status=="In Grace Period"),"Current_New",
 #                             ifelse((loans_dt$loan_status=="Default"|loans_dt$loan_status=="Charged Off"),"Default_New",
#                                     "Late")))
#loans_dt$loan_status_1<-as.factor(loans_dt$loan_status_1)

#-----------------------------Binning Intrest Rates-------------------------------------------#

#Lets remove the % symbol from each value and make it numeric
loans_dt[,int_rate:=gsub("%","",int_rate)]
loans_dt$int_rate=as.numeric(loans_dt$int_rate)

#Bin Intrest Rates and create new column int_rate_grp.
loans_dt$int_rate_grp=ifelse(loans_dt$int_rate<10,"Low",ifelse((loans_dt$int_rate>=10 & loans_dt$int_rate<=18),"Medium","High"))

#Ordering the factor levels
loans_dt$int_rate_grp<-factor(loans_dt$int_rate_grp,levels = c("Low","Medium","High"))

table(loans_dt$int_rate_grp)

#------------------------Binning Emp_Length----------------------------------------#
employeeGroup<-function(emp_len){
  group<-c("n/a","Junior","Mid-level","Senior")
  if((emp_len=="1 year"|emp_len=="2 years"|emp_len=="3 years"|emp_len=="4 years"|emp_len=="< 1 year")){
    return("Junior")
  }else if((emp_len=="5 years"|emp_len=="6 years"|emp_len=="7 years"|emp_len=="8 years")){
    return("Mid-level")
  }else if((emp_len=="9 years"|emp_len=="10+ years")){
    return("Senior")
  }else{
    return("n/a")
  }
}

loans_dt$emp_len_group<-factor(sapply(loans_dt$emp_length,employeeGroup),levels=c("n/a","Junior","Mid-level","Senior"))
table(loans_dt$emp_len_group)

#let us export this cleaned data to xlsx for Tableau analysis.
#write.xlsx(loans_dt, "output/data/loan_ds_cleaned.xlsx")
#write.table(loans_dt, "output/data/loan_ds_cleaned.csv", sep=",")
#------------------------------End of Data Preparation---------------------------------------#


#-----------------------------CHECKPOINT 2 : Exploratory Data Analysis----------------------------------------------------------------------#

#-----------------------UNIVARIATE ANALYSIS----------------------------------------------#

#------------------------1.Annual Income-------------------------------------------------#

#Summary Stats
summary(loans_dt$annual_inc) # Looks like data is skewed.

#boxplot(loans_dt$annual_inc)
max(loans_dt$annual_inc) #1250000
quantile(loans_dt$annual_inc,probs =c(0.75,0.90,0.95,0.99,0.995,0.996,0.997,0.998,0.999))

#99% of the annual income is less than  2.35 lakh #234040
#99.9 % of the data lies below in 6.3 lakh
#99.6% lies below 325000

#Distribution Plots

#99.6 % of the the values are below 360000, so for better visualation lets take the data only <=350000
income_hist_plot <- ggplot(loans_dt[annual_inc<325000],aes(x=annual_inc))
income_hist_plot <- income_hist_plot+geom_histogram(aes(y=..count..),binwidth = 1000, colour="royalblue4", fill="lightblue")
income_hist_plot <- income_hist_plot+geom_density(aes(y = ..count..*1000),alpha=.1, fill="blue") 
income_hist_plot <- income_hist_plot+labs(x="Annual Income")+ggtitle("Distribution of Annual Income")
income_hist_plot<- income_hist_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
income_hist_plot <- income_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
income_hist_plot <- income_hist_plot+geom_vline(aes(xintercept=mean(annual_inc)),color="brown4", linetype="dashed", size=1.4)
income_hist_plot
#From the plot, we can say it is possitively skewed ( having right long tail)

#Lets see the skewness and kurtosis
skewness(loans_dt$annual_inc) #6.3 
#highly skweed

kurtosis(loans_dt$annual_inc) #89.81

#box plot
income_box_plot <- ggplot(loans_dt,aes(x=annual_inc,y=annual_inc))+geom_boxplot(varwidth = F, outlier.colour="skyblue4", outlier.shape=16,outlier.size=2,fill="lightblue")
income_box_plot <- income_box_plot+ coord_flip()+scale_x_continuous(limits = c(0, 1500000))+stat_boxplot(geom ='errorbar',width=0.5)
income_box_plot <- income_box_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
income_box_plot <- income_box_plot+ggtitle("Boxplot of Annual Income")
income_box_plot

#we could see so many outliers, let us try to impute them
#outlier treatment

length(boxplot.stats(loans_dt$annual_inc)$out) 
# there are 404 outliers

lower_whisker <- boxplot(loans_dt$annual_inc)$stats[1,]
nrow(loans_dt[annual_inc <lower_whisker])
#there are 0 records below the lower whisker


quantile(loans_dt$annual_inc,probs = 0.05)#6798
nrow(loans_dt[annual_inc<=22000]) 
# there are 382 records having annual income less than 5% percentail (=6798) 

loans_dt[annual_inc<=22000,(.N),by=loan_status_1]
#Iteresting! 96.3% of the people whose income less than 22000 are  Defaulters

quantile(loans_dt$annual_inc,probs =0.1) #27600
loans_dt[annual_inc<=27600,(.N),by=loan_status_1]
#Interesting! 94 % of the people whose income is less than 27600 are Defaulters

quantile(loans_dt$annual_inc,probs =0.2) #36000
loans_dt[annual_inc<=36000,(.N),by=loan_status_1]
# Again Interesting! 92 % of the people whose income is less than 36000 are Defaulters

#Plot to show relation between income and defaulters

upper_fence <- boxplot(loans_dt$annual_inc)$stats[5,]
upper_fence
nrow(loans_dt[annual_inc >upper_fence])
#there are 404  records above the upper whisker, lets us see their distribution
loans_dt[annual_inc>upper_fence,(.N),by=loan_status_1]
#75% of those outliers are either Defaulters or Late payers, very much biased
#Better not to remove these observations, it will be useful for our analysis 
# But if we don't remove mean will get skwed and our hypothesis testing will be impacted.
#So , let us keep those records but replace them by upper whisker

#Before that let us do Further Analysis

quantile(loans_dt$annual_inc,probs =c(0.001,0.01,0.05,0.1,0.15,0.20,0.25,0.7,0.8,0.90,0.95,0.96,0.97,0.98,0.99,0.995,0.996,0.997,0.998,0.999))


# 99 % of the population are below 234040
loans_dt[annual_inc>234040,(.N),by=loan_status_1]

#interesting agian! 80% of people whose income is grater than 234040, are defaulters

loans_dt[annual_inc>70000,(.N),by=loan_status_1]
#Interesting, about 84% of the people are Defaulters whose income is greater than 70000

quantile(loans_dt$annual_inc,probs =c(0.95,0.96,0.97,0.98,0.99,0.995,0.996,0.997,0.998,0.999))

#99.6% of values are under 350000,
nrow(loans_dt[annual_inc>=350000])
#there are 29 observations whose income >= 350000, let us see.
loans_dt[annual_inc>350000,(.N),by=loan_status_1]
loans_dt[annual_inc>184000,(.N),by=loan_status_1]


#let us replace outliers with upper fence value now
loans_dt[annual_inc>136000]$annual_inc <- 136000
mean(loans_dt$annual_inc)
length(boxplot.stats(loans_dt$annual_inc)$out) 



#-----------------End of Annual Income-------------------------------------------------#

#-----------------2.Loan Amount-------------------------------------------------#
#Summary Statistics
summary(loans_dt$loan_amnt)

skewness(loans_dt$loan_amnt) #0.83
kurtosis(loans_dt$loan_amnt) # 3.06, slightly more than 3
qqnorm(loans_dt$loan_amnt)

#Distribution plot
loan_amt_hist_plot <- ggplot(loans_dt,aes(x=loan_amnt))
loan_amt_hist_plot <- loan_amt_hist_plot+geom_histogram(aes(y=..count..),binwidth = 1000, colour="royalblue4", fill="lightblue")
loan_amt_hist_plot <- loan_amt_hist_plot+geom_density(aes(y = ..count..*1000),alpha=.1, fill="blue") 
loan_amt_hist_plot <- loan_amt_hist_plot+labs(x="Loan Amount")+ggtitle("Distribution of Loan Amount")
loan_amt_hist_plot<- loan_amt_hist_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
loan_amt_hist_plot <- loan_amt_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
loan_amt_hist_plot <- loan_amt_hist_plot+geom_vline(aes(xintercept=mean(loan_amnt)),color="blue", linetype="dashed", size=1)
loan_amt_hist_plot <- loan_amt_hist_plot+annotate("text", x = 11500, y= 800, label = "Mean", colour="darkred",size=5)
loan_amt_hist_plot
#possitively skeweed

#box plot
loan_amt_box_plot <- ggplot(loans_dt,aes(x=loan_amnt,y=loan_amnt))+geom_boxplot(varwidth = F, outlier.colour="skyblue4", outlier.shape=16,outlier.size=2,fill="lightblue")
loan_amt_box_plot <- loan_amt_box_plot+ coord_flip()+scale_x_continuous(limits = c(0, 60000))+scale_y_continuous(limits = c(0, 40000))+stat_boxplot(geom ='errorbar',width=0.5)
loan_amt_box_plot <- loan_amt_box_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
loan_amt_box_plot <- loan_amt_box_plot+ggtitle("Boxplot of Loan Amount")
loan_amt_box_plot

#Outlier treatment
length(boxplot.stats(loans_dt$loan_amnt)$out)
#there are NO outliers 

#Observations
#possitively skweed,  
quantile(loans_dt$loan_amnt,probs =c(0.001,0.01,0.05,0.1,0.15,0.20,0.25,0.7,0.8,0.90,0.95,0.96,0.97,0.98,0.99))

#80% of loan amount is less than or equal to 20000
#90% of the loan amount is less than or equal to 25000
#there are not many observations between 25000 and 35000,but many observations at 35000 

#-----------------End of Loan Amount-------------------------------------------------#

#-----------------3.Funded Amount-------------------------------------------------#
#Summary Statistics
summary(loans_dt$funded_amnt)

skewness(loans_dt$funded_amnt) #0.863
kurtosis(loans_dt$funded_amnt) # 3.23 slightly more than 3, little fat tail

#Distribution plot
funded_amt_hist_plot <- ggplot(loans_dt,aes(x=funded_amnt))
funded_amt_hist_plot <- funded_amt_hist_plot+geom_histogram(aes(y=..count..),binwidth = 1000, colour="royalblue4", fill="lightblue")
funded_amt_hist_plot <- funded_amt_hist_plot+geom_density(aes(y = ..count..*1000),alpha=.1, fill="blue") 
funded_amt_hist_plot <- funded_amt_hist_plot+labs(x="Funded Amount")+ggtitle("Distribution of Funded Amount")
funded_amt_hist_plot<- funded_amt_hist_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
funded_amt_hist_plot <- funded_amt_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
funded_amt_hist_plot <- funded_amt_hist_plot+geom_vline(aes(xintercept=mean(funded_amnt)),color="blue", linetype="dashed", size=1)
funded_amt_hist_plot <- funded_amt_hist_plot+annotate("text", x = 12000, y= 800, label = "Mean", colour="darkred",size=5)
funded_amt_hist_plot
#lightly possitively skeweed

#box plot
funded_amt_box_plot <- ggplot(loans_dt,aes(x=funded_amnt,y=funded_amnt))+geom_boxplot(varwidth = F, outlier.colour="skyblue4", outlier.shape=16,outlier.size=2,fill="lightblue")
funded_amt_box_plot <- funded_amt_box_plot+ coord_flip()+scale_x_continuous(limits = c(0, 60000))+scale_y_continuous(limits = c(0, 40000))+stat_boxplot(geom ='errorbar',width=0.5)
funded_amt_box_plot <- funded_amt_box_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
funded_amt_box_plot <- funded_amt_box_plot+ggtitle("Funded Amount Box Plot")
funded_amt_box_plot

#Outlier treatment
length(boxplot.stats(loans_dt$funded_amnt)$out)
#there are 196 outliers (above the upper fence)

#lets see how the outliers are distributed
funded_amt_upper_whisker <- boxplot(loans_dt$funded_amnt)$stats[5,]
loans_dt[funded_amnt>funded_amt_upper_whisker,(.N),by=loan_status_1]

## ?? shall we set to upper whisker? 


#-----------------End of Funded Amount-------------------------------------------------#

#-----------------4.Intrest Rate Groups-------------------------------------------------#
#Summary Statistics
summary(loans_dt$int_rate_grp)

#Proportions of different intrest group in the data
table(loans_dt$int_rate_grp)*100/nrow(loans_dt)
#about 74% of the loans, have medium intrest rates.

#Distribution plot
int_rate_group_bar_plot <- ggplot(loans_dt,aes(x=int_rate_grp,fill=int_rate_grp))+geom_bar(position = "stack", width = .4)
int_rate_group_bar_plot <- int_rate_group_bar_plot+ggtitle("Distribution of Intrest Rate Groups")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
int_rate_group_bar_plot <- int_rate_group_bar_plot+xlab("Interest Rate Group")+ylab("Number of Observations")+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))
int_rate_group_bar_plot <- int_rate_group_bar_plot+geom_text(aes(label = ..count..),stat= "count",position=position_dodge(.9),vjust = -0.3)
int_rate_group_bar_plot

#Outlier treatment

# there are more than 10% observations in all 3 categories, so NO Outliers.


#-----------------End of Intrest Rate Groups-------------------------------------------------#

#--------------------------------5. Grade--------------------------------------------------------#
#Summary Statistics

loans_dt$grade <- as.factor(loans_dt$grade)
summary(loans_dt$grade)

#Proportions of different intrest group in the data
table(loans_dt$grade)*100/nrow(loans_dt)
#about 74% of the loans, have medium intrest rates.

level_grade=levels(loans_dt$grade)
level_grade<-level_grade[level_grade!=""]
level_grade
loans_dt$grade<-factor(loans_dt$grade,levels = level_grade)

#Distribution plot
grade_bar_plot <- ggplot(loans_dt,aes(x=grade))+geom_bar(position = "stack", width = .4,fill="slateblue4")
grade_bar_plot <- grade_bar_plot+ggtitle("Distribution of Intrest Rate Groups")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
grade_bar_plot <- grade_bar_plot+xlab("Interest Rate Group")+ylab("Number of Observations")
grade_bar_plot <- grade_bar_plot+geom_text(aes(label = ..count..),stat= "count",position=position_dodge(.9),vjust=-0.2)
grade_bar_plot

#outlier treatement 
#There are no outliers as all the categories are having more than 5%

#-----------------------End of Grade-------------------------------------------------------------#

#-------------------------6.DTI--------------------------------------------------------------#
#Summary Statistics
summary(loans_dt$dti)
skewness(loans_dt$dti) #-0.149, negative skewness
kurtosis(loans_dt$dti)#2.211
qqnorm(loans_dt$dti,col="lightblue")
qqline(loans_dt$dti,col="red")

#Distribution plot
dti_hist_plot <- ggplot(loans_dt,aes(x=dti))
dti_hist_plot <- dti_hist_plot+geom_histogram(aes(y=..count..),binwidth = 1, colour="royalblue4", fill="lightblue")
dti_hist_plot <- dti_hist_plot+geom_density(aes(y = ..count..),alpha=.1, fill="blue") 
dti_hist_plot <- dti_hist_plot+labs(x="Funded Amount")+ggtitle("Distribution of DTI")
dti_hist_plot <- dti_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dti_hist_plot <- dti_hist_plot+geom_vline(aes(xintercept=mean(dti)),color="blue", linetype="dashed", size=1)
dti_hist_plot <- dti_hist_plot+annotate("text", x = 14, y= 500, label = "Mean", colour="darkred",size=5)
dti_hist_plot

#boxplots
dit_box_plot <- ggplot(loans_dt,aes(x=dti,y=dti))+geom_boxplot(varwidth = F, outlier.colour="skyblue4", outlier.shape=16,outlier.size=2,fill="lightblue")
dit_box_plot <- dit_box_plot+ coord_flip()+scale_x_continuous(limits = c(-10, 40))+scale_y_continuous(limits = c(-10, 40))+stat_boxplot(geom ='errorbar',width=0.5)
dit_box_plot <- dit_box_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dit_box_plot <- dit_box_plot+ggtitle("Funded Amount Box Plot")
dit_box_plot

boxplot(loan$dti)
length(boxplot.stats(loans_dt$dti)$out)
#there are no outliers in the dti.

#------------------------End of DTI---------------------------------------------------------#


#------------------7.Purpose-------------------------------------------------------------

#Removing the empty level in purpose.
loans_dt$purpose <- as.factor(loans_dt$purpose)
levels_purpose<-levels(loans_dt$purpose)
levels_purpose<-levels_purpose[levels_purpose!=""]
loans_dt$purpose<-factor(loans_dt$purpose,levels = levels_purpose)

summary(loans_dt$purpose)
table(loans_dt$purpose)*100/nrow(loans_dt)

#Distribution plot
purpose_bar_plot <- ggplot(loans_dt,aes(x=factor(purpose)))+geom_bar(position = "stack", width = .7,fill="dodgerblue4")
purpose_bar_plot <- purpose_bar_plot+ggtitle("Distribution of Intrest Rate Groups")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
purpose_bar_plot <- purpose_bar_plot+xlab("Interest Rate Group")+ylab("Number of Observations")
purpose_bar_plot <- purpose_bar_plot+geom_text(aes(label = ..count..),stat= "count",position=position_dodge(.9),hjust=0.01)+coord_flip()
purpose_bar_plot

#debt consolidations, contributes to 48% of the prrpose for which loan in taken

loans_dt[purpose=="debt_consolidation",(.N),by=loan_status_1]
#84% of the loans taken for debt_consolidation are Defaulted!!!

totalDefulters <- nrow(loans_dt[loan_status_1=="Default_New"])
loans_dt[loan_status_1=="Default_New",round((.N)*100/totalDefulters),purpose][order(V1,decreasing = T)]

# 48% of the Defaulters have taken loan for the purpose " debt_consolidation" 

#---------------------------End of Purpose----------------------------------------------------------------

#--------------------------------8.Home Ownership-------------------------------------------------------------------------#
loans_dt$home_ownership <- as.factor(loans_dt$home_ownership)
summary(loans_dt$home_ownership)
levels_home_ownership<-levels(loans_dt$home_ownership)[levels(loans_dt$home_ownership)!=""]
loans_dt$home_ownership<-factor(loans_dt$home_ownership,levels=levels_home_ownership)

table(loans_dt$home_ownership)*100/nrow(loans_dt)
#44% of the people who have applied for loan have mortaged their homes
#about48% of the people who have taken loan are staying in Rented house

#Distribution
ownhouse_bar_plot <- ggplot(loans_dt,aes(x=factor(home_ownership)))+geom_bar(position = "stack", width = .5,fill="dodgerblue4")
ownhouse_bar_plot <- ownhouse_bar_plot+ggtitle("Distribution of Home Ownership Status")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ownhouse_bar_plot <- ownhouse_bar_plot+xlab("Home Ownership Status of Loan Applicants")+ylab("Number of Observations")
ownhouse_bar_plot <- ownhouse_bar_plot+geom_text(aes(label = ..count..),stat= "count",position=position_dodge(.9),vjust=-0.2)
ownhouse_bar_plot


loans_dt[home_ownership=="RENT",(.N),by=loan_status_1]
#84% of the loans taken for debt_consolidation are Defaulted!!!
##Interesting observation. 88% of the people who are staying in Rented house are either defaulters


totalDefulters <- nrow(loans_dt[loan_status_1=="Default_New"])
loans_dt[loan_status_1=="Default_New",round((.N)*100/totalDefulters),home_ownership][order(V1,decreasing = T)]

# 50% of the Defaulters are staying in Reted house
# 42% of the Defaulters mortgaged thier home


#-------------------------------------End of home ownership-------------------------------------------------------------------#


#-----------------------------------9. Emp_len_grp-----------------------------------------------------------------#

#Summary Statistics 
summary(loans_dt$emp_len_group)
table(loans_dt$emp_len_group)*100/nrow(loans_dt)
#46.6% of the employess who applied for loan  are in Junior Level

#Distribution
emp_grp_bar_plot <- ggplot(loans_dt,aes(x=factor(emp_len_group)))+geom_bar(position = "stack", width = .3,fill="dodgerblue4")
emp_grp_bar_plot <- emp_grp_bar_plot+ggtitle("Distribution of Emp Length Group")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
emp_grp_bar_plot <- emp_grp_bar_plot+xlab("Employeemnt Length Group Distribution")+ylab("Number of Observations")
emp_grp_bar_plot <- emp_grp_bar_plot+geom_text(aes(label = ..count..),stat= "count",position=position_dodge(.9),vjust=-0.3)
emp_grp_bar_plot


#Distribution
emp_grp_bar_plot1 <- ggplot(loans_dt,aes(x=emp_len_group,fill=loan_status_1))+geom_bar(position = "dodge", width = .3)+scale_fill_manual(values=alpha(c("orchid2","slateblue3","blue4"),.8))
emp_grp_bar_plot1 <- emp_grp_bar_plot1+ggtitle("Distribution of Emp Length Group")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
emp_grp_bar_plot1 <- emp_grp_bar_plot1+xlab("Employeemnt Length Group Distribution")+ylab("Number of Observations")
emp_grp_bar_plot1 <- emp_grp_bar_plot1+geom_text(aes(label = ..count..),stat= "count",position = position_dodge(0.3),vjust=-0.3)
emp_grp_bar_plot1

#Analysis
loans_dt[emp_len_group=="Junior",(.N),by=loan_status_1]
#87.7% of the Junior employment group are Defaulted!!!

totalDefulters <- nrow(loans_dt[loan_status_1=="Default_New"])
loans_dt[loan_status_1=="Default_New",round((.N)*100/totalDefulters),emp_len_group][order(V1,decreasing = T)]
# 48% of the Defaulters are having junior emp lenth

#--------------------------------_End of emp_len_grp-----------------------------------------------------------------#


#---------------------------------10.loan_status_1---------------------------------------------------------------------#
loans_dt$loan_status_1 <- as.factor(loans_dt$loan_status_1)
summary(loans_dt$loan_status_1)
table(loans_dt$loan_status_1)*100/nrow(loans_dt)

#84% of the loan applicant are new defaulters.


#Distribution plot
loan_status_bar_plot <- ggplot(loans_dt,aes(x=loan_status_1,fill=loan_status_1))+geom_bar(position = "stack", width = .4)
loan_status_bar_plot <- loan_status_bar_plot+ggtitle("Distribution of Loan Status of Active accounts")+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
loan_status_bar_plot <- loan_status_bar_plot+xlab("Loan Status")+ylab("Number of Observations")+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))
loan_status_bar_plot <- loan_status_bar_plot+geom_text(aes(label = ..count..),stat= "count",position=position_dodge(.9),vjust = -0.3)
loan_status_bar_plot



#----------------------------End of loan_status_1----------------------------------------------------------

#-----------------------END UNIVARIATE ANALYSIS----------------------------------------------#


#-----------------------------------------------------------###
#Distribution Plots of all continuous variable FOR PPT
grid.arrange(income_hist_plot,loan_amt_hist_plot,dti_hist_plot,funded_amt_hist_plot,ncol=2)
grid.arrange(purpose_bar_plot,int_rate_group_bar_plot,emp_grp_bar_plot,loan_status_bar_plot,ncol=2)


#-----------------------BEGIN MULTIVARIATE ANALYSIS----------------------------------------------#

#-----------------Corelation between Pairs of Continuous Variables---------------------------------#

#continuous variables are annual_inc,loan_amnt,funded_amnt,dti
continuous_cols<-c("annual_inc","loan_amnt","funded_amnt","dti")

#lets consider log_annual_income instead of annual income to calculate corelation matrix
loan_continuous<-loans_dt[,continuous_cols,with=FALSE]
head(loan_continuous)
#loan_continuous$annual_inc<-NULL
#let us see the corelation metrix
cor(loan_continuous)
#From the cor matrix we can see that there is 0.97 correlation between loan amount and funded amount.

#let us see them visually
library(GGally)
ggpairs(loan_continuous)


#We can observe that the is a very high correlation between loan_amnt and funded_amnt.which means that almost all the times 
#full of requested loan amount has been funded 

#-------------------End of Correlation between Pairs of Continuous Variables--------------------------------------------------#

#----------------------------Multivariate Analysis of all Driver Variables with Loan_Status_1 and Int_Rate_Grp--------------------------------------------#

#----------------------------Multivariate Analysis of all Driver Variables with Loan_Status_1 and Int_Rate_Grp--------------------------------------------#

#---------1.1 loan_status_1 vs annual_inc-------------------------------------------------------
#ggplot(loan,aes(x=loan_status_1,y=log(annual_inc),fill=loan_status_1))+geom_boxplot()
status_income_plot<-ggplot(loans_dt,aes(x=loan_status_1, y=annual_inc, col= loan_status_1)) +geom_violin(aes(y=annual_inc))+ stat_summary(fun.y = "mean", geom= "point")+scale_colour_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8)) +stat_summary(fun.y = "median", geom = "point", col="black", shape = "M")
status_income_plot<-status_income_plot+ggtitle("Loan Status vs Income")+xlab("Loan Status")+ylab("Annual Income")
status_income_plot

#We can see that for Defaulters are more for Lower Income Ranges.
#Also, for very low income ranges, there seems to be 100% Default Rate.

#The median income on New Default is lower than there Current_New and Late Loan Status Group


#--------End of loan_status_1 vs annual_inc-----------------------------------------------------

#---------1.2 int_rate_grp vs annual_inc--------------------------------------------------------
#ggplot(loan,aes(x=int_rate_grp,y=log(annual_inc),fill=int_rate_grp))+geom_boxplot()
#The median income of High Intrest Rate Groups is higher than the rest of the intrest group
int_income_plot<-ggplot(loans_dt, aes(annual_inc, fill = int_rate_grp)) + geom_density() + facet_grid(int_rate_grp ~ .)+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))
int_income_plot<-int_income_plot+ggtitle("Annual Income vs Interest Rate")+xlab("Annual Income")
int_income_plot<-int_income_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
int_income_plot

#-------End of int_rate_grp vs annual_inc--------------------------------------------------------

#---------2.1 loan_status_1 vs loan_amnt---------------------------------------------------------

#There is a significant difference n median loan amount of Default_new and other categories.
#There are a few large loan amount in default_new
status_loan_amnt<-ggplot(loans_dt,aes(x=loan_status_1, y=loan_amnt, col= loan_status_1)) +geom_violin(aes(y=loan_amnt))+ stat_summary(fun.y = "mean", geom= "point")+scale_colour_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8)) +stat_summary(fun.y = "median", geom = "point", col="black", shape = "M")
status_loan_amnt<-status_loan_amnt+ggtitle("Loan Status vs Loan Amount")+xlab("Loan Status")+ylab("Loan Amount")
status_loan_amnt

#We can observe that For Low Loan Amounts, the Density of Defaulters in very high compared to Late and Cirrent
#Also, the median loan amount of defaulters is lower than Other Groups

#ggplot(loan,aes(x=loan_amnt,fill=loan_status_1))+geom_histogram()

#----------End of loan_status_1 vs loan_amnt----------------------------------------------------

#-----------2.2 int_rate_grp vs loan_amnt------------------------------------------------------
#ggplot(loan,aes(x=int_rate_grp,y=loan_amnt,fill=int_rate_grp))+geom_boxplot()


int_loan_amnt<-ggplot(loans_dt, aes(loan_amnt, fill = int_rate_grp)) + geom_density()+ facet_grid(int_rate_grp~ .)+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))
int_loan_amnt<-int_loan_amnt+ggtitle("Loan Amount vs Interest Rate")+xlab("Loan Amount")
int_loan_amnt <- int_loan_amnt + scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
int_loan_amnt

#The meding Loan_Amnt for High intrest Rate Groups is Much higher compares to lower and medium intrest grous
#Also, the median loan_amnt for Low<Medium<High intrest group

#ggplot(loan,aes(x=loan_amnt,color=int_rate_grp))+geom_density()
#------------------End of int_rate_grp vs loan_amnt---------------------------------------------


#---------3.1 loan_status_1 vs funded_amnt---------------------------------------------------------

#ggplot(loans_dt,aes(x=loan_status_1,y=funded_amnt,fill=loan_status_1))+geom_boxplot()
#The distribution of funded_amnt vs loans_Status_1 is similar to that of loan_status_1 vs loan_amnt

#ggplot(loans_dt,aes(x=funded_amnt,fill=loan_status_1))+geom_histogram(position="dodge")

status_funded_amnt<-ggplot(loans_dt,aes(x=loan_status_1, y=funded_amnt, col= loan_status_1)) +geom_violin(aes(y=funded_amnt))+ stat_summary(fun.y = "mean", geom= "point")+scale_colour_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8)) +stat_summary(fun.y = "median", geom = "point", col="black", shape = "M")
status_funded_amnt<-status_funded_amnt+ggtitle("Loan Status vs Funded Amount")+xlab("Loan Status")+ylab("Funded Amount")
status_funded_amnt

#-------------End of loan_status_1 vs funded_amnth-----------------------------------------------

#-----------3.2 int_rate_grp vs funded_amnt-----------------------------------------------------
int_funded_amnt<-ggplot(loans_dt,aes(x=funded_amnt,fill=int_rate_grp))+geom_density()+facet_grid(int_rate_grp~.)+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))
int_funded_amnt<-int_funded_amnt+ggtitle("Funded Amunt vs Interest Rate")+xlab("Funded Amount")
int_funded_amnt

#The meding Funded_Amnt for High intrest Rate Groups is Much higher compares to lower and medium intrest grous
#Also, the median loan_amnt for Low<Medium<High intrest group
#Skeweness of Low intrest groups > Medium >High

#---------------End of int_rate_grp vs funded_amnt------------------------------------------------

#-------------4.1 loan_status_1 vs grade---------------------------------------------------------

#both are categorical variables, and hence we will plot a bar chart
status_grade<-ggplot(loans_dt,aes(x=grade,fill=loan_status_1))+geom_bar(aes(y=(..count..)*100/sum(..count..)),position="dodge")+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))+ylab("Percentage")
status_grade<-status_grade+ggtitle("Grade vs Loan Status")
status_grade
#Highest proportions of defaulters are in grade A,followed by Grade G,
 
#----------------End of loan_status_1 vs grade---------------------------------------------------


#------------------5.1 loan_status_1 vs dti-----------------------------------------------------

status_dti_plot<-ggplot(loans_dt,aes(x=loan_status_1, y=dti, col= loan_status_1)) +geom_violin(aes(y=dti))+ stat_summary(fun.y = "mean", geom= "point")+scale_colour_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8)) +stat_summary(fun.y = "median", geom = "point", col="black", shape = "M")
status_dti_plot<-status_dti_plot+ggtitle("Loan Status vs DTI")+xlab("Loan Status")
status_dti_plot

#The median dti of default_new is slightly lower than current_new.
#Looks like DTI vs Loan Status is almost normal. As Mean and Medina are overlapping.


#----------------------End of loan_status_1 vs dti----------------------------------------------

#-------------------5.2 int_rate_grp vs dti------------------------------------------------------
#ggplot(loan,aes(x=int_rate_grp,y=dti,fill=int_rate_grp))+geom_boxplot()
#The median dti of Lo Intrest group is lower compared to Medium and High

#ggplot(loan,aes(x=dti,fill=int_rate_grp))+geom_histogram()

int_dti_plot<-ggplot(loans_dt,aes(x=dti,fill=int_rate_grp))+geom_density()+facet_grid(int_rate_grp~.)+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))
int_dti_plot<-int_dti_plot+ggtitle("DTI vs Interest Group")
int_dti_plot

#------------------End of int_rate_grp vs dti----------------------------------------------------

#-----------------6.1 loan_status_1 vs purpose--------------------------------------------------
#ggplot(loan,aes(x=purpose,fill=loan_status_1))+geom_bar(position="fill")+coord_flip()

status_purpose<-ggplot(loans_dt,aes(x=purpose,fill=loan_status_1))+geom_bar(aes(y=(..count..)/sum(..count..)),position="fill")+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))+ylab("Percentage")+coord_flip()
status_purpose<-status_purpose+ggtitle("Purpose vs Loan Status")
status_purpose

#The most loans are for dbt consolidation.
#The highest default_new is for educational loans
#100% of the loans, which are taken for educational purposes are defaulted.

#-----------------End of loan_status_1 vs purpose----------------------------------------------

#-----------------6.2 int_rate_grp vs purpose-------------------------------------------------
int_purpose<-ggplot(loans_dt,aes(x=purpose,fill=int_rate_grp))+geom_bar()+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))+coord_flip()
int_purpose<-int_purpose+ggtitle("Purpose vs Intrest Rate")
int_purpose

#---------------End of int_rate_grp vs purpose-----------------------------------------------

#------------7.1 loan_status_1 vs homeownership----------------------------------------------
status_home_plot<-ggplot(loans_dt,aes(x=home_ownership,fill=loan_status_1))+geom_bar(position="fill")+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))
status_home_plot<-status_home_plot+xlab("Home Ownership")+ggtitle("Home Ownership vs Loan Status")
status_home_plot
#None and OTher Home_ownership type have 100% Default_Neew
#Percentage of Defaulters for Mortgaged house is less amoung Own,Rent and Mortgage

#---------End of loan_status_1 vs homwownership----------------------------------------------

#------------7.2 int_rate_grp vs homeownership-----------------------------------------------

int_home_plot<-ggplot(loans_dt,aes(x=home_ownership,fill=int_rate_grp))+geom_bar(position="fill")+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))
int_home_plot<-int_home_plot+xlab("Home Ownership")+ggtitle("Home Ownership vs Intrest Rate Group")
int_home_plot

#------------End of int_rate_group vs homeownership------------------------------------------

#------------8.1 loan_status_1 vs emp_len_grp------------------------------------------------
status_emp_plot<-ggplot(loans_dt,aes(x=emp_len_group,fill=loan_status_1))+geom_bar(position="dodge")+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))
status_emp_plot<-status_emp_plot+xlab("Employee Length Group")+ggtitle("Employee Length Group vs Loan Status")
status_emp_plot
#-----------End of loan_status_1 vs emp_len_grp----------------------------------------------

#-------------8.2 int_rate_grp vs emp_len_grp-----------------------------------------------

int_emp_plot<-ggplot(loans_dt,aes(x=emp_len_group,fill=int_rate_grp))+geom_bar(position="dodge")+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))
int_emp_plot<-int_emp_plot+xlab("Employee Length Group")+ggtitle("EmployeeLength Group vs Intrest Group")
int_emp_plot
#--------------------End of int_rate_grp vs emp_len_grp--------------------------------------

#---------------9 int_rate_grp vs loan_status_1-----------------------------------------------

intrest_loan_status_plot<-ggplot(loans_dt,aes(x=int_rate_grp,fill=loan_status_1))+geom_bar(position = "fill")+scale_fill_manual(values=alpha(c("orchid2", "slateblue3","blue4"),.8))
intrest_loan_status_plot<-intrest_loan_status_plot+xlab("Intrest Rate Group")+ggtitle("Loan Status vs Intrest Rate Groups")
intrest_loan_status_plot
#As the intrest rate increases, the Proportion of Default_New reduces.

#-------------End of int_rate_grp vs loan_status_1-------------------------------------------

#------------------------END MULTIVARIATE ANALYSIS------------------------------------------------#
grid.arrange(status_income_plot,status_loan_amnt,status_dti_plot,status_funded_amnt,ncol=2)
grid.arrange(int_income_plot,int_loan_amnt,int_dti_plot,int_funded_amnt,ncol=2)
grid.arrange(int_purpose,int_grade,int_emp_plot,int_home_plot,ncol=2)
grid.arrange(status_purpose,status_grade,status_emp_plot,status_home_plot,intrest_loan_status_plot,ncol=2)


#-----------------------------END CHECKPOINT 2 : Exploratory Data Analysis----------------------------------------------------------------------#


#-----------------------------BEGIN CHECKPOINT 3: Hypothesis Testing-------------------------------------------------------------------------------------------#

#Objective : Analyse if continuous driver variables have different mean values for the two categorical variables, loan_status_1 and int_rate_grp

#-----------------------------Begin Loan Status Hypothesis Testing----------------------------------------

#For loan_status_1 we consider only levels default_new  and current_new
#Level of significance is 95% => alpha=0.05

#Lets extract data where Loan_Status_1 is Default_New or Current_New
loan_status_subset<-loans_dt[loan_status_1=="Default_New" | loan_status_1=="Current_New"]

default_new_loan<-loan_status_subset[loan_status_1=="Default_New"]
default_new_loan

current_new_loan<-loan_status_subset[loan_status_1=="Current_New"]
current_new_loan

#------------1. Annual_Income vs Loan Status-------------------------------------------------

#Null Hypothesis : The Average Annual Income for Default_New in equal to annual income for Current_New

avg_annual_income=loan_status_subset[,mean(annual_inc),by=loan_status_1]
colnames(avg_annual_income)<-c("loan_status_1","Average_Annual_Income")
avg_annual_income


ttest1<-t.test(annual_inc~loan_status_1,data=loan_status_subset) #t=8.1166
ttest1
ttest1$p.value
avgAnnualIn_Default <- mean(loans_dt[loan_status_1=="Default_New" & annual_inc<136000]$annual_inc)
avgAnnualIn_Current <- mean(loans_dt[loan_status_1=="Current_New" & annual_inc<136000]$annual_inc)

#default_income_hist_plot <- ggplot(loan_status_subset[loan_status_1=="Default_New" & annual_inc<136000],aes(x=annual_inc))+geom_histogram(aes(y=..count..),binwidth = 1000, colour="royalblue4", fill="lightblue")+geom_density(aes(y = ..count..*1000),alpha=.1, fill="blue") 
#default_income_hist_plot <- default_income_hist_plot+labs(x="Annual Income")+ggtitle("Distribution of Annual Income")
#default_income_hist_plot<- default_income_hist_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
#default_income_hist_plot<- default_income_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#default_income_hist_plot<- default_income_hist_plot+geom_vline(aes(xintercept=avgAnnualIn_Default),color="brown4", linetype="dashed", size=1.4)
#default_income_hist_plot <- default_income_hist_plot+ annotate("text", x = avgAnnualIn_Default, y= 310, label = "Defaulter's Avg Income", colour="brown4",size=4)
#default_income_hist_plot


income_hist_plot_2 <- ggplot(loan_status_subset[annual_inc<136000],aes(x=annual_inc))+geom_histogram(aes(y=..count..),binwidth = 1000, colour="royalblue4", fill="lightblue")+geom_density(aes(y = ..count..*1000),alpha=.1, fill="blue") 
income_hist_plot_2 <- income_hist_plot_2+labs(x="Annual Income")+ggtitle("Distribution of Annual Income")
income_hist_plot_2<- income_hist_plot_2+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
income_hist_plot_2<- income_hist_plot_2+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
income_hist_plot_2<- income_hist_plot_2+geom_vline(aes(xintercept=avgAnnualIn_Current),color="blue", linetype="dashed", size=1.4)
income_hist_plot_2<- income_hist_plot_2+geom_vline(aes(xintercept=avgAnnualIn_Default),color="brown4", linetype="dashed", size=1.4)
income_hist_plot_2<-income_hist_plot_2+facet_grid(loan_status_1~.)
income_hist_plot_2


#---------End of Annual Income vs Loan Status---------------------------------------------------

#----------2. Loan_Amount vs Loan Status--------------------------------------------------------

avg_loan_amount=loan_status_subset[,mean(loan_amnt),by=loan_status_1]
colnames(avg_loan_amount)=c("loan_status_1","Average Loan Amount")
avg_loan_amount

ttest2<-t.test(loan_amnt~loan_status_1,data=loan_status_subset)
ttest2
ttest2$p.value
avgLoanAmnt_default<-avg_loan_amount$`Average Loan Amount`[1]
avgLoanAmnt_current<-avg_loan_amount$`Average Loan Amount`[2]

#default_loan_amt_hist_plot <- ggplot(loan_status_subset[loan_status_1=="Default_New"],aes(x=loan_amnt))
#default_loan_amt_hist_plot <- default_loan_amt_hist_plot+geom_histogram(aes(y=..count..),binwidth = 1000, colour="royalblue4", fill="lightblue")
#default_loan_amt_hist_plot <- default_loan_amt_hist_plot+geom_density(aes(y = ..count..*1000),alpha=.1, fill="blue") 
#default_loan_amt_hist_plot <- default_loan_amt_hist_plot+labs(x="Loan Amount")+ggtitle("Distribution of Loan Amount")
#default_loan_amt_hist_plot<- default_loan_amt_hist_plot+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
#default_loan_amt_hist_plot <- default_loan_amt_hist_plot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#default_loan_amt_hist_plot <- default_loan_amt_hist_plot+geom_vline(aes(xintercept=avgLoanAmnt_default),color="darkred", linetype="dashed", size=1)
#default_loan_amt_hist_plot<- default_loan_amt_hist_plot+annotate("text", x = avgLoanAmnt_default, y= 600, label = "Defaulter's Avg Loan Amount", colour="darkred",size=5)
#default_loan_amt_hist_plot

loan_amt_hist_plot_2 <- ggplot(loan_status_subset,aes(x=loan_amnt))
loan_amt_hist_plot_2 <- loan_amt_hist_plot_2+geom_histogram(aes(y=..count..),binwidth = 1000, colour="royalblue4", fill="lightblue")
loan_amt_hist_plot_2 <- loan_amt_hist_plot_2+geom_density(aes(y = ..count..*1000),alpha=.1, fill="blue") 
loan_amt_hist_plot_2 <- loan_amt_hist_plot_2+labs(x="Loan Amount")+ggtitle("Distribution of Loan Amount")
loan_amt_hist_plot_2<- loan_amt_hist_plot_2+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
loan_amt_hist_plot_2 <- loan_amt_hist_plot_2+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
loan_amt_hist_plot_2 <- loan_amt_hist_plot_2+geom_vline(aes(xintercept=avgLoanAmnt_current),color="blue", linetype="dashed", size=1)
loan_amt_hist_plot_2 <- loan_amt_hist_plot_2+geom_vline(aes(xintercept=avgLoanAmnt_default),color="darkred", linetype="dashed", size=1)
loan_amt_hist_plot_2<-loan_amt_hist_plot_2+facet_grid(loan_status_1~.)
loan_amt_hist_plot_2


#---------End of Loan Amount vs Loan Status----------------------------------------------------

#---------3. Funded_Amount vs Loan Status------------------------------------------------------
avg_funded_amount=loan_status_subset[,mean(funded_amnt),by=loan_status_1]
colnames(avg_funded_amount)=c("loan_status_1","Average Funded Amount")
avg_funded_amount

ttest3<-t.test(funded_amnt~loan_status_1,data=loan_status_subset)
ttest3
ttest3$p.value

df_mean <- mean(loan_status_subset[loan_status_1=="Default_New"]$funded_amnt)
cu_mean <- mean(loan_status_subset[loan_status_1=="Current_New"]$funded_amnt)

funded_amt_hist_plot_2 <- ggplot(loan_status_subset,aes(x=funded_amnt))
funded_amt_hist_plot_2 <- funded_amt_hist_plot_2+geom_histogram(aes(y=..count..),binwidth = 1000, colour="royalblue4", fill="lightblue")
funded_amt_hist_plot_2 <- funded_amt_hist_plot_2+geom_density(aes(y = ..count..*1000),alpha=.1, fill="blue") 
funded_amt_hist_plot_2 <- funded_amt_hist_plot_2+labs(x="Funded Amount")+ggtitle("Distribution of Funded Amount")
funded_amt_hist_plot_2<- funded_amt_hist_plot_2+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
funded_amt_hist_plot_2 <- funded_amt_hist_plot_2+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
funded_amt_hist_plot_2 <- funded_amt_hist_plot_2+geom_vline(aes(xintercept=df_mean),color="blue", linetype="dashed", size=1)
funded_amt_hist_plot_2 <- funded_amt_hist_plot_2+geom_vline(aes(xintercept=cu_mean),color="darkred", linetype="dashed", size=1)
funded_amt_hist_plot_2<-funded_amt_hist_plot_2+facet_grid(loan_status_1~.)
funded_amt_hist_plot_2


#-----------End of funded_amount vs Loan Status------------------------------------------------


#-------------4. dti vs Loan_Status-----------------------------------------------------------

ttest4<-t.test(dti~loan_status_1,data=loan_status_subset)
ttest4
ttest4$p.value

mean1<- mean(loans_dt[loan_status_1=="Default_New"]$dti)
mean2<- mean(loans_dt[loan_status_1=="Current_New"]$dti)

dti_hist_plot_2 <- ggplot(loan_status_subset,aes(x=dti))
dti_hist_plot_2 <- dti_hist_plot_2+geom_histogram(aes(y=..count..),binwidth = 1, colour="royalblue4", fill="lightblue")
dti_hist_plot_2 <- dti_hist_plot_2+geom_density(aes(y = ..count..),alpha=.1, fill="blue") 
dti_hist_plot_2 <- dti_hist_plot_2+labs(x="Funded Amount")+ggtitle("Distribution of DTI")
dti_hist_plot_2 <- dti_hist_plot_2+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dti_hist_plot_2 <- dti_hist_plot_2+geom_vline(aes(xintercept=mean1),color="blue", linetype="dashed", size=1)
dti_hist_plot_2 <- dti_hist_plot_2+geom_vline(aes(xintercept=mean2),color="darkred", linetype="dashed", size=1)
dti_hist_plot_2<-dti_hist_plot_2+facet_grid(loan_status_1~.)
dti_hist_plot_2


#------------End of dti vs Loan_Status---------------------------------------------------------

grid.arrange(income_hist_plot_2,loan_amt_hist_plot_2,funded_amt_hist_plot_2,dti_hist_plot_2,ncol=2)
#-----------------------------END Loan Status Hypothesis Testing----------------------------------------

#-----------------------------BEGIN INT_RATE_GRP Hypothesis Testing----------------------------------------

#For int_rate_grp, we will consider only low and high intrest group rate.
#The confidence level for all the test is 95% => alpha is 0.05.

loan_intrest_rate_subset<-loans_dt[int_rate_grp=="Low" | int_rate_grp=="High"]
loan_intrest_rate_subset

m1 <- mean(loan_intrest_rate_subset[loan_status_1=="Default_New"]$annual_inc)
m2 <- mean(loan_intrest_rate_subset[loan_status_1=="Current_New"]$annual_inc)

#------------------------1. Annual Income vs int_rate_grp----------------------------------------------

ttest5<-t.test(annual_inc~int_rate_grp,data=loan_intrest_rate_subset)
ttest5
ttest5$p.value
income_hist_plot_3 <- ggplot(loan_intrest_rate_subset[annual_inc<136000],aes(x=annual_inc))+geom_histogram(aes(y=..count..),binwidth = 1000, colour="royalblue4", fill="lightblue")+geom_density(aes(y = ..count..*1000),alpha=.1, fill="blue") 
income_hist_plot_3 <- income_hist_plot_3+labs(x="Annual Income")+ggtitle("Distribution of Annual Income")
income_hist_plot_3<- income_hist_plot_3+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
income_hist_plot_3<- income_hist_plot_3+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
income_hist_plot_3<- income_hist_plot_3+geom_vline(aes(xintercept=m1),color="brown4", linetype="dashed", size=1.4)
income_hist_plot_3<- income_hist_plot_3+geom_vline(aes(xintercept=m2),color="blue", linetype="dashed", size=1.4)
income_hist_plot_3<-income_hist_plot_3+facet_grid(int_rate_grp~.)
income_hist_plot_3

#-------------------End of Annual Income vs Int Rate Group-----------------------------------------

#--------------------2. Loan Amount vs int rate group---------------------------------------------

ttest6<-t.test(loan_amnt~int_rate_grp,data=loan_intrest_rate_subset)
ttest6
ttest6$p.value

avg1 <- mean(loan_intrest_rate_subset[loan_status_1=="Default_New"]$loan_amnt)
avg2 <- mean(loan_intrest_rate_subset[loan_status_1=="Current_New"]$loan_amnt)

loan_amt_hist_plot_3 <- ggplot(loan_intrest_rate_subset,aes(x=loan_amnt))
loan_amt_hist_plot_3 <- loan_amt_hist_plot_3+geom_histogram(aes(y=..count..),binwidth = 1000, colour="royalblue4", fill="lightblue")
loan_amt_hist_plot_3 <- loan_amt_hist_plot_3+geom_density(aes(y = ..count..*1000),alpha=.1, fill="blue") 
loan_amt_hist_plot_3 <- loan_amt_hist_plot_3+labs(x="Loan Amount")+ggtitle("Distribution of Loan Amount")
loan_amt_hist_plot_3<- loan_amt_hist_plot_3+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
loan_amt_hist_plot_3 <- loan_amt_hist_plot_3+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
loan_amt_hist_plot_3 <- loan_amt_hist_plot_3+geom_vline(aes(xintercept=avg1),color="blue", linetype="dashed", size=1)
loan_amt_hist_plot_3 <- loan_amt_hist_plot_3+geom_vline(aes(xintercept=avg2),color="darkred", linetype="dashed", size=1)
loan_amt_hist_plot_3<-loan_amt_hist_plot_3+facet_grid(int_rate_grp~.)
loan_amt_hist_plot_3

#------------------End of Loan Amount vs Int Rate Group-------------------------------------------

#----------------3. Funded Amount vs int rate group-----------------------------------------------
ttest7<-t.test(funded_amnt~int_rate_grp,data=loan_intrest_rate_subset)
ttest7
ttest7$p.value

fund_avg1 <- mean(loan_intrest_rate_subset[loan_status_1=="Default_New"]$funded_amnt)
fund_avg2 <- mean(loan_intrest_rate_subset[loan_status_1=="Current_New"]$funded_amnt)

funded_amt_hist_plot_3 <- ggplot(loan_intrest_rate_subset,aes(x=funded_amnt))
funded_amt_hist_plot_3 <- funded_amt_hist_plot_3+geom_histogram(aes(y=..count..),binwidth = 1000, colour="royalblue4", fill="lightblue")
funded_amt_hist_plot_3 <- funded_amt_hist_plot_3+geom_density(aes(y = ..count..*1000),alpha=.1, fill="blue") 
funded_amt_hist_plot_3 <- funded_amt_hist_plot_3+labs(x="Funded Amount")+ggtitle("Distribution of Funded Amount")
funded_amt_hist_plot_3<- funded_amt_hist_plot_3+scale_x_continuous(labels=function(n){format(n, scientific = FALSE)})
funded_amt_hist_plot_3 <- funded_amt_hist_plot_3+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
funded_amt_hist_plot_3 <- funded_amt_hist_plot_3+geom_vline(aes(xintercept=fund_avg1),color="blue", linetype="dashed", size=1)
funded_amt_hist_plot_3 <- funded_amt_hist_plot_3+geom_vline(aes(xintercept=fund_avg2),color="darkred", linetype="dashed", size=1)
funded_amt_hist_plot_3<-funded_amt_hist_plot_3+facet_grid(int_rate_grp~.)
funded_amt_hist_plot_3


#---------------------End of Funded Amount vs Int Rate Group--------------------------------------

#--------------4. DTI vs Int Rate Group-------------------------------------------------------
ttest8<-t.test(dti~int_rate_grp,data=loan_intrest_rate_subset)
ttest8
ttest8$p.value


dti_avg1 <- mean(loan_intrest_rate_subset[loan_status_1=="Default_New"]$dti)
dti_avg2 <- mean(loan_intrest_rate_subset[loan_status_1=="Current_New"]$dti)

dti_hist_plot_3<- ggplot(loan_intrest_rate_subset,aes(x=dti))
dti_hist_plot_3 <- dti_hist_plot_3+geom_histogram(aes(y=..count..),binwidth = 1, colour="royalblue4", fill="lightblue")
dti_hist_plot_3 <- dti_hist_plot_3+geom_density(aes(y = ..count..),alpha=.1, fill="blue") 
dti_hist_plot_3 <- dti_hist_plot_3+labs(x="Funded Amount")+ggtitle("Distribution of DTI")
dti_hist_plot_3 <- dti_hist_plot_3+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
dti_hist_plot_3 <- dti_hist_plot_3+geom_vline(aes(xintercept=dti_avg1),color="blue", linetype="dashed", size=1)
dti_hist_plot_3 <- dti_hist_plot_3+geom_vline(aes(xintercept=dti_avg2),color="darkred", linetype="dashed", size=1)
dti_hist_plot_3<-dti_hist_plot_3+facet_grid(int_rate_grp~.)
dti_hist_plot_3

#----------End of DTI vs int rate group--------------------------------------------------------

grid.arrange(income_hist_plot_3,loan_amt_hist_plot_3,funded_amt_hist_plot_3,dti_hist_plot_3,ncol=2)



#-----------------------_END OF CHECKPOINT 3: HYPOTHESIS TESTING------------------------------------




