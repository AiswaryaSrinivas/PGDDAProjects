setwd("/home/aiswarya/upgradDataAnalytics/Course4/Assignments/Graded/NeuralNetworks/")

library(data.table)
library(ggplot2)
#----------------------------BEGIN DATA PREPARATION-------------------------------------------------------------------------------------------
churn<-fread("telecom_nn_train.csv",stringsAsFactors = T)
str(churn)

#--------Remove duplicate Columns--------------------------------------------

all(!churn[,"MultipleLinesNo phone service"]==churn$PhoneService)
#MultipleLines No phone Service is just the inverse of Phone Service.Hence, we remove col MultipleLins No Phone Service
churn[,`MultipleLinesNo phone service`:=NULL]
str(churn)

all(churn$`OnlineSecurityNo internet service`==churn$`OnlineBackupNo internet service`)
all(churn$`StreamingTVNo internet service`==churn$`StreamingMoviesNo internet service`)
all(churn$`OnlineBackupNo internet service`==churn$`TechSupportNo internet service`)
all(churn$InternetServiceNo==churn$`OnlineBackupNo internet service`)

#Lets drop all Columns with No InternetService
churn$`OnlineBackupNo internet service`<-NULL
churn$`OnlineSecurityNo internet service`<-NULL
churn$`StreamingTVNo internet service`<-NULL
churn$`StreamingMoviesNo internet service`<-NULL
churn$`TechSupportNo internet service`<-NULL
churn$`DeviceProtectionNo internet service`<-NULL
str(churn)

#-------End remove duplicate columns----------------------------------------

#------Missing Value---------------------------------------------------------------------
sum(is.na(churn))
colSums(is.na(churn)) #Total Charges has 6 missing values
churn[is.na(TotalCharges)]

# Notice all the of observations tenure is 0.
churn[tenure==0]
# we can clearly see that for new customers( whose tenure is zero) Total Charges value is NA.

# May be we can replace Total Charges missing values with zero. (with an assumption that they have not paid anything yet)

churn[is.na(TotalCharges),TotalCharges:=0]
sum(is.na(churn$TotalCharges))
summary(churn$TotalCharges)


#----END Missing Value Treatment-----------------------------------------------------------

boxplot(churn$tenure)
quantile(churn$tenure,probs=seq(0,1,0.01))
#No outliers---

boxplot(churn$MonthlyCharges)
quantile(churn$MonthlyCharges,probs=seq(0,1,0.01))
#There are no outliers 

boxplot(churn$TotalCharges)
quantile(churn$TotalCharges,probs=seq(0,1,0.01))
#No outliers


#Write this data into

write.csv(churn,"churn_preprocessed.csv",row.names = F)

#----------------------------END DATA PREPARATION------------------------------------------------------------------------------


#---------------------DATA EXPLORATION--------------------------------------------------------------

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

table(churn$gender,churn$Churn)/nrow(churn)
plot_gender<- churn.barPlot.dist(churn,churn$gender,"Distribution of Gender","Gender")
plot_gender

plot_gender_churn<- churn.barPlot.withTarget(churn,churn$gender,"Gender vs Churn","Gender",position = "dodge")
plot_gender_churn


table(churn$SeniorCitizen)
plot_senior<-churn.barPlot.dist(churn,churn$SeniorCitizen,"Distribution of Gender","Senior Citizen",percentage = TRUE)
plot_senior  
#~16% of the total customers are senior citizens

plot_senior_churn<- churn.barPlot.withTarget(churn,churn$SeniorCitizen,"Senior Citizen vs Churn","Senior Citizen",position = "dodge",percentage = TRUE,grid = FALSE)
plot_senior_churn

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

#PhoneService
plot_phoneservice <- churn.barPlot.dist(churn,churn$PhoneService,"Distribution of Customers with and without Phone Service",xlabel = "PhoneService", ylabel = "Percentage of Customers",percentage = TRUE)
plot_phoneservice
#around 90% of the people have Phone Service

plot_phoneservice_churn<-churn.barPlot.withTarget(churn,churn$PhoneService,"PhoneService vs Churn","Has PhoneService",position = "dodge",percentage = TRUE,grid = FALSE)
plot_phoneservice_churn
#Churn Rate is high for people who opted phone service than who didnt opt 



plot_paperlessBilling<-churn.barPlot.dist(churn,churn$PhoneService,"Distribution of Customers with and without Paperless Billing",xlabel = "Paperless Billing", ylabel = "Percentage of Customers",percentage = TRUE)
plot_paperlessBilling

plot_paperlessBilling_churn<-churn.barPlot.withTarget(churn,churn$PaperlessBilling,"Paperless Billing vs Churn","Opted Paperless Billing",position = "dodge",percentage = TRUE,grid = FALSE)
plot_paperlessBilling_churn

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

#MultipleLines Yes
plot_multipleLinesYes<-ggplot(churn,aes(x=MultipleLinesYes,fill=MultipleLinesYes))+geom_bar(aes(y=(..count..)/sum(..count..)))+scale_fill_brewer(palette = "Paired")+ylab("Percentage")+ggtitle("Distribution of Multiple Line")
plot_multipleLinesYes

plot_multipleLinesYes_churn<-churn.barPlot.withTarget(churn,churn$MultipleLinesYes,"MultipleLinesvs Churn","MultipleLinesYes",position = "dodge",percentage = TRUE,grid = FALSE)
plot_multipleLinesYes_churn

plot_internetFibreOptic<-churn.barPlot.dist(churn,churn$`InternetServiceFiber optic`,"Distribution of FibreOptic Service","FibreOptic",percentage = TRUE)
plot_internetFibreOptic

plot_internetFibreOptic_Churn<-churn.barPlot.withTarget(churn,churn$`InternetServiceFiber optic`,"FibreOptic vs Churn","FibreOptic",position = "dodge",percentage = TRUE,grid = FALSE)
plot_internetFibreOptic_Churn

plot_Nointernet<-churn.barPlot.dist(churn,churn$InternetServiceNo,"Distribution of No InternetService","No Internet Service",percentage = TRUE)
plot_Nointernet

plot_Nointernet_churn<-churn.barPlot.withTarget(churn,churn$InternetServiceNo,"No Internet Service vs Churn","No Internet Service",position = "dodge",percentage = TRUE,grid = FALSE)
plot_Nointernet_churn

plot_OnlineSecurity<-churn.barPlot.dist(churn,churn$OnlineSecurityYes,"Distribution of Online Security","Online Security",percentage = TRUE)
plot_OnlineSecurity

plot_OnlineSecurity_churn<-churn.barPlot.withTarget(churn,churn$OnlineSecurityYes,"Online Security vs Churn","Online Security",position = "dodge",percentage = TRUE,grid = FALSE)
plot_OnlineSecurity_churn

plot_OnlineBackup<-churn.barPlot.dist(churn,churn$OnlineBackupYes,"Distribution of Online Backup","Online Backup",percentage = TRUE)
plot_OnlineBackup

plot_OnlineBackup_churn<-churn.barPlot.withTarget(churn,churn$OnlineBackupYes,"OnlineBackup vs Churn","OnlineBackup",position = "dodge",percentage = TRUE,grid = FALSE)
plot_OnlineBackup_churn

plot_DeviceProtection<-churn.barPlot.dist(churn,churn$DeviceProtectionYes,"Distribution of Device Protection","Device Protection",percentage = TRUE)
plot_DeviceProtection

plot_DeviceProtection_churn<-churn.barPlot.withTarget(churn,churn$DeviceProtectionYes,"Device Protection vs Churn","Device Protection",position = "dodge",percentage = TRUE,grid = FALSE)
plot_DeviceProtection_churn

plot_TechSupoort<-churn.barPlot.dist(churn,churn$TechSupportYes,"Distribution of Tech Support","Tech Support",percentage = TRUE)
plot_TechSupoort

plot_TechSupoort_churn<-churn.barPlot.withTarget(churn,churn$TechSupportYes,"TechSupport vs Churn","TechSupport",position = "dodge",percentage = TRUE,grid = FALSE)
plot_TechSupoort_churn

plot_StreamingTV<-churn.barPlot.dist(churn,churn$StreamingTVYes,"Distribution of Streaming TV","Streaming TV",percentage = TRUE)
plot_StreamingTV

plot_StreamingTV_churn<-churn.barPlot.withTarget(churn,churn$StreamingTVYes,"StreamingTV vs Churn","StreamingTV",position = "dodge",percentage = TRUE,grid = FALSE)
plot_StreamingTV_churn

plot_StreamingMovies<-churn.barPlot.dist(churn,churn$StreamingMoviesYes,"Distribution of Streaming Movies","Streaming Movies",percentage = TRUE)
plot_StreamingMovies

plot_StreamingMovies_churn<-churn.barPlot.withTarget(churn,churn$StreamingMoviesYes,"StreamingMovies vs Churn","StreamingMovies",position = "dodge",percentage = TRUE,grid = FALSE)
plot_StreamingMovies_churn

plot_ContractOneYr<-churn.barPlot.dist(churn,churn$`ContractOne year`,"Distribution of One Year Contract","One Year COntract",percentage = TRUE)
plot_ContractOneYr

plot_ContractOneYr_churn<-churn.barPlot.withTarget(churn,churn$`ContractOne year`,"One Year Contract vs Churn","One Year Contract",position = "dodge",percentage = TRUE,grid = FALSE)
plot_ContractOneYr_churn

plot_ContractTwoYr<-churn.barPlot.dist(churn,churn$`ContractTwo year`,"Distribution of Two Year Contract","Two Year Contract",percentage = TRUE)
plot_ContractTwoYr

plot_ContractTwoYr_churn<-churn.barPlot.withTarget(churn,churn$`ContractTwo year`,"TwoYearContract vs Churn","Two Year Contract",position = "dodge",percentage = TRUE,grid = FALSE)
plot_ContractTwoYr_churn

plot_paymentCreditcard<-churn.barPlot.dist(churn,churn$`PaymentMethodCredit card (automatic)`,"Distribution of Credit Card Payments","Credit Card",percentage = TRUE)
plot_paymentCreditcard

plot_paymentCreditcard_churn<-churn.barPlot.withTarget(churn,churn$`PaymentMethodCredit card (automatic)`,"Credit Card vs Churn","Credit Card",position = "dodge",percentage = TRUE,grid = FALSE)
plot_paymentCreditcard_churn

plot_paymentElectronicCheck<-churn.barPlot.dist(churn,churn$`PaymentMethodElectronic check`,"Distribution of Electronic Check Payments","Electronic Check",percentage = TRUE)
plot_paymentElectronicCheck

plot_paymentElectronicCheck_churn<-churn.barPlot.withTarget(churn,churn$`PaymentMethodElectronic check`,"Electronic Check vs Churn","Electroni Check",position = "dodge",percentage = TRUE,grid = FALSE)
plot_paymentElectronicCheck_churn

plot_paymentMailedCheck<-churn.barPlot.dist(churn,churn$`PaymentMethodMailed check`,"Distribution of Mailed Check Payments","Mailed Check",percentage = TRUE)
plot_paymentMailedCheck

plot_paymentMailedCheck_churn<-churn.barPlot.withTarget(churn,churn$`PaymentMethodMailed check`,"Mailed Check vs Churn","Mailed Check",position = "dodge",percentage = TRUE,grid = FALSE)
plot_paymentMailedCheck_churn

#---------------------END DATA EXPLORATION---------------------------------------------------------------

#----------------------------BEGIN MODELLING-----------------------------------------------------------------------------------------------
validationresults=data.frame(model=character(),TrainingAUC=double(),validationAUC=double(),TrainingLogLoss=double(),ValidationLogLoss=double())
library(h2o)


#Start H2o
h2o.init()

#Import the  data into h2o
churn<-h2o.importFile("churn_preprocessed.csv")

#Split the data into 80:20 for testing and training with 5 fold cross validation
splits=h2o.splitFrame(churn,ratio=0.8,seed = 100)
train=h2o.assign(splits[[1]],key="train")
test=h2o.assign(splits[[2]],key="test")

target<-"Churn"

features<-setdiff(colnames(train),target)
features #Thre are 23 features.

train[,target]<-as.factor(train[,target])
test[,target]<-as.factor(test[,target])
#We will use AUC as the stoppin metric, and from AUC curve we will identify cutoffs for maximising sensitivity.

#Let us first start with the default parameters and apply cross validation
model=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                       ,stopping_metric = "AUC",reproducible = T)
    
summary(model)
model@allparameters$epochs

results<-data.frame(model=model@model_id,TrainingAUC=h2o.auc(model),validationAUC=h2o.auc(model,xval=T),TrainingLogLoss=h2o.logloss(model),ValidationLogLoss=h2o.logloss(model,xval = T))
results
str(results)
validationresults<-rbind(validationresults,results)

#This model is slow.
# On Validation AUC: 0.8135659 and Logloss="0.5595615"
#On Full Training Data, AUC: 0.8422104 and LogLoss= 0.5384053
#We can observe that logloss on training is much less than log loss on validation.
#Also the AUC on the training data is 0.825 while data on 5 fold cross valudation is 0.79
#This means, variance ishigher. The default number of hidden layers in 2, with 200 neurons respectively
#This model also by default used the rectifier function.
#This model has high bias and high variance


#Lets us decrease the variance and bias by decreasing the number of hidden neurons to 3* number_input_features
model_1=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                       ,hidden = c(3*length(features),3*length(features)),stopping_metric = "AUC",reproducible = T)

summary(model_1)
results<-data.frame(model=model_1@model_id,TrainingAUC=h2o.auc(model_1),validationAUC=h2o.auc(model_1,xval=T),TrainingLogLoss=h2o.logloss(model_1),ValidationLogLoss=h2o.logloss(model_1,xval = T))
results
validationresults<-rbind(validationresults,results)
#AUC of 0.8286342 and LogLoss of 0.4622216 on Validation
#AUC of 0.8514699 and LogLoss of 0.4508577 on Training
#This model also has high variance, but the AUC of this model is better than the benchmark model
#Let us further decrease the number of neurons to 1.5*number_input_features(35 neurons in each layer)

model_2=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                         ,hidden = c(35,35),stopping_metric = "AUC",reproducible = T)

summary(model_2)
results<-data.frame(model=model_2@model_id,TrainingAUC=h2o.auc(model_2),validationAUC=h2o.auc(model_2,xval=T),TrainingLogLoss=h2o.logloss(model_2),ValidationLogLoss=h2o.logloss(model_2,xval = T))
results
validationresults<-rbind(validationresults,results)
#AUC of  0.827918 and logLoss of 0.4636483 on Validation data
#AUC of 0.8474655 and Logloss of 0.4346461 on Training data.
#We can observe that difference between AUC of training and validation data has decreased,
#This indicates that decrease in Number of Neurons in the hidden layer has decreased the variance.
#Also, we can see that the AUC of model_1 and model_2 is 0.82 on validation data, which further validates
#that decrease in number of neurons in hidden layer was right.

#Let us decrease the number of hidden neurons to 30 for each layer and observe what happens

model_3=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                         ,hidden = c(30,30),stopping_metric = "AUC",reproducible = T)


summary(model_3)

results<-data.frame(model=model_3@model_id,TrainingAUC=h2o.auc(model_3),validationAUC=h2o.auc(model_3,xval=T),TrainingLogLoss=h2o.logloss(model_3),ValidationLogLoss=h2o.logloss(model_3,xval = T))

validationresults<-rbind(validationresults,results)
validationresults
#AUC of 0.8247370 and Log loss of 0.4590817 on Validation data
#AUC of 0.8497806 and log loss of 0.4151233 on Training data.

#The training AUC has increased, while validation AUC has decreased. Showing signs of overfitting
#We can see that decreasing the number of neurons in hidden layer from 35 to 30, has increased the variance
#Hence, model_2 is better than model_3 .

#Optimal Number of Neurons in Hidden Layer: (35,35)

#To decrease the variance further, let us try changing the input_dropout_ratio
model_4=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                         ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.1,reproducible = T)

summary(model_4)

results<-data.frame(model=model_4@model_id,TrainingAUC=h2o.auc(model_4),validationAUC=h2o.auc(model_4,xval=T),TrainingLogLoss=h2o.logloss(model_4),ValidationLogLoss=h2o.logloss(model_4,xval = T))
results
validationresults<-rbind(validationresults,results)


#AUC : 0.8249939 and Logloss :0.4696721 on Validation
#AUC : 0.8406159 and LogLoss : 0.4410114 on Training
#Setting the input dropout ratio to 0.1, has led to a lower Training AUC and higher logloss.
#Does increase in input dropout ratio leads to higher log loss?? Let us verify

model_5=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                         ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,reproducible = T)

summary(model_5)

results<-data.frame(model=model_5@model_id,TrainingAUC=h2o.auc(model_5),validationAUC=h2o.auc(model_5,xval=T),TrainingLogLoss=h2o.logloss(model_5),ValidationLogLoss=h2o.logloss(model_5,xval = T))
results
validationresults<-rbind(validationresults,results)
validationresults
#AUC : 0.8224027 and Logloss :0.4715242
#AUC :0.8364317 and Logloss :0.4520154

#We can observe that increase in input dropout ratio,has decreased the variance
#Training and Validation AUC is minimised, which means that it has reduced the variance of our stoppin metric.
#We shall increase the input dropout ratio further and check if it reduces the variance

model_6=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                         ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.2,reproducible = T)

summary(model_6)
results<-data.frame(model=model_6@model_id,TrainingAUC=h2o.auc(model_6),validationAUC=h2o.auc(model_6,xval=T),TrainingLogLoss=h2o.logloss(model_6),ValidationLogLoss=h2o.logloss(model_6,xval = T))
results
validationresults<-rbind(validationresults,results)
validationresults
#AUC :0.8208505 and log loss :0.4748938
#AUC : 0.8349914 and logloss : 0.4537044
#Havin input_dropout_ratio of 0.2 has increased the training logloss, which is not quality of a good model
#Input_dropout_ratio of 0.2 also led to decrease in AUC,also variance has not decreased.

#Optimal Input Dropout : 0.15


#Let us try changin the activation function to rectifierwithDropout to see if it increase the AUC 
model_7=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                         ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                         activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.05,0.05),reproducible = T)

summary(model_7)

results<-data.frame(model=model_7@model_id,TrainingAUC=h2o.auc(model_7),validationAUC=h2o.auc(model_7,xval=T),TrainingLogLoss=h2o.logloss(model_7),ValidationLogLoss=h2o.logloss(model_7,xval = T))
results
validationresults<-rbind(validationresults,results)
validationresults
#Reduce in Bias and slight increase in let us further increase the hidden dropout ratio of one layer and check what happens

model_8=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                         ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                         reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.1,0.05))

summary(model_8)

results<-data.frame(model=model_8@model_id,TrainingAUC=h2o.auc(model_8),validationAUC=h2o.auc(model_8,xval=T),TrainingLogLoss=h2o.logloss(model_8),ValidationLogLoss=h2o.logloss(model_8,xval = T))
results
validationresults<-rbind(validationresults,results)
validationresults

#There is a very slight ddecrease in Validation AUC, while there is a slight increase in Training AUC. But log loss has decreased
#Let us increase the dropout ratio of the other layer also and check what happens to the model

model_9=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                         ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                         reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.1,0.1))

summary(model_9)
results<-data.frame(model=model_9@model_id,TrainingAUC=h2o.auc(model_9),validationAUC=h2o.auc(model_9,xval=T),TrainingLogLoss=h2o.logloss(model_9),ValidationLogLoss=h2o.logloss(model_9,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#The training log loss has decreased, while validation logloss has increased, implying overfitting.
#Also, the Validation AUC has decreased further while trainign AUC has increased.
#So to model 8, let us increase the dropout ratio of the first layer, keeping dropout of the second layer as 0.05

model_10=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                         ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                         reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.15,0.05))


summary(model_10)

results<-data.frame(model=model_10@model_id,TrainingAUC=h2o.auc(model_10),validationAUC=h2o.auc(model_10,xval=T),TrainingLogLoss=h2o.logloss(model_10),ValidationLogLoss=h2o.logloss(model_10,xval = T))

validationresults<-rbind(validationresults,results)
validationresults

#Compated to model 8, the log loss of training as well as validation has reduced.
#The Variance has also decreased. There is a increase in Validation AUC.
#Let us further increase the dropout ratio of the 1st hidden layer and check what happend

model_11=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.2,0.05))

summary(model_11)

results<-data.frame(model=model_11@model_id,TrainingAUC=h2o.auc(model_11),validationAUC=h2o.auc(model_11,xval=T),TrainingLogLoss=h2o.logloss(model_11),ValidationLogLoss=h2o.logloss(model_11,xval = T))

validationresults<-rbind(validationresults,results)
validationresults
#The logloss of the validation data has increased from model_10. Hence, model_10 is a better model
#We also see there is a decrease in validation AUC and training AUC.

#Optimal HiddenDropoutRatio =c (0.15,0.05)

#Let us change the activation function and understand what effect it has on AUC and LogLoss

model_12=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.15,0.05))

summary(model_12)

results<-data.frame(model=model_12@model_id,TrainingAUC=h2o.auc(model_12),validationAUC=h2o.auc(model_12,xval=T),TrainingLogLoss=h2o.logloss(model_12),ValidationLogLoss=h2o.logloss(model_12,xval = T))

validationresults<-rbind(validationresults,results)
validationresults

#The variance has reduced consoderably. Also the Logloss is also reduced.There is a only a slight increase in bias.


#Let us try MaxOut activation function
model_13=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "MaxoutWithDropout",hidden_dropout_ratios = c(0.15,0.05))

summary(model_13)

results<-data.frame(model=model_13@model_id,TrainingAUC=h2o.auc(model_13),validationAUC=h2o.auc(model_13,xval=T),TrainingLogLoss=h2o.logloss(model_13),ValidationLogLoss=h2o.logloss(model_13,xval = T))

validationresults<-rbind(validationresults,results)
validationresults
#This model has a very high logloss, which means this model will have lower accuracy.
#This model also has high variance.
#Optimal Activation Function : TanHWithDropout

#Just an experiment, for TanHWithDropout, if we increase the hidden dropout_ratio what happens.
#Will it give results similar to RectifierwithDropout>

model_14=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.2,0.05))

summary(model_14)
results<-data.frame(model=model_14@model_id,TrainingAUC=h2o.auc(model_14),validationAUC=h2o.auc(model_14,xval=T),TrainingLogLoss=h2o.logloss(model_14),ValidationLogLoss=h2o.logloss(model_14,xval = T))

validationresults<-rbind(validationresults,results)
validationresults

#With TanHWithDropout, changing the hidden dropout ratio to 0.2 and 0.05 has increased AUC of training, but validation AUC is almost the same
#This is case of overfitting to the data. Hence, we will stick to the optimal Hidden Dropout Ratio
#of (0.15 and 0.05)

#Optimal Activation function : tanhWithDropout

#What happens when we apply regularisation. Will it bring down the variance of the model further?
#Will it decrease the bias??


model_15=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.15,0.05),l1 = 0.00002)

summary(model_15)

results<-data.frame(model=model_15@model_id,TrainingAUC=h2o.auc(model_15),validationAUC=h2o.auc(model_15,xval=T),TrainingLogLoss=h2o.logloss(model_15),ValidationLogLoss=h2o.logloss(model_15,xval = T))

validationresults<-rbind(validationresults,results)
validationresults
#This has slighlty increased training and validation AUC. Let us firther increase l1 and check

model_16=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.15,0.05),l1 = 0.00004)

summary(model_16)
results<-data.frame(model=model_16@model_id,TrainingAUC=h2o.auc(model_16),validationAUC=h2o.auc(model_16,xval=T),TrainingLogLoss=h2o.logloss(model_16),ValidationLogLoss=h2o.logloss(model_16,xval = T))

validationresults<-rbind(validationresults,results)
validationresults
#There is a sloght increase in both the train AUC and Validation AUC. Let us further increase the l1 regularisation parameter

model_17=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.15,0.05),l1 = 0.00006)

summary(model_17)
results<-data.frame(model=model_17@model_id,TrainingAUC=h2o.auc(model_17),validationAUC=h2o.auc(model_17,xval=T),TrainingLogLoss=h2o.logloss(model_17),ValidationLogLoss=h2o.logloss(model_17,xval = T))

validationresults<-rbind(validationresults,results)
validationresults

#There is a slight increase in Training AUC and  Validation AUC.
#Let us increase further

model_18=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.15,0.05),l1 = 0.00008)

summary(model_18)
results<-data.frame(model=model_18@model_id,TrainingAUC=h2o.auc(model_18),validationAUC=h2o.auc(model_18,xval=T),TrainingLogLoss=h2o.logloss(model_18),ValidationLogLoss=h2o.logloss(model_18,xval = T))

validationresults<-rbind(validationresults,results)
validationresults
#There is a slight increase in both training and validation AUC

model_19=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.15,0.05),l1 = 0.0001)

summary(model_19)
results<-data.frame(model=model_19@model_id,TrainingAUC=h2o.auc(model_19),validationAUC=h2o.auc(model_19,xval=T),TrainingLogLoss=h2o.logloss(model_19),ValidationLogLoss=h2o.logloss(model_19,xval = T))

validationresults<-rbind(validationresults,results)
validationresults

#Increase in Training AUC and decrease in Validation AUC, sign of overfitting. Lets try 
#optimisin for l2. l1=0.00008

model_20=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.15,0.05),l1 = 0.00008,l2=0.00002)

summary(model_20)
results<-data.frame(model=model_20@model_id,TrainingAUC=h2o.auc(model_20),validationAUC=h2o.auc(model_20,xval=T),TrainingLogLoss=h2o.logloss(model_20),ValidationLogLoss=h2o.logloss(model_20,xval = T))

validationresults<-rbind(validationresults,results)
validationresults


#Compared to model_18, there is a increase in Validation AUC.
#Lets increase l2.
model_21=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.15,0.05),l1 = 0.00008,l2=0.00003)

summary(model_21)
results<-data.frame(model=model_21@model_id,TrainingAUC=h2o.auc(model_21),validationAUC=h2o.auc(model_21,xval=T),TrainingLogLoss=h2o.logloss(model_21),ValidationLogLoss=h2o.logloss(model_21,xval = T))

validationresults<-rbind(validationresults,results)
validationresults

#There is a increase in training and Validation AUC. That is bias has reduced. Let us increase l2 further


model_22=h2o.deeplearning(x=features,y=target,training_frame = train,epochs = 1,nfolds = 5,seed=100
                          ,hidden = c(35,35),stopping_metric = "AUC",input_dropout_ratio = 0.15,
                          reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.15,0.05),l1 = 0.00008,l2=0.00004)

summary(model_22)
results<-data.frame(model=model_22@model_id,TrainingAUC=h2o.auc(model_22),validationAUC=h2o.auc(model_22,xval=T),TrainingLogLoss=h2o.logloss(model_22),ValidationLogLoss=h2o.logloss(model_22,xval = T))

validationresults<-rbind(validationresults,results)
validationresults
#Thre is a increase in training AUC and decrease in Validation AUC.which is sign of overfitting.
#So optimal l2 is 0.00003
write.csv(validationresults,"validationResults_WithoutEpochs.csv",row.names = F)
#model_22 is our final model


#We will calculate the variance as the difference in Training and Validation AUC.
#Bias is the error. An ideal situtaion is when AUC is 1. So bias=1-AUC. But, we want bias in same scale as variance and hence we divide bias by 10
#We calculate the training and the validation bias.

validationresults=read.csv("validationResults_WithoutEpochs.csv")
validationresults$variance=validationresults$TrainingAUC - validationresults$validationAUC
validationresults
validationresults$bias=(1 - validationresults$validationAUC)/10
validationresults$trainBias=(1 - validationresults$TrainingAUC)/10
plot(validationresults$variance,col="red",type="l")
par(new=T)
lines(validationresults$bias,col="blue",type="l")
par(new=T)
lines(validationresults$trainBias,col="green",type="l")
legend(x=0.01,y=0.012,legend = c("Variance","ValidationBias","TrainBias"),col=c("red","blue","green"),lty = 1)
#We can see that after model 20, the difference between error in train and Validation also has reduced.
#Variance is minimum for model_22.

#Let us check  model_22 on our test data and check its performance
final_model=model_22
test_performance=h2o.performance(final_model,newdata = test)
test_performance@metrics$AUC #0.8108215
test_performance@metrics$logloss #0.448812

#For this model, let us plot the ROC curve and identify,cutoffs for having high sensitivity
plot(test_performance,type="roc",col="red",typ="l")
par(new=T)
plot(h2o.performance(model_16,newdata=train),type="roc",col="blue",typ="l")
par(new=T)
plot(h2o.performance(final_model,xval = T),type="roc",col="green",typ="l")
legend(x=0.7,y=0.3,legend = c("Test","Train","Validation"),col=c("red","blue","green"),lty = 1)

#Let us create a confusion matrix to calculate accuracy,sensitivity and specificity
test_pred<-h2o.predict(final_model,newdata = test)
test_pred<-test_pred[,3]
test_pred<-as.data.frame(test_pred)

test_churn<-as.data.frame(test$Churn)
test_churn$Churn<- ifelse(test_churn$Churn=="No", 0, 1)
library(caret)
confusionMatrix(as.numeric(test_pred$Yes> 0.3),test_churn$Churn,positive = "1")
#Accuracy of 0.7467
#Sensitivity of 0.6570
#Specificity of 0.7764

confusionMatrix(as.numeric(test_pred$Yes> 0.2),test_churn$Churn,positive = "1")
#Accuracy of 0.7127
#Sensitivity of 0.7727
#Specificity of 0.6927

confusionMatrix(as.numeric(test_pred$Yes> 0.5),test_churn$Churn,positive = "1")
#Accuracy of 0.7889
#Sensitivity of 0.4132
#Specificity of 0.9136


confusionMatrix(as.numeric(test_pred$Yes> 0.6),test_churn$Churn,positive = "1")
#Sensitivity : 0.2769 
#Specificity : 0.9396
#Accuracy : 0.7745 

confusionMatrix(as.numeric(test_pred$Yes> 0.7),test_churn$Churn,positive = "1")
#Specificity : 0.97531 
#Sensitivity : 0.18182         
#Accuracy : 0.7775

#As we want to maximise the sensitivity of the model, we will consider threshold of 0.2
rm(validationresults)

#---------END OF MANUAL TUNING FOR WITHOUT EPOCHS----------------------------------------------------------

#Now, we have calculated parameters for without epochs, let us tune for epochs also
validationresults=data.frame(model=character(),TrainingAUC=double(),validationAUC=double(),TrainingLogLoss=double(),ValidationLogLoss=double())

#Let us start with model, with all default parameters, including for number of epochs
model_withepoch=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                 ,stopping_metric = "AUC",reproducible = T)

summary(model_withepoch)
validationresults<-data.frame(model=model_withepoch@model_id,TrainingAUC=h2o.auc(model_withepoch),validationAUC=h2o.auc(model_withepoch,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch),ValidationLogLoss=h2o.logloss(model_withepoch,xval = T))
validationresults
#The model is very slow.
#We can observe, very high variance in the default model for both AUC and logloss.
#Hence, we must reduce this variance. So let us reduce the number of neurons in hidden layer

model_withepoch_1=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100,
                                 reproducible=T,stopping_metric = "AUC",hidden = c(69,69))

summary(model_withepoch_1)
results<-data.frame(model=model_withepoch_1@model_id,TrainingAUC=h2o.auc(model_withepoch_1),validationAUC=h2o.auc(model_withepoch_1,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_1),ValidationLogLoss=h2o.logloss(model_withepoch_1,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#This model also has a high variance, but lower compared to previous model. There is a decrease
#in AUC of the training set and increase in AUC of the validation set.So, let use decrease the
#Number of Node in second hidden layer further

model_withepoch_2=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                   ,reproducible=T,stopping_metric = "AUC",hidden = c(69,35))

summary(model_withepoch_2)
results<-data.frame(model=model_withepoch_2@model_id,TrainingAUC=h2o.auc(model_withepoch_2),validationAUC=h2o.auc(model_withepoch_2,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_2),ValidationLogLoss=h2o.logloss(model_withepoch_2,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#High Variance, decrease the number of neurons in the first hidden layer also.


model_withepoch_3=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                   ,reproducible = T,stopping_metric = "AUC",hidden = c(35,35))

summary(model_withepoch_3)

results<-data.frame(model=model_withepoch_3@model_id,TrainingAUC=h2o.auc(model_withepoch_3),validationAUC=h2o.auc(model_withepoch_3,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_3),ValidationLogLoss=h2o.logloss(model_withepoch_3,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#The variance is high, but the AUC of validation has increased and that on training has decreased.
#So let us tune for input_dropout_ratio and later check for number of epochs

model_withepoch_4=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                   ,reproducible = T,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.1)

summary(model_withepoch_4)
results<-data.frame(model=model_withepoch_4@model_id,TrainingAUC=h2o.auc(model_withepoch_4),validationAUC=h2o.auc(model_withepoch_4,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_4),ValidationLogLoss=h2o.logloss(model_withepoch_4,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#Adding Dropout Ratio has decreased Variance. SO let us increase it further

model_withepoch_5=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                   ,reproducible = T,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.15)

summary(model_withepoch_5)
results<-data.frame(model=model_withepoch_5@model_id,TrainingAUC=h2o.auc(model_withepoch_5),validationAUC=h2o.auc(model_withepoch_5,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_5),ValidationLogLoss=h2o.logloss(model_withepoch_5,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#There is a 0.004 decrease in Training AUC, and 0.001 decrease in Validation AUC. 
#The variance has decreased.
#Let us increase epochs in this model and check how the variance changes
model_withepoch_6=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                   ,reproducible = T,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.15,epochs = 50)

summary(model_withepoch_6)
results<-data.frame(model=model_withepoch_6@model_id,TrainingAUC=h2o.auc(model_withepoch_6),validationAUC=h2o.auc(model_withepoch_6,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_6),ValidationLogLoss=h2o.logloss(model_withepoch_6,xval = T))
validationresults<-rbind(validationresults,results)
validationresults


#Increasing epochs has increased the variance slightly, but has decreased the bias buy 0.001 on validation 


#Instead of increasing to 50. let us increase it to 20 and check what happens.
model_withepoch_7=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                   ,reproducible = T,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.15,epochs = 20)

summary(model_withepoch_7)
results<-data.frame(model=model_withepoch_7@model_id,TrainingAUC=h2o.auc(model_withepoch_7),validationAUC=h2o.auc(model_withepoch_7,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_7),ValidationLogLoss=h2o.logloss(model_withepoch_7,xval = T))
validationresults<-rbind(validationresults,results)
validationresults


#There is no change by decreasing the number of epochs to 20. So let us keep the epochs to 20 instead of 50 and increase input dropout ratio
#to 0.2
model_withepoch_8=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                   ,reproducible = T,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2,epochs = 20)

summary(model_withepoch_8)
results<-data.frame(model=model_withepoch_8@model_id,TrainingAUC=h2o.auc(model_withepoch_8),validationAUC=h2o.auc(model_withepoch_8,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_8),ValidationLogLoss=h2o.logloss(model_withepoch_8,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#The training AUC has increased and Validation AUC decrease. Also training log loss has decreased,
#while Validation AUC has increased. This is sign of overfitting.
#This model has a higher variance than previous model. model_withepoch_7 is the best model so far

#What happens if epoch is 10 and input_dropout_ratio is 0.2?
model_withepoch_9=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                   ,reproducible = T,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_9)
results<-data.frame(model=model_withepoch_9@model_id,TrainingAUC=h2o.auc(model_withepoch_9),validationAUC=h2o.auc(model_withepoch_9,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_9),ValidationLogLoss=h2o.logloss(model_withepoch_9,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#This model has a higher variance and bias. Let us try for Number of epochs should be between 10 and 20? Lets try epochs=15

model_withepoch_10=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,epochs=15,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_10)
results<-data.frame(model=model_withepoch_10@model_id,TrainingAUC=h2o.auc(model_withepoch_10),validationAUC=h2o.auc(model_withepoch_10,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_10),ValidationLogLoss=h2o.logloss(model_withepoch_10,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#We can see that the variance has decreased, from previous model. But this model performs worse than 
#DeepLearning_model_R_1481944074353_2121 which is model_withepoch_10.

#Let us add hidden dropout ratio also. This meaans, we have to change the activation function from
#Rectifier to RectifierWithDropout
model_withepoch_11=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.05,0.05),epochs=15,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_11)
results<-data.frame(model=model_withepoch_11@model_id,TrainingAUC=h2o.auc(model_withepoch_11),validationAUC=h2o.auc(model_withepoch_11,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_11),ValidationLogLoss=h2o.logloss(model_withepoch_11,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#This model has a lower variance and bias. Let us further increase the hidden dropout ratio of the first layer

model_withepoch_12=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.1,0.05),epochs=15,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_12)
results<-data.frame(model=model_withepoch_12@model_id,TrainingAUC=h2o.auc(model_withepoch_12),validationAUC=h2o.auc(model_withepoch_12,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_12),ValidationLogLoss=h2o.logloss(model_withepoch_12,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#The AUC has decreased. An variance has come down by 0.001.
#Let us increase the hidden dropout of the first layer further and check if variance comes down

model_withepoch_13=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.15,0.05),epochs=15,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_13)

results<-data.frame(model=model_withepoch_13@model_id,TrainingAUC=h2o.auc(model_withepoch_13),validationAUC=h2o.auc(model_withepoch_13,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_13),ValidationLogLoss=h2o.logloss(model_withepoch_13,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#Thre is a huge drop in variance and decrease in Bias also.
#Let us try increasing number of epochs and check if variance decreased further
model_withepoch_14=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.15,0.05),epochs=20,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_14)
results<-data.frame(model=model_withepoch_14@model_id,TrainingAUC=h2o.auc(model_withepoch_14),validationAUC=h2o.auc(model_withepoch_14,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_14),ValidationLogLoss=h2o.logloss(model_withepoch_14,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#The training AUC has increased, but validation AUC is the same.let us further increase the number of epochs and check

model_withepoch_15=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.15,0.05),epochs=25,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_15)
results<-data.frame(model=model_withepoch_15@model_id,TrainingAUC=h2o.auc(model_withepoch_15),validationAUC=h2o.auc(model_withepoch_15,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_15),ValidationLogLoss=h2o.logloss(model_withepoch_15,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#There is a increase in both bias and variance. So keep epochs as 20 and increase dropout of the second layer

model_withepoch_16=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.15,0.1),epochs=20,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_16)
results<-data.frame(model=model_withepoch_16@model_id,TrainingAUC=h2o.auc(model_withepoch_16),validationAUC=h2o.auc(model_withepoch_16,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_16),ValidationLogLoss=h2o.logloss(model_withepoch_16,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#this model has a slighlty lower variance, the training AUC has decreased and calidation AUC has increased.
#Let us firther increase drop out ratio of the second hidden layer

model_withepoch_17=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.15,0.15),epochs=20,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_17)

results<-data.frame(model=model_withepoch_17@model_id,TrainingAUC=h2o.auc(model_withepoch_17),validationAUC=h2o.auc(model_withepoch_17,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_17),ValidationLogLoss=h2o.logloss(model_withepoch_17,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#this model has a slighlty lower variance, the training AUC has decreased and calidation AUC has increased.
#Let us further increase the hidden dropout ratio of the second layer

model_withepoch_18=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.15,0.2),epochs=20,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_18)
results<-data.frame(model=model_withepoch_18@model_id,TrainingAUC=h2o.auc(model_withepoch_18),validationAUC=h2o.auc(model_withepoch_18,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_18),ValidationLogLoss=h2o.logloss(model_withepoch_18,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#This has further reduced the bias of the model while variance is the same.
#Let us increase the dropout of the second layer further
model_withepoch_19=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.15,0.25),epochs=20,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_19)
results<-data.frame(model=model_withepoch_19@model_id,TrainingAUC=h2o.auc(model_withepoch_19),validationAUC=h2o.auc(model_withepoch_19,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_19),ValidationLogLoss=h2o.logloss(model_withepoch_19,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#This has reduced the variance of the model and also bias of the model.
#Let us further increase the dropout of the second layer
model_withepoch_20=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.15,0.3),epochs=20,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_20)

results<-data.frame(model=model_withepoch_20@model_id,TrainingAUC=h2o.auc(model_withepoch_20),validationAUC=h2o.auc(model_withepoch_20,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_20),ValidationLogLoss=h2o.logloss(model_withepoch_20,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#This model has decreased the trainingAUC but increased validation AUC. This has increased the variance.
#So let us increase dropout of the first hidden layer
model_withepoch_21=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.2,0.3),epochs=20,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_21)
results<-data.frame(model=model_withepoch_21@model_id,TrainingAUC=h2o.auc(model_withepoch_21),validationAUC=h2o.auc(model_withepoch_21,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_21),ValidationLogLoss=h2o.logloss(model_withepoch_21,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#The varinace has slighlty increased. While Bias is almost same.
#Let us increase the number of epochs and see what happens

model_withepoch_22=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.2,0.3),epochs=25,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_22)
results<-data.frame(model=model_withepoch_22@model_id,TrainingAUC=h2o.auc(model_withepoch_22),validationAUC=h2o.auc(model_withepoch_22,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_22),ValidationLogLoss=h2o.logloss(model_withepoch_22,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#Decrease in Bias and variance.Let us increase dropout of the first hidden layer

model_withepoch_23=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.25,0.3),epochs=25,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_23)
results<-data.frame(model=model_withepoch_23@model_id,TrainingAUC=h2o.auc(model_withepoch_23),validationAUC=h2o.auc(model_withepoch_23,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_23),ValidationLogLoss=h2o.logloss(model_withepoch_23,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#Lower Bias and Slighlty lower variance(0.003).This change is negligible. So Lets us change the activation function and see if the Variance decrease

model_withepoch_24=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.25,0.3),epochs=25,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_24)
results<-data.frame(model=model_withepoch_24@model_id,TrainingAUC=h2o.auc(model_withepoch_24),validationAUC=h2o.auc(model_withepoch_24,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_24),ValidationLogLoss=h2o.logloss(model_withepoch_24,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#There is a increase in bias,Before we discard the activation function, Let us decrease the number of epochs
#There is also a increase in variance.
model_withepoch_25=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "TanhWithDropout",hidden_dropout_ratios = c(0.2,0.25),epochs=10,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(model_withepoch_25)
results<-data.frame(model=model_withepoch_25@model_id,TrainingAUC=h2o.auc(model_withepoch_25),validationAUC=h2o.auc(model_withepoch_25,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_25),ValidationLogLoss=h2o.logloss(model_withepoch_25,xval = T))
validationresults<-rbind(validationresults,results)
validationresults
#This model is better than previous model, but not better than the model_23

#Let us add regularisation and try to reduce variance further.
#Lets start with L1 regularisation to model_23
model_withepoch_26=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.25,0.3),epochs=25,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2,l1=0.00001)

summary(model_withepoch_26)
results<-data.frame(model=model_withepoch_26@model_id,TrainingAUC=h2o.auc(model_withepoch_26),validationAUC=h2o.auc(model_withepoch_26,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_26),ValidationLogLoss=h2o.logloss(model_withepoch_26,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#The bias and variance has reduced. So let us increase l1 further
#But the decrease in variance not as significant as in model_23
model_withepoch_27=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.25,0.3),epochs=25,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2,l1=0.00002)

summary(model_withepoch_27)
results<-data.frame(model=model_withepoch_27@model_id,TrainingAUC=h2o.auc(model_withepoch_27),validationAUC=h2o.auc(model_withepoch_27,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_27),ValidationLogLoss=h2o.logloss(model_withepoch_27,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#Bias and variance reduced, let us increase l1 further

model_withepoch_28=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.25,0.3),epochs=25,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2,l1=0.00003)

summary(model_withepoch_28)

results<-data.frame(model=model_withepoch_28@model_id,TrainingAUC=h2o.auc(model_withepoch_28),validationAUC=h2o.auc(model_withepoch_28,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_28),ValidationLogLoss=h2o.logloss(model_withepoch_28,xval = T))

validationresults<-rbind(validationresults,results)
validationresults
#The training AUC increased, but validation AUC decreased, sign of overfitting.
#So let us stick with l1=0.00002
#Lets add l2 regularisation

model_withepoch_29=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.25,0.3),epochs=25,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2,l1=0.00002,l2=0.00001)

summary(model_withepoch_29)
results<-data.frame(model=model_withepoch_29@model_id,TrainingAUC=h2o.auc(model_withepoch_29),validationAUC=h2o.auc(model_withepoch_29,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_29),ValidationLogLoss=h2o.logloss(model_withepoch_29,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#Compared to model_27, the training AUC has increased, but validation AUC has decreased.
#Looks like overfitting. But we will verify by increasing l2 further and seeif overfitting happens.
#If yed then we wnt add l2 norm
model_withepoch_30=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.25,0.3),epochs=25,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2,l1=0.00002,l2=0.00002)

summary(model_withepoch_30)
results<-data.frame(model=model_withepoch_30@model_id,TrainingAUC=h2o.auc(model_withepoch_30),validationAUC=h2o.auc(model_withepoch_30,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_30),ValidationLogLoss=h2o.logloss(model_withepoch_30,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#We cab see that that training AUC has decreased and Validation AUC has increased.
#Let us increase the l2 further
model_withepoch_31=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                                    ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.25,0.3),epochs=25,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2,l1=0.00002,l2=0.00003)

summary(model_withepoch_31)

results<-data.frame(model=model_withepoch_31@model_id,TrainingAUC=h2o.auc(model_withepoch_31),validationAUC=h2o.auc(model_withepoch_31,xval=T),TrainingLogLoss=h2o.logloss(model_withepoch_31),ValidationLogLoss=h2o.logloss(model_withepoch_31,xval = T))
validationresults<-rbind(validationresults,results)
validationresults

#Increaase in Bias. So we will stick with l2=0.0002

#After applying regularisation, we can see that model_withepoch_2 has a lower variance and the bias is almost the same
#for the models.Let us verify this by plotting the results
#We will calculate the variance as the difference in Training and Validation AUC.
#Bias is the error. An ideal situtaion is when AUC is 1. So bias=1-AUC. But, we want bias in same scale as variance and hence we divide bias by 10
#We calculate the training and the validation bias.

#Write the validation results

write.csv(validationresults,"validationResults_WithEpoch.csv",row.names = F)
validationresults_withepoch<-read.csv("validationResults_WithEpoch.csv")
validationresults_withepoch$variance= validationresults_withepoch$TrainingAUC - validationresults_withepoch$validationAUC
validationresults_withepoch$validationBias=(1 - validationresults_withepoch$validationAUC)/10
validationresults_withepoch$trainingBias= (1 - validationresults_withepoch$TrainingAUC)/10

plot(validationresults_withepoch$variance,col="red",type="l")
par(new=T)
lines(validationresults_withepoch$validationBias,col="blue",type="l")
par(new=T)
lines(validationresults_withepoch$trainingBias,col="green",type="l")
legend(x=13,y=0.07,legend = c("Variance","ValidationBias","TrainBias"),col=c("red","blue","green"),lty = 1)

#Model_23 is the lowest variance and is best model 
#Let us verify the model stability using the test set
test_performance=h2o.performance(model_withepoch_23,newdata = test)
test_performance@metrics$AUC #0.8424112
test_performance@metrics$logloss #0.4126046

#For this model, let us plot the ROC curve and identify,cutoffs for having high sensitivity
plot(test_performance,type="roc",col="red",typ="l")
par(new=T)
plot(h2o.performance(model_withepoch_23,newdata=train),type="roc",col="blue",typ="l")
legend(x=0.7,y=0.3,legend = c("Test","Train"),col=c("red","blue"),lty = 1)
#The logloss on validation and test data is almost same, while Validation AUC is slightly higher
#Than test

#Let us create a confusion matrix to calculate accuracy,sensitivity and specificity
test_pred<-h2o.predict(model_withepoch_23,newdata = test)
test_pred<-test_pred[,3]
test_pred<-as.data.frame(test_pred)

test_churn<-as.data.frame(test$Churn)
test_churn$Churn<- ifelse(test_churn$Churn=="No", 0, 1)
library(caret)
confusionMatrix(as.numeric(test_pred$Yes> 0.3),test_churn$Churn,positive = "1")
#Accuracy : 0.7683 
#Sensitivity : 0.7273  
#Specificity : 0.7819

confusionMatrix(as.numeric(test_pred$Yes> 0.2),test_churn$Churn,positive = "1")
#Accuracy : 0.7085
#Sensitivity : 0.8471  
#Specificity : 0.6626 

confusionMatrix(as.numeric(test_pred$Yes> 0.16),test_churn$Churn,positive = "1")
#Specificity : 0.5830
#Sensitivity: 0.9132
#Accuracy : 0.6653 

confusionMatrix(as.numeric(test_pred$Yes> 0.5),test_churn$Churn,positive = "1")
#Accuracy : 0.8033
#Sensitivity : 0.4917
#Specificity : 0.9067


confusionMatrix(as.numeric(test_pred$Yes> 0.6),test_churn$Churn,positive = "1")
#Sensitivity : 0.38430 
#Specificity : 0.94650
#Accuracy : 0.8064 

confusionMatrix(as.numeric(test_pred$Yes> 0.7),test_churn$Churn,positive = "1")
#Specificity : 0.98217
#Sensitivity : 0.14876        
#Accuracy : 0.7745 


#We want to maximise sensitivity. Hence, we chose a threshold of 0.2.
#This model does much better than the model, developed without tuning for epochs.

#We can see that variance with epochs is much lower than models without using epochs.


#--------END OF MODEL BUILDING-----------------------------------------------------------------------------
 
#The final model is model_23_withepoch. This model has the least variance and lower bias among all other models.
Final_Model=h2o.deeplearning(x=features,y=target,training_frame = train,nfolds = 5,seed=100
                             ,reproducible = T,activation = "RectifierWithDropout",hidden_dropout_ratios = c(0.25,0.3),epochs=25,stopping_metric = "AUC",hidden = c(35,35),input_dropout_ratio = 0.2)

summary(Final_Model)

#Let us plot the ROC Curves of the final model
test_performance=h2o.performance(Final_Model,newdata = test)
test_performance@metrics$AUC #0.8424112
test_performance@metrics$logloss #0.4126046

#For this model, let us plot the ROC curve and identify,cutoffs for having high sensitivity
plot(test_performance,type="roc",col="red",typ="l")
par(new=T)
plot(h2o.performance(Final_Model,newdata=train),type="roc",col="blue",typ="l")
par(new=T)
plot(h2o.performance(Final_Model,xval = T),type="roc",col="green",typ="l")
legend(x=0.6,y=0.4,legend = c("Test","Train","Cross Validation"),col=c("red","blue","green"),lty = 1)

#As discussed before,
#We want to maximise Sensitivity and hence we will choose a lower threshold.

test_pred<-h2o.predict(Final_Model,newdata = test)
test_pred<-test_pred[,3]
test_pred<-as.data.frame(test_pred)

test_churn<-as.data.frame(test$Churn)
test_churn$Churn<- ifelse(test_churn$Churn=="No", 0, 1)
library(caret)
confusionMatrix(as.numeric(test_pred$Yes> 0.3),test_churn$Churn,positive = "1")
#Accuracy : 0.7683 
#Sensitivity : 0.7273  
#Specificity : 0.7819

confusionMatrix(as.numeric(test_pred$Yes> 0.2),test_churn$Churn,positive = "1")
#Accuracy : 0.7085
#Sensitivity : 0.8471  
#Specificity : 0.6626 

confusionMatrix(as.numeric(test_pred$Yes> 0.16),test_churn$Churn,positive = "1")
#Specificity : 0.5830
#Sensitivity: 0.9132
#Accuracy : 0.6653 

confusionMatrix(as.numeric(test_pred$Yes> 0.5),test_churn$Churn,positive = "1")
#Accuracy : 0.8033
#Sensitivity : 0.4917
#Specificity : 0.9067


confusionMatrix(as.numeric(test_pred$Yes> 0.6),test_churn$Churn,positive = "1")
#Sensitivity : 0.38430 
#Specificity : 0.94650
#Accuracy : 0.8064 

confusionMatrix(as.numeric(test_pred$Yes> 0.7),test_churn$Churn,positive = "1")
#Specificity : 0.98217
#Sensitivity : 0.14876        
#Accuracy : 0.7745 


#We want to maximise sensitivity and at same time have good accuracy. Hence, we chose a threshold of 0.2.
#Accuracy : 0.7085
#Sensitivity : 0.8471  
#Specificity : 0.6626 
