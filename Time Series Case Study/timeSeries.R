
#The Aim of this work is to forecast sales and demand for next 6 months


library(data.table)
library(dplyr)
library(ggplot2)


data<-fread("Global Superstore.csv")
str(data)
#51290 observations and 24 variables

#What are the different market segments
market_segment<-unique(data[,"Market",with=F])
market_segment
#The 7 unique market segments are US,APAC,AFRICA,EMEA,LATAM,Canada

#What are the different product Segments
product_segment<-unique(data[,"Segment",with=F])
product_segment
#Consumer,Corporate and HomeOffice

#What are the different buckets that are to be analysed?
buckets<-unique(data[,c("Market","Segment"),with=F])
buckets
#We have 21 different Buckets.

#From Order date, lets extract the Month order was placed
data[,Order_Month:=month(`Order Date`)]
data$Order_Month<-month.abb[data$Order_Month] #Converts the months in numbers to mabbreviated months


#-------------------Profit Aggregation-----------------------------------------------------------------------
#We have to select the most profitable segment and predict the sales and demand for the top 5 segment

#Lets us get the aggregated profit for each month for each market and each segment
aggregatedProfit<-data %>%select(Market,Segment,Profit,Order_Month) %>%group_by(Market,Segment,Order_Month) %>% mutate(MonthlyProfit=sum(Profit))
aggregatedProfit<-aggregatedProfit %>% select(Market,Segment,Order_Month,MonthlyProfit)
aggregatedProfit<-unique(aggregatedProfit)

#Let us plot the Monthly Profit for different segments and market
aggregatedProfit$Order_Month<-factor(aggregatedProfit$Order_Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

ggplot(aggregatedProfit,aes(x=Order_Month,y=MonthlyProfit,group=1))+geom_point()+geom_line()+facet_grid(Market~Segment)

#From. the plot we can see that canada is not doing weel.Its monthly profit is almost a straight line.
#From the plot, we also observe that home offiece is not doing great for all the 7 market segments

#Lets split the aggregated profit into multiple dataframes,one for each bucket
profit_bucket<-split(aggregatedProfit,with(aggregatedProfit,interaction(Market,Segment)),drop=T)
length(profit_bucket) #21, same as the number of buckets identified

#Lets us create a time series for each of these profit buckets 
coeff_variation_profit<-data.frame(Market=character(),Segment=character(),coeffOfVariation=double())


for(i in 1:length(profit_bucket)){
  market<-unique(profit_bucket[[i]]$Market)
  segment<-unique(profit_bucket[[i]]$Segment)
  dat <- data.frame(profit_bucket[[i]]) %>% select(Order_Month,MonthlyProfit)
 
  coefficient_variation<-sd(dat$MonthlyProfit)/mean(dat$MonthlyProfit)  
  coefficient_variation
 
  coeff_variation_profit<-rbind(coeff_variation_profit,do.call(data.frame,setNames(as.list(c(market,segment,coefficient_variation)),names(coeff_variation_profit))))
  
}
coeff_variation_profit$coeffOfVariation<-as.numeric(as.character(coeff_variation_profit$coeffOfVariation))
coeff_variation_profit$coeffOfVariation
coeff_variation_profit<-coeff_variation_profit[order(coeff_variation_profit$coeffOfVariation),]



#What are the 5 most profitable segments ?

#We want to find segment which is consistently profitable which means, it has a high mean and low standard deviation
#Hence, we will choose the segment with the least coefficient of variation
coeff_variation_profit[1:5,]

#Hence we will choose the following segments for further Analysis
segments<-data.frame(Market=coeff_variation_profit[1:5,"Market"],Segment=coeff_variation_profit[1:5,"Segment"])
segments

#-----------------End of Profit Aggregation------------------------------------------------------------------------------------------------------------------------------------

#-----------------Sales Aggregation-----------------------------------------------------------------------------------

#Lets us get the aggregated Sales for each month for each market and each segment
aggregatedSales<-data %>%select(Market,Segment,Sales,Order_Month) %>%group_by(Market,Segment,Order_Month) %>% mutate(MonthlySales=sum(Sales))
aggregatedSales<-aggregatedSales%>% select(Market,Segment,Order_Month,MonthlySales)
aggregatedSales<-unique(aggregatedSales)
nrow(aggregatedSales)
aggregatedSales


#Aggregated Yearly Sales ( To identify yearly trends)

#Let us plot the aggregated Sales for each Market and each Segment
ggplot(aggregatedSales,aes(x=Order_Month,y=MonthlySales,group=1))+geom_point()+geom_line()+facet_grid(Market~Segment)



#Let us get the aggregated Quantity for each month for each market and each segment

aggregatedQuantity<-data %>%select(Market,Segment,Quantity,Order_Month) %>%group_by(Market,Segment,Order_Month) %>% mutate(MonthlyQuantity=sum(Quantity))
aggregatedQuantity<-aggregatedQuantity%>% select(Market,Segment,Order_Month,MonthlyQuantity)
aggregatedQuantity<-unique(aggregatedQuantity)
nrow(aggregatedQuantity)
aggregatedQuantity

ggplot(aggregatedQuantity,aes(x=Order_Month,y=MonthlyQuantity,group=1))+geom_point()+geom_line()+facet_grid(Market~Segment)

#Lets merge aggregated Profit,Sales and Quantity and plot the time series
aggregatedData<-merge(aggregatedProfit,aggregatedSales,by=c("Market","Segment","Order_Month"),all=T)
aggregatedData<-merge(aggregatedData,aggregatedQuantity,by=c("Market","Segment","Order_Month"),all=T)
aggregatedData

#Lets us normalise Monthly Sales,Profit and Quantity
aggregatedData$MonthlyProfit<-scale(aggregatedData$MonthlyProfit)
aggregatedData$MonthlySales<-scale(aggregatedData$MonthlySales)
aggregatedData$MonthlyQuantity<-scale(aggregatedData$MonthlyQuantity)

plot<-ggplot(aggregatedData,aes(Order_Month,group=1))+geom_point(aes(y=MonthlyProfit))+geom_line(aes(y=MonthlyProfit,colour="MonthlyProfit"))
plot<-plot+geom_point(aes(y=MonthlySales))+geom_line(aes(y=MonthlySales,colour="MonthlySales"))+facet_grid(Market~Segment)
plot<-plot+geom_point(aes(y=MonthlyQuantity))+geom_line(aes(y=MonthlyQuantity,colour="MonthlyQuantity"))
plot<-plot+scale_color_manual(values=c("red", "blue", "green"))
plot


#Let us only select data for the 5 buckets identified from the data


subset<-data[Market %in% segments$Market & Segment %in% segments$Segment,]

subset<-subset %>% select(Market,Segment,`Order Date`,Order_Month,Sales,Quantity)



#-----------------------END OF DATA PREPARATION------------------------------------------------------------

subset$`Order Date`<-as.Date(subset$`Order Date`,"%d-%m-%Y")

head(subset)
summary(subset$`Order Date`)

#Lets Extract Month and Year from Order Date
subset$Order_Year<-year(subset$`Order Date`)

#Lets get the monthly sales for each of the 5 segments for each month of each year
aggregated_subset_sales<-subset %>%select(Market,Segment,Sales,Order_Month,Order_Year,`Order Date`) %>%group_by(Market,Segment,Order_Month,Order_Year) %>% mutate(MonthlySales=sum(Sales))
aggregated_subset_sales


aggregated_subset_quantity<-subset %>% select(Market,Segment,Quantity,Order_Month,Order_Year,`Order Date`) %>% group_by(Market,Segment,Order_Month,Order_Year) %>% mutate(MonthlyDemand=sum(Quantity))
aggregated_subset_quantity

#Split the data into train and test. The test data has the last 6 months data
#test_data<-aggregated_subset_sales[`Order Date`>="2014-07-01",]
#train_data<-aggregated_subset_sales[`Order Date`<"2014-07-01",]

#Now for each segment, apply time series


#--------------TIME SERIES ANALYSIS MODELLING-----------------------------------------------------
getSeries<- function(targetMarket,targetSegment,sales=1){
  
  if(sales==1){
  series<-aggregated_subset_sales %>% filter(Market==targetMarket & Segment==targetSegment) %>%  select(`Order Date`,MonthlySales)  
  series$Month<-as.Date(cut(series$`Order Date`,breaks="month"))
  series<-series[,c("Month","MonthlySales"),with=F]
    
    }
  if(sales==0){
  series<-aggregated_subset_quantity %>% filter(Market==targetMarket & Segment==targetSegment) %>%  select(`Order Date`,MonthlyDemand)
  series$Month<-as.Date(cut(series$`Order Date`,breaks="month"))
  series<-series[,c("Month","MonthlyDemand"),with=F]
  }
  
  series<- unique(series)
  
  series<-series[order(Month,decreasing = F),]    
  month=as.numeric(row.names(series))
  series$Month<-NULL
  series$Month<-month
  return(series)
  
}

plotTimeSeries<-function(series,title,sales=1){
  if(sales==1){
    timeser<-ts(series$MonthlySales)
  }
  if(sales==0){
    timeser<-ts(series$MonthlyDemand)
  }
  
  plot(timeser,col="red",main=title)
}


smoothen<-function(timeser,windowSize=3){
 

 smooth<- stats::filter(timeser,filter = rep(1/windowSize,windowSize),method="convolution",sides=2)
 #Fill the first and last NA. 
 diff_1<- smooth[3] - smooth[2]
 smooth[1]<- smooth[2] - diff_1
 
 diff_2<-smooth[length(smooth) -1] -smooth[length(smooth)-2]
 smooth[length(smooth)]<-smooth[length(smooth)-1] + diff_2
 

  return(smooth)
}

plotExponential<-function(timeser,alphas,title=""){
  plot(timeser,main=title)
  cols<-c('red', 'blue', 'green', 'black')
  
  labels <- c(paste('alpha =', alphas), 'Original')
  for(i in seq(1 , length(alphas))){
    smoothedseries<-HoltWinters(timeser,alpha = alphas[i],beta = F,gamma = F)
    lines(fitted(smoothedseries)[,1],col=cols[i],lwd=2)
    
  }
  legend("topleft", labels,col=cols, lwd=2,pt.cex = 1,cex=0.5)
}

plotResidual<-function(timeser,trend,title=""){
  par(mfrow=c(2,2))
  
  resi <- timeser - trend
  ylim<-c(min(resi),max(timeser))
  
 
  acf(resi)
  acf(resi, type="partial")
  
  plot(timeser, col='red',ylim=ylim,main=title)
  lines(trend,col="blue")
  lines(resi,col="green")
  legend("topleft",col=c("red","blue","green"),legend = c("Actual Series","Trend Line","Residual"),lty=1,pt.cex = 1,cex=0.3)
  
}
#-----------------------_END OF FUNCTIONS------------------------------------------------------

#---------------------EU Consumer Segment Time Series Analysis---------------------------------------------
EU_Consumer_Sales<-getSeries("EU","Consumer",sales=1)
EU_Consumer_Demand<-getSeries("EU","Consumer",sales=0)

#Convert to timeseries
EU_Consumer_Sales_timeser<-ts(EU_Consumer_Sales$MonthlySales)
EU_Consumer_Demand_timeser<-ts(EU_Consumer_Demand$MonthlyDemand)

plot(EU_Consumer_Sales_timeser,col="red",main="EU Consumer Sales Time Series")
#There is no seasonal pattern. But there is an upward trend


#-----------BEGIN STATIONARISE----------------------------------------------------------------
#Let us check if the Sales timeseries is stationary or not. 
#To check for stationarity, we will plot the ACF and PACF ploy.
#If the lags die out quickly then it is a sttionary series

acf(EU_Consumer_Sales_timeser)
pacf(EU_Consumer_Sales_timeser)
#From the plot we can see that series is not stationary

kpss.test(EU_Consumer_Sales_timeser)
#The p value is smaller indicating that series is not stationary.

library(astsa)
library(forecast)

plot(diff(EU_Consumer_Sales_timeser))
acf2(diff(EU_Consumer_Sales_timeser))

kpss.test(diff(EU_Consumer_Sales_timeser))
#Large p valu indicates that the differential of series,is stationary. So we will contunue with this series

EU_Consumer_Sales_timeser<-diff(EU_Consumer_Sales_timeser) #Stationarised Series


#Stationarising the Demand

#Let us check if the Demand timeseries is stationary or not. 
#To check for stationarity, we will plot the ACF and PACF ploy.
#If the lags die out quickly then it is a sttionary series

acf(EU_Consumer_Demand_timeser)
pacf(EU_Consumer_Demand_timeser)
#From the plot we can see that series is not stationary

kpss.test(EU_Consumer_Demand_timeser)
#The p value is smaller indicating that series is not stationary.




plot(diff(EU_Consumer_Demand_timeser))
acf2(diff(EU_Consumer_Demand_timeser))

kpss.test(diff(EU_Consumer_Demand_timeser))
#Large p valu indicates that the differential of series,is stationary. So we will contunue with this series

EU_Consumer_Demand_timeser<-diff(EU_Consumer_Demand_timeser) #Stationarised Series

#------END STATIONARISING-----------------------------------------------------------------

#Let us smooth Sales and Demand Time Series. Lets smmoth the sales using different window size and decide on the optial window size
par(mfrow=c(1,1))


fit<-lm(EU_Consumer_Sales_timeser~c(1:47))
summary(fit)
  
plot(EU_Consumer_Sales_timeser,col="red",main="Monthly Sales in EU Consumer Segment for Different Window Size")
#We can see that ths series can be explained by a additive model as the random fluctuations are constant over time
abline(fit,col="blue")

library(forecast)
#Let us apply moving average

EU_Consumer_Sales_smooth<-smoothen(timeser = EU_Consumer_Sales_timeser,windowSize = 3)
plot(EU_Consumer_Sales_timeser)
lines(EU_Consumer_Sales_smooth,col="brown")
#The curve is smooth, but tends to have a forward lag.

#The month should be from 1 to 48 which is nthing must the row names of the EU_Cosumer_Sales, which is sorted in increasing Order of Month

month<-c(1:length(EU_Consumer_Sales_smooth))
month
sales<-EU_Consumer_Sales_smooth

head(sales)

EU_Consumer_Sales_SmoothedSeries<-data.frame("month"=month,"sales"=sales)
View(EU_Consumer_Sales_SmoothedSeries)


plot(EU_Consumer_Sales_timeser,col="red",main="Monthly Sales in EU Consumer Segment")

lines(EU_Consumer_Sales_SmoothedSeries$sales,col="blue")

legend("topleft",col=c("red","blue"),legend = c("Actual","Smoothed"),lty=1)

#------SMOOTHING FOR DEMAND-------------------------------------------------------------------------------


par(mfrow=c(2,1))
plotTimeSeries(EU_Consumer_Demand,title = "Monthly Demand in EU Consumer Segment",sales =0)

plotTimeSeries(EU_Consumer_Demand,title="Monthly Demand in EU Consumer Segment",sales=0)


#Let us smooth Demand and Demand Time Series. Lets smmoth the Demand using different window size and decide on the optial window size
par(mfrow=c(1,1))

EU_Consumer_Demand_timeser<-ts(EU_Consumer_Demand$MonthlyDemand)

plot(EU_Consumer_Demand_timeser,col="red",main="Monthly Demand in EU Consumer Segment for Different Window Size")

lines(smoothen(EU_Consumer_Demand_timeser,windowSize = 3),col="purple")

lines(smoothen(EU_Consumer_Demand_timeser,windowSize = 2),col="blue")

lines(smoothen(EU_Consumer_Demand_timeser,windowSize = 4),col="green")

lines(smoothen(EU_Consumer_Demand_timeser,windowSize = 5),col="brown")

legend("topleft",col=c("red","purple","blue","green","brown"),legend = c("Actual Series","Window Size 3","Window Size 2","Window Size 4","Window Size 5"),lty=1,pt.cex = 1,cex=0.7)

#Window Size 3 seems OK.
EU_Consumer_Demand_smooth<-smoothen(EU_Consumer_Demand$MonthlyDemand,windowSize = 3)

#Lets try exponential smoothing nd compare
plotExponential(EU_Consumer_Sales_timeser,alphas=c(0.8,0.4,0.3))
#Alpha=0.3 seems to be good. Lets see if it is better than moving average window
plot(EU_Consumer_Sales_timeser,col="red")
smoothedseries<-HoltWinters(EU_Consumer_Sales_timeser,alpha = 0.4,beta = F,gamma = F)
lines(fitted(smoothedseries)[,1],col="blue",lwd=2)
lines(smoothen(EU_Consumer_Sales_timeser,windowSize = 5),col="brown")

#The month should be from 1 to 48 which is nthing must the row names of the EU_Cosumer_Demand, which is sorted in increasing Order of Month

month<-as.numeric(row.names(EU_Consumer_Demand))
month
Demand<-EU_Consumer_Demand_smooth

head(Demand)

EU_Consumer_Demand_SmoothedSeries<-data.frame("month"=month,"Demand"=Demand)
View(EU_Consumer_Demand_SmoothedSeries)


plot(EU_Consumer_Demand_timeser,col="red",main="Monthly Demand in EU Consumer Segment")

lines(EU_Consumer_Demand_SmoothedSeries$Demand,col="blue")

legend("topleft",col=c("red","blue"),legend = c("Actual","Smoothed"),lty=1)

#-------------END SMOOTHING FOR DEMAND---------------------------------------------------------------------------

#-----STEP 2 : BEST REGRESSION AND ARIMA FIT--------------------------------------------------------------

#Split into train and test

train_data_EU_Consumer_Sales<-EU_Consumer_Sales_SmoothedSeries[1:42,]
test_data_EU_Consumer_Sales<-EU_Consumer_Sales_SmoothedSeries[43:nrow(EU_Consumer_Sales_SmoothedSeries),]
View(test_data_EU_Consumer_Sales)

train_data_EU_Consumer_Demand<-EU_Consumer_Demand_SmoothedSeries[1:42,]
test_data_EU_Consumer_Demand<-EU_Consumer_Demand_SmoothedSeries[43:nrow(EU_Consumer_Demand_SmoothedSeries),]
View(test_data_EU_Consumer_Demand)

#----------------SALES REGRESSION FIT------------------------------------------------------------------------

#Applying Linear Regression to Time Series in EU Consumer Segment.

EU_Consumer_Sales_Lmfit<-lm(sales~sin(month)+month,data=train_data_EU_Consumer_Sales)
summary(EU_Consumer_Sales_Lmfit)
#Let us see the trend on train data
EU_Consumer_Sales_trend<-predict(EU_Consumer_Sales_Lmfit,train_data_EU_Consumer_Sales)

plot(train_data_EU_Consumer_Sales$month,train_data_EU_Consumer_Sales$sales,col="red",lty=1,type="n",main="EU Consumer Segment Trend Line")
lines(train_data_EU_Consumer_Sales$month,train_data_EU_Consumer_Sales$sales,col="red")
lines(EU_Consumer_Sales_trend,col="blue")
legend("bottomright",col=c("red","blue"),legend = c("Actual Series","Linear Fit Trend Line"),lty=1)


#Let us forecast on the remaining 6 months data and predict the Sales Trend
EU_Consumer_Sales_test_trend<-predict(EU_Consumer_Sales_Lmfit,newdata = test_data_EU_Consumer_Sales)

#Let us plot the entire sales time series and the trend lines for complete 48 months

EU_Consumer_Sales_trend<-predict(EU_Consumer_Sales_Lmfit,newdata =EU_Consumer_Sales_SmoothedSeries)
plot(EU_Consumer_Sales_SmoothedSeries$month,EU_Consumer_Sales_SmoothedSeries$sales,col="red",lty=1,type="n",main="Trend for Sales in EU Consumer Segment")
lines(EU_Consumer_Sales_SmoothedSeries$month,EU_Consumer_Sales_SmoothedSeries$sales,col="red")
lines(EU_Consumer_Sales_trend,col="blue")
legend("topleft",col=c("red","blue"),legend = c("Smoothed Sales","Sales Trend Line"),lty=1)


#----------------END SALES REGRESSION FIT----------------------------------------------------------------



#----STEP 3 Check Residual For White Noise----------------------------------

#Residual Plot for EU Consumer Segment Sales Linear Fit
plotResidual(EU_Consumer_Sales_timeser,trend = EU_Consumer_Sales_trend,title="EU Consumer Sales Residual Plot")




