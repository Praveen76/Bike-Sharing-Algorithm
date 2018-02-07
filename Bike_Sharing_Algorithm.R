rm(list=ls())
# Step 1. Hypothesis Generation
# Here are some of the hypothesis which I thought could influence the demand of bikes:
  
# 1.Hourly trend: There must be high demand during office timings. Early morning and late evening can have different trend (cyclist) and low demand during 10:00 pm to 4:00 am.
# 2.Daily Trend: Registered users demand more bike on weekdays as compared to weekend or holiday.
# 3.Rain: The demand of bikes will be lower on a rainy day as compared to a sunny day. Similarly, higher humidity will cause to lower the demand and vice versa.
# 4.Temperature: In India, temperature has negative correlation with bike demand. But, after looking at Washington's temperature graph, I presume it may have positive correlation.
# 5.Pollution: If the pollution level in a city starts soaring, people may start using Bike (it may be influenced by government / company policies or increased awareness).
# 6.Time: Total demand should have higher contribution of registered user as compared to casual because registered user base would increase over time.
# 7.Traffic: It can be positively correlated with Bike demand. Higher traffic may force people to use bike as compared to other road transport medium like car, taxi etc



# Step  2. Understanding the Data Set
# Step  3. Importing Data set , Libraries and Basic Data Exploration

#loading the required libraries
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(caret)

setwd("C:/R/Analytics Vidhya/Bike Sharing Competition/")

#1. Import Train and Test Data Set
train=read.csv("C:/R/Analytics Vidhya/Bike Sharing Competition/train.csv")
test=read.csv("C:/R/Analytics Vidhya/Bike Sharing Competition/test.csv")
str(train)
str(test)

# 2.Combine both Train and Test Data set (to understand the distribution of independent variable together).
# introducing variables in test to combine train and test
# can also be done by removing the same variables from training data

test$registered=0
test$casual=0
test$count=0
data=rbind(train,test)

str(data)
summary(data)
unique(data$atemp)
unique(data$season)
unique(data$weather)
unique(data$holiday)
unique(data$workingday)
# 3. Find missing values in data set if any.
colSums(is.na(data))


# 4.Understand the distribution of numerical variables and generate a frequency table for numeric variables.  
# Now, I'll test and plot a histogram for each numerical variables and analyze the distribution.

# 
par(mfrow=c(4,2))
par(mar = rep(2, 4))
hist(data$season)
hist(data$weather)
hist(data$humidity)
hist(data$holiday)
hist(data$workingday)
hist(data$temp)
hist(data$atemp)
hist(data$windspeed)

# Few inferences can be drawn by looking at the these histograms:
#   
#   Season has four categories of almost equal distribution
# Weather 1 has higher contribution i.e. mostly clear weather.
prop.table(table(data$weather))

# As expected, mostly working days and variable holiday is also showing a similar inference. You can use the code above to look at the distribution in detail. Here you can generate a variable for weekday using holiday and working day. Incase, if both have zero values, then it must be a working day.
# Variables temp, atemp, humidity and windspeed  looks naturally distributed.

#5. Convert discrete variables into factor (season, weather, holiday, workingday)
# factoring some variables from numeric
  data$season=as.factor(data$season)
  data$weather=as.factor(data$weather)
  data$holiday=as.factor(data$holiday)
  data$workingday=as.factor(data$workingday)

  
  str(data)
  


# Step 4.Hypothesis Testing (using multivariate analysis)
  # Till now, we have got a fair understanding of the data set. Now, let's test the hypothesis which we had generated earlier.  
  # Here I have added some additional hypothesis from the dataset. Let's test them one by one:
    
    # 4.1.Hourly trend: We don't have the variable 'hour' with us right now. But we can extract it using the datetime column.
       unique(data$datetime)
  data$hour=substr(data$datetime,12,16) # extracting hour from the datetime variable(hour is starting from 12th digit in datetime and ending on 13th digit )
  unique(data$hour)
    data$hour=as.factor(data$hour)
  


  # 4.2.Let's plot the hourly trend of count over hours and check if our hypothesis is correct or not. We will separate train and test data set from combined one.
  
  train=data[as.integer(substr(data$datetime,9,10))<20,]
  test=data[as.integer(substr(data$datetime,9,10))>19,]
  
  # creating some boxplots on the count of rentals
  boxplot(train$count~train$hour,xlab="hour", ylab="count of users")
  
  
  # Above, you can see the trend of bike demand over hours. Quickly, I'll segregate the bike demand in three categories:
    
  #   High       : 7-9 and 17-19 hours
  # Average  : 10-16 hours
  # Low         : 0-6 and 20-24 hours
  # Here I have analyzed the distribution of total bike demand. Let's look at the distribution of registered and casual users separately.
    
    
  boxplot(train$casual~train$hour,xlab="hour", ylab="casual users")
  boxplot(train$registered~train$hour,xlab="hour", ylab="registered users")
  
    # Above you can see that registered users have similar trend as count. Whereas, casual users have different trend. Thus, we can say that 'hour' is significant variable and our hypothesis is 'true'.
    # You might have noticed that there are a lot of outliers while plotting the count of registered and casual users. These values are not generated due to error, so we consider them as natural outliers. 
    # They might be a result of groups of people taking up cycling (who are not registered). To treat such outliers, we will use logarithm transformation. Let's look at the similar plot after log transformation.
    
    boxplot(log(train$count)~train$hour,xlab="hour",ylab="log(count)")
    
    # 4.3.Daily Trend: Like Hour, we will generate a variable for day from datetime variable and after that we'll plot it.
    
    date=substr(data$datetime,1,10) # extracting days of week from datetime
    days<-weekdays(as.Date(date))
    data$day=days
    
    train=data[as.integer(substr(data$datetime,9,10))<20,]
    test=data[as.integer(substr(data$datetime,9,10))>19,]
    
    # Below Plot shows registered and casual users' demand over days.
    boxplot(data$casual ~ data$day,xlab="casual", ylab="count of casual users")  
    boxplot(data$registered ~ data$day,xlab="registered", ylab="count of registered users")  
    
    # While looking at the plot, I can say that the demand of causal users increases over weekend.
    
    # 4.4. Rain: We don't have the 'rain' variable with us but have 'weather' which is sufficient to test our hypothesis. As per variable description, weather 3 represents light rain and weather 4 represents heavy rain. Take a look at the plot:
    boxplot(data$casual ~ data$weather,xlab="casual", ylab="count of casual users")  
    boxplot(data$registered ~ data$weather,xlab="registered", ylab="count of registered users")  
    
    # 4.5. Temperature
    boxplot(train$registered~train$temp,xlab="temp", ylab="registered users")
    boxplot(train$casual~train$temp,xlab="temp", ylab="casual users")
    
    
    # It is clearly satisfying our hypothesis.
    
    # 4.6.Temperature, Windspeed and Humidity: These are continuous variables so we can look at the correlation factor to validate hypothesis.
    
    sub=data.frame(train$registered,train$casual,train$count,train$temp,train$humidity,train$atemp,train$windspeed)
    cor(sub) #Plot Correlation Matrix
    
    # Here are a few inferences you can draw by looking at the above histograms:
    #   
    #   Variable temp is positively correlated with dependent variables (casual is more compare to registered)
    # Variable atemp is highly correlated with temp.
    # Windspeed has lower correlation as compared to temp and humidity
    
    # 4.7.Time: Let's extract year of each observation from the datetime column and see the trend of bike demand over year.
    
    data$year=substr(data$datetime,1,4) # extracting year from data
    data$year=as.factor(data$year)
    
    train=data[as.integer(substr(data$datetime,9,10))<20,]
    test=data[as.integer(substr(data$datetime,9,10))>19,]
    
    # again some boxplots with different variables
    # these boxplots give important information about the dependent variable with respect to the independent variables
    
    boxplot(train$registered~train$year,xlab="year", ylab="registered users")
    boxplot(train$casual~train$year,xlab="year", ylab="casual users")
    
    
    
    
    boxplot(train$registered~train$windspeed,xlab="year", ylab="registered users")
    boxplot(train$casual~train$windspeed,xlab="year", ylab="casual users")
    
    boxplot(train$registered~train$humidity,xlab="humidity", ylab="registered users")
    boxplot(train$casual~train$humidity,xlab="humidity", ylab="casual users")
    
    boxplot(train$count~train$year,xlab="year", ylab="count")
    
    # You can see that 2012 has higher bike demand as compared to 2011.
    data$hour=as.integer(data$hour)
    
    
    
    #4.7. monthly trend
    data$month=substr(data$datetime,6,7)
    data$month=as.integer(data$month)
    unique(data$month)
    
    train=data[as.integer(substr(data$datetime,9,10))<20,]
    test=data[as.integer(substr(data$datetime,9,10))>19,]
    
    
    boxplot(train$registered~train$month,xlab="humidity", ylab="registered users")
    boxplot(train$casual~train$month,xlab="humidity", ylab="casual users")
    
    boxplot(train$count~train$month,xlab="year", ylab="count")
    
    
    # 4.8. Pollution & Traffic: We don't have the variable related with these metrics in our data set so we cannot test this hypothesis.
    
    # created this variable to divide a day into parts, but did not finally use it
    data$day_part=0
    
    train=data[as.integer(substr(data$datetime,9,10))<20,]
    test=data[as.integer(substr(data$datetime,9,10))>19,]
    
    data=rbind(train,test)

    

# Step 5. Feature Engineering    
    
    # you must have noticed that we generated new variables like hour, month, day and year.
    # 
    # Here we will create more variables, let's look at the some of these:  
    
    # Hour Bins: Initially, we have broadly categorize the hour into three categories. Let's create bins for the hour variable separately for casual and registered users. Here we will use decision tree to find the accurate bins.   
    
    # We use the library rpart for decision tree algorithm.

        #using decision trees for binning some variables, this was a really important step in feature engineering
    d=rpart(registered~hour,data=train)
    fancyRpartPlot(d)
    
    d=rpart(casual~hour,data=train)
    fancyRpartPlot(d)
    
    data=rbind(train,test)
    str(data)
    
    # Now, looking at the nodes we can create different hour bucket for registered users.
  
    data$dp_reg=0
    data$dp_reg[data$hour<8]=1
    data$dp_reg[data$hour>=22]=2
    data$dp_reg[data$hour>9 & data$hour<18]=3
    data$dp_reg[data$hour==8]=4
    data$dp_reg[data$hour==9]=5
    data$dp_reg[data$hour==20 | data$hour==21]=6
    data$dp_reg[data$hour==19 | data$hour==18]=7
    
    data$dp_cas=0
    data$dp_cas[data$hour<=8]=1
    data$dp_cas[data$hour==9]=2
    data$dp_cas[data$hour>=10 & data$hour<=19]=3
    data$dp_cas[data$hour>19]=4
    
    f=rpart(registered~temp,data=train)
    fancyRpartPlot(f)
    
    f=rpart(casual~temp,data=train)
    fancyRpartPlot(f)
    
    # Temp Bins:  Using similar methods, we have created bins for temperature for both registered and casuals users. Variables created are (temp_reg and temp_cas).
    data$temp_reg=0
    data$temp_reg[data$temp<13]=1
    data$temp_reg[data$temp>=13 & data$temp<23]=2
    data$temp_reg[data$temp>=23 & data$temp<30]=3
    data$temp_reg[data$temp>=30]=4
    
    data$temp_cas=0
    data$temp_cas[data$temp<15]=1
    data$temp_cas[data$temp>=15 & data$temp<23]=2
    data$temp_cas[data$temp>=23 & data$temp<30]=3
    data$temp_cas[data$temp>=30]=4
    
    # Year Bins: We had a hypothesis that bike demand will increase over time and we have proved it also. Here I have created 8 bins (quarterly) for two years. Jan-Mar 2011 as 1 ...Oct-Dec2012 as 8.   
    
    unique(data$year_part)
    
    data$year_part[data$year=='2011']=1
    data$year_part[data$year=='2011' & data$month>3]=2
    data$year_part[data$year=='2011' & data$month>6]=3
    data$year_part[data$year=='2011' & data$month>9]=4
    data$year_part[data$year=='2012']=5
    data$year_part[data$year=='2012' & data$month>3]=6
    data$year_part[data$year=='2012' & data$month>6]=7
    data$year_part[data$year=='2012' & data$month>9]=8
    table(data$year_part)
    
    # creating another variable day_type which may affect our accuracy as weekends and weekdays are important in deciding rentals

    # Day Type: Created a variable having categories like "weekday", "weekend" and "holiday".
    data$day_type=""
    data$day_type[data$holiday==0 & data$workingday==0]="weekend"
    data$day_type[data$holiday==1]="holiday"
    data$day_type[data$holiday==0 & data$workingday==1]="working day"
    
    # Weekend: Created a separate variable for weekend (0/1)
    data$weekend=0
    data$weekend[data$day=="Sunday" | data$day=="Saturday" ]=1
    
    train=data[as.integer(substr(data$datetime,9,10))<20,]
    test=data[as.integer(substr(data$datetime,9,10))>19,]
    
    
    plot(train$temp,train$count)
    data=rbind(train,test)
    

    
    str(data)
    unique(data$windspeed)
    sum(is.na(data$windspeed)) #Check ifany missing values
    table(data$windspeed==0)
    
    # dividing total data depending on windspeed to impute/predict the missing values
    
    k=data$windspeed==0
    wind_0=subset(data,k)
    wind_1=subset(data,!k)
    
    str(data)
    
    # predicting missing values in windspeed using a random forest model
    # this is a different approach to impute missing values rather than just using the mean or median or some other statistic for imputation
    
    set.seed(415)
    
    # fit <- randomForest(windspeed ~ season+weather +humidity +month+temp+ year+atemp, data=wind_1,importance=TRUE, ntree=250)
    
    # rm(fit)
    # saveRDS(fit, "RandomForest_Wind.rds")##Save Model
    mod <- readRDS("C:/R/Analytics Vidhya/Bike Sharing Competition/RandomForest_Wind.rds") ##Load Model
   
    pred=predict(mod,wind_0)
    wind_0$windspeed=pred
    
    data=rbind(wind_0,wind_1)
    
    data$weekend=0
    data$weekend[data$day=="Sunday" | data$day=="Saturday"]=1
    
    str(data)
    
    # converting all relevant categorical variables into factors to feed to our random forest model
    data$season=as.factor(data$season)
    data$holiday=as.factor(data$holiday)
    data$workingday=as.factor(data$workingday)
    data$weather=as.factor(data$weather)
    data$hour=as.factor(data$hour)
    data$month=as.factor(data$month)
    data$day_part=as.factor(data$dp_cas)
    data$day_type=as.factor(data$dp_reg)
    data$day=as.factor(data$day)
    data$temp_cas=as.factor(data$temp_cas)
    data$temp_reg=as.factor(data$temp_reg)
   
    
    # log transformation for some skewed variables, which can be seen from their distribution
    # As we know that dependent variables have natural outliers so we will predict log of dependent variables.
    # y1=log(casual+1) and y2=log(registered+1), Here we have added 1 to deal with zero values in the casual and registered columns.
    train$reg1=train$registered+1
    train$cas1=train$casual+1
    train$logcas=log(train$cas1)
    train$logreg=log(train$reg1)
    test$logreg=0
    test$logcas=0
    
    boxplot(train$logreg~train$weather,xlab="weather", ylab="registered users")
    boxplot(train$logreg~train$season,xlab="season", ylab="registered users")
    
    
    
    
    
# Step 6. Model Building
    # As this was our first attempt, we applied decision tree, conditional inference tree and random forest algorithms and found that random forest is performing the best. You can also go with regression, boosted regression, neural network and find which one is working well for you.
    # final model building using random forest
    # note that we build different models for predicting for registered and casual users
    # this was seen as giving best result after a lot of experimentation
    set.seed(415)
    # fit1 <- randomForest(logreg ~ hour +workingday+day+holiday+ day_type +temp_reg+humidity+atemp+windspeed+season+weather+dp_reg+weekend+year+year_part, data=train,importance=TRUE, ntree=250)
    
    saveRDS(fit1, "RandomForest_1.rds")##Save Model
    mod1 <- readRDS("C:/R/Analytics Vidhya/Bike Sharing Competition/RandomForest_1.rds") ##Load Model
    
    pred1=predict(mod1,test)
    test$logreg=pred1
    
    
    set.seed(415)
    # fit2 <- randomForest(logcas ~hour + day_type+day+humidity+atemp+temp_cas+windspeed+season+weather+holiday+workingday+dp_cas+weekend+year+year_part, data=train,importance=TRUE, ntree=250)
    
    saveRDS(fit2, "RandomForest_2.rds")##Save Model
    mod2 <- readRDS("C:/R/Analytics Vidhya/Bike Sharing Competition/RandomForest_2.rds") ##Load Model
    
    
    
    pred2=predict(mod2,test)
    test$logcas=pred2
    
    #creating the final submission file
    test$registered=exp(test$logreg)-1
    test$casual=exp(test$logcas)-1
    test$count=test$casual+test$registered
    s<-data.frame(datetime=test$datetime,count=test$count)
    write.csv(s,file="submit.csv",row.names=FALSE)
    
    
    
    
    
    
    