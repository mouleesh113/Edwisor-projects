##################################################
######## BIKE RENTAL COUNT PREDICTION ############
##################################################

#lets clean the R environment
rm(list = ls())

#setting working directory
setwd("D:/Data Science/Assignments/Project")
getwd()

# Load libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced",
       "Information", "MASS", "rpart", "ROSE",
      'sampling', 'DataCombine', 'inTrees',"scales","psych","gplots")
#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

#lets load the data
Bike_Data = read.csv("day.csv")

#explore the data
dim(Bike_Data)
names(Bike_Data)

#rename the shortcut values for our understanding
names(Bike_Data)[1] = 'index'
names(Bike_Data)[2] = 'date'
names(Bike_Data)[4] = 'year'
names(Bike_Data)[5] = 'month'
names(Bike_Data)[9] = 'weather'
names(Bike_Data)[10] = 'temperature'
names(Bike_Data)[12] = 'humidity'
names(Bike_Data)[16] = 'count'

#lets check column names after renamed
names(Bike_Data)

#lets see top 5 observations in the dataset
head(Bike_Data)

#lets check last 5 observations in our dataset
tail(Bike_Data)

#lets check structure of each variable
str(Bike_Data)

#lets see summary of the dataset
summary(Bike_Data)

#in our dataset we have 16 variables out of which all are independent variable except last variable
str(Bike_Data['count'])

#in our dataset some vaiables has no usefull information for our prediction
#so it is better to remove those variables.so it helps us to make useful inferences

#lets drop unnecessary variables
Bike_Data = subset(Bike_Data,select = -c(index,date,casual,registered))

#lets divide categorical variables and numerical variables
#numerical variables
cnames = c("temperature",'atemp','windspeed','humidity','count')
#categorical variables
catnames = c('season','year','month','holiday','weekday','workingday','weather')

#Data preprocessing
missing_val = sum(is.na(Bike_Data))
missing_val
#there is no missing values in our dataset

#outlier analysis
for (i in 1:length(cnames))
   {
     assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "count"), data = subset(Bike_Data))+ 
              stat_boxplot(geom = "errorbar", width = 0.5) +
              geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                           outlier.size=1, notch=FALSE) +
              theme(legend.position="bottom")+
              labs(y=cnames[i],x="count")+
              ggtitle(paste("Box plot of count for",cnames[i])))
}

#plotting boxplot
library(gridExtra)
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,ncol=1)

#lets remove outliers using boxplot
df = Bike_Data
for(i in cnames){
    print(i)
     outliers = Bike_Data[,i][Bike_Data[,i] %in% boxplot.stats(Bike_Data[,i])$out]
     print(length(outliers))
     Bike_Data = Bike_Data[which(!Bike_Data[,i] %in% outliers),]
   }
#lets plot boxplot after removing outliers
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "count"), data = subset(Bike_Data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="count")+
           ggtitle(paste("Box plot of count for",cnames[i])))
}
#plotting Boxplot after removing outliers
gridExtra::grid.arrange(gn1,gn2,ncol=2)
gridExtra::grid.arrange(gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,ncol=1)

#data visualization
#univariate analysis
#lets see distribution of the variables.
for(i in 1:length(cnames))
{
   assign(paste0("h",i),ggplot(aes_string(x=(cnames[i])),
                               data=subset(Bike_Data))+
             geom_histogram(fill="green",colour = "red")+geom_density()+
             scale_y_continuous(breaks =pretty_breaks(n=10))+
             scale_x_continuous(breaks = pretty_breaks(n=10))+
             theme_bw()+xlab(cnames[i])+ylab("Frequency")+
             ggtitle(paste("distribution plot for ",cnames[i])))
}

#lets see distribution plot
gridExtra::grid.arrange(h1,h2,ncol=1)
gridExtra::grid.arrange(h3,h4,ncol=1)
gridExtra::grid.arrange(h5,ncol=1)

#bivariate analysis
#lets check distribution between target and continuous variables
for(i in 1:length(cnames))
{
   assign(paste0("s",i),ggplot(aes_string(y='count',x = (cnames[i])),
                               data=subset(Bike_Data))+
             geom_point(alpha=0.5,color="green") +
             ggtitle(paste("Scatter Plot between count vs ",cnames[i])))
}
#lets plot between continuous and target variables.
gridExtra::grid.arrange(s1,s2,ncol=1)
gridExtra::grid.arrange(s3,s4,ncol=1)
gridExtra::grid.arrange(s5,ncol=1)

#from the above graphs,we can see that as temperature increases and rental count also increases
#apart from temperature,windspeed and humidity doesnot impact on rental count

#lets check categorical variables
for(i in 1:length(catnames))
{
   assign(paste0("b",i),ggplot(aes_string(y='count',x = (catnames[i])),
                               data=subset(Bike_Data))+
             geom_bar(stat = "identity",fill = "green") +
             ggtitle(paste("Number of bikes rented with respect to",catnames[i])))+
      theme(axis.text.x = element_text( color="red", size=8))+
      theme(plot.title = element_text(face = "old"))
}

#lets plot between categorical and target variables
gridExtra::grid.arrange(b1,b2,ncol=1)
gridExtra::grid.arrange(b3,b4,ncol=1)
gridExtra::grid.arrange(b5,b6,ncol=1)
gridExtra::grid.arrange(b7,ncol=1)

# Based on the plot,we can the observe the below inferences 
aggregate(count ~ season ,sum,data = Bike_Data)
#Bike rental count is high in season 3 which is fall  and low in season 1

aggregate(count ~ year ,sum,data = Bike_Data)
#Bike rental count is high in year 1 which is 2012

aggregate(count ~ month,sum,data = Bike_Data)
#Bike rental count is high in the month of august and low in january

aggregate(count ~ holiday ,sum,data = Bike_Data)
#Bike rental count is high on holidays which is 0 and low in working day

aggregate(count ~ weekday ,sum,data = Bike_Data)
#bike rental count is high in 5 which is friday and low in 0 which is sunday

aggregate(count ~ workingday,sum,data = Bike_Data)
#Bike rental count is high in 1 which is working day

#Bike rental count is high in weather 1 which is Clear, Few clouds, Partly cloudy, Partly cloudy
#and there is no bike rented on 4
aggregate(count ~ weather,sum,data = Bike_Data)

# Bikes rented with respect to temperature and humidity
ggplot(Bike_Data,aes(temperature,count)) + 
   geom_point(aes(color=humidity),alpha=0.8) +
   labs(title = "Variations of bikes rented with respect to temperature and humidity",
        x = "temperature")+ theme_bw()
#based on the below plot we know that bike rental is higher when the 
#temperature is between 0.5 to 0.75 
#humidity less than 0.6

#Bikes rented with respect to temperature and windspeed
ggplot(Bike_Data, aes(x = temperature, y = count))+
   geom_point(aes(color=windspeed))+
   labs(title = "Variations of bikes rented with respect to temperature and windspeed", 
        x = "temperature")+
   theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
   theme_bw()
#based on the below plot we know that bike rental is higher when the 
#temperature is between 0.5 to 0.75
#windspeed is less than 0.2

# Bikes rented with respect to temperature and season
ggplot(Bike_Data, aes(x = temperature, y = count))+
   geom_point(aes(color=season))+
   labs(title = "Variations of bikes rented with respect to temperature and season",
        x = "temperature")+
   theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
   theme_bw()
#based on the below plot we know that bike rental is higher when the 
#temperature is between 0.5 to 0.75 
#season was 2 and 3

#FEATURE SELECTION

#lets find correlation matrix using corrplot and correlation plot using corrgram library
#FOR NUMERICAL VARIABLES
#lets save dataset after outlier analysis
df = Bike_Data
#correlation matrix
correlation_matrix = cor(Bike_Data[,cnames])
correlation_matrix
#correlation plot
corrgram(Bike_Data[,cnames],order = F,upper.panel = panel.pie,
         text.panel = panel.txt,main = 'Correlation plot')
#From the correlation plot,we see that temperature and atemp variables are correlated to each other
#so we need to remove atemp variable.

#lets see annova test for categorical variables
for (i in catnames) {
   print(i)
   anova = summary(aov(formula = count~Bike_Data[,i],Bike_Data))
   print(anova)
}
#based on the anova result, we are going to drop three variables namely,
#HOLIDAY
#WEEKDAY
#WORKINGDAY
#because these variables having the p-value > 0.05

#Dimension reduction
Bike_Data = subset(Bike_Data,select = -c(holiday,weekday,workingday,atemp))

#lets check after dimension reduction
dim(Bike_Data)
head(Bike_Data)

#lets update continous and categorical variables after dimension reduction
cnames = c('temperature','humidity','windspeed','count')
catnames = c('season','year','month','weather')

#FEATURE SCALING
#lets check normality between the varaibles
for (i in cnames){
   print(i)
   normality = qqnorm(Bike_Data[,i])

}

#already we plotted distrution between these variables,lets recall it
for(i in 1:length(cnames))
{
   assign(paste0("h",i),ggplot(aes_string(x=(cnames[i])),
                               data=subset(Bike_Data))+
             geom_histogram(fill="green",colour = "red")+geom_density()+
             scale_y_continuous(breaks =pretty_breaks(n=10))+
             scale_x_continuous(breaks = pretty_breaks(n=10))+
             theme_bw()+xlab(cnames[i])+ylab("Frequency")+
             ggtitle(paste("distribution plot for ",cnames[i])))
}
gridExtra::grid.arrange(h1,h2,h3,h4,ncol = 2)

#summary of the data
for (i in cnames) {
   print(i)
   print(summary(Bike_Data[,i]))
   
}
#Based on the above inferences and plots,we can see that the variables are normalised 
#as mentioned in problem statement

#MODEL DEVELOPMENT

#lets clean our environment except preprocessed dataset
rmExcept('Bike_Data')

#we can pass categorical variables to regression problems
#lets convert categorical variables into dummy variables
#save our preprocessed data
df = Bike_Data

#create dummies
library(dummies)
catnames = c('season','year','month','weather')
Bike_Data = dummy.data.frame(Bike_Data,catnames)

#we succesfully created dummies,lets check dimension and top 5 observations
dim(Bike_Data)
head(Bike_Data)

#divide the data into train and test
set.seed(1234)
train_index = sample(1:nrow(df), 0.8 * nrow(df))
train_data = Bike_Data[train_index,]
test_data = Bike_Data[-train_index,]

#linear regression
#check multicollearity
library(usdm)
cnames = c('temperature','humidity','windspeed')
vif(Bike_Data[,cnames])

vifcor(Bike_Data[,cnames], th = 0.9)
#No variable from the 3 input variables has collinearity problem. 

#The linear correlation coefficients ranges between: 
 #  min correlation ( humidity ~ temperature ):  0.1267216 
#max correlation ( windspeed ~ humidity ):  -0.2411599 

#---------- VIFs of the remained variables -------- 
 #  Variables      VIF
#1 temperature 1.034137
#2    humidity 1.070959
#3   windspeed 1.080362
#lets run regression model
lm_model = lm(count~. ,data = Bike_Data)
#lets check performance of our modedl
summary(lm_model)
#Residual standard error: 787.1 on 710 degrees of freedom
#Multiple R-squared:  0.8394,	Adjusted R-squared:  0.8349 
#F-statistic: 185.6 on 20 and 710 DF,  p-value: < 2.2e-16

# Function for Error metrics to calculate the performance of model
#lets build function for MAPE
#calculate MAPE
MAPE = function(y, y1){
   mean(abs((y - y1)/y))
}

# Function for r2 to calculate the goodness of fit of model
rsquare=function(y,y1){
   cor(y,y1)^2
}

# Function for RMSE value 
RMSE = function(y,y1){
   difference = y - y1
   root_mean_square = sqrt(mean(difference^2))
}

#lets predict for train and test data
Predictions_LR_train = predict(lm_model,train_data[,-25])
Predictions_LR_test = predict(lm_model,test_data[,-25])

#let us check performance of our model

#mape calculation
LR_train_mape = MAPE(Predictions_LR_train,train_data[,25])
LR_test_mape = MAPE(test_data[,25],Predictions_LR_test)

#Rsquare calculation
LR_train_r2 = rsquare(train_data[,25],Predictions_LR_train)
LR_test_r2 = rsquare(test_data[,25],Predictions_LR_test)

#rmse calculation
LR_train_rmse = RMSE(train_data[,25],Predictions_LR_train)
LR_test_rmse = RMSE(test_data[,25],Predictions_LR_test)

print(LR_train_mape)#0.16
print(LR_test_mape)#0.17
print(LR_train_r2)#0.825
print(LR_test_r2)#0.893
print(LR_train_rmse)#804.9
print(LR_test_rmse)#648.9


#Decision tree regression
library(rpart)
DT_model = rpart(count ~ ., data = train_data, method = "anova")
DT_model


# Lets predict for train and test data
predictions_DT_train= predict(DT_model,train_data[,-25])
predictions_DT_test= predict(DT_model,test_data[,-25])

# MAPE calculation
DT_train_mape = MAPE(train_data[,25],predictions_DT_train)
DT_test_mape = MAPE(test_data[,25],predictions_DT_test)

# Rsquare calculation
DT_train_r2= rsquare(train_data[,25],predictions_DT_train)
DT_test_r2 = rsquare(test_data[,25],predictions_DT_test)

# RMSE calculation
DT_train_rmse = RMSE(train_data[,25],predictions_DT_train)
DT_test_rmse = RMSE(test_data[,25],predictions_DT_test)

print(DT_train_mape)#0.536
print(DT_test_mape)#0.269
print(DT_train_r2)#0.806
print(DT_test_r2)#0.834
print(DT_train_rmse)#846.85
print(DT_test_rmse)#805.67


#Random search CV in decision tree
df = Bike_Data
#setting parameters for training using caret library
control = trainControl(method="repeatedcv", number=5, repeats=1,search='random')
maxdepth = c(1:30)
tunegrid = expand.grid(.maxdepth=maxdepth)

# Lets build a model using above parameters on train data 
RDT_model = caret::train(count~., data=train_data, method="rpart2",trControl=control,tuneGrid= tunegrid)
print(RDT_model)

#lets look best parameter
best_parameter = RDT_model$bestTune
print(best_parameter)
#maximum depth = 10

#again build a decsion tree using best parameters
RDT_bestmodel = rpart(count~.,train_data,method = 'anova',maxdepth=10)
print(RDT_bestmodel)

#lets predict for train and test data
predictions_RDT_train = predict(RDT_bestmodel,train_data[1:24])
predictions_RDT_test = predict(RDT_bestmodel,test_data[1:24])

#model performance
# MAPE calculation
RDT_train_mape = MAPE(train_data[,25],predictions_RDT_train)
RDT_test_mape = MAPE(test_data[,25],predictions_RDT_test)

# Rsquare calculation
RDT_train_r2= rsquare(train_data[,25],predictions_RDT_train)
RDT_test_r2 = rsquare(test_data[,25],predictions_RDT_test)

# RMSE calculation
RDT_train_rmse = RMSE(train_data[,25],predictions_RDT_train)
RDT_test_rmse = RMSE(test_data[,25],predictions_RDT_test)

print(RDT_train_mape)#0.522
print(RDT_test_mape)#0.243
print(RDT_train_r2)#0.811
print(RDT_test_r2)#0.798
print(RDT_train_rmse)#833.48
print(RDT_test_rmse)#885.59

#Grid search CV decision tree
#setting parameters for training using caret library
control = trainControl(method="repeatedcv", number=5, repeats=2,search='grid')
maxdepth = c(6:30)
tunegrid = expand.grid(maxdepth=maxdepth)

# Lets build a model using above parameters on train data 
GDT_model = caret::train(count~., data=train_data, method="rpart2",trControl=control,tuneGrid= tunegrid)
print(GDT_model)

#lets look best parameter
best_parameter = GDT_model$bestTune
print(best_parameter)
#maximum depth = 10

#again build a decsion tree using best parameters
GDT_bestmodel = rpart(count~.,train_data,method = 'anova',maxdepth=10)
print(GDT_bestmodel)

#lets predict for train and test data
predictions_GDT_train = predict(GDT_bestmodel,train_data[1:24])
predictions_GDT_test = predict(GDT_bestmodel,test_data[1:24])

#model performance
# MAPE calculation
GDT_train_mape = MAPE(train_data[,25],predictions_GDT_train)
GDT_test_mape = MAPE(test_data[,25],predictions_GDT_test)

# Rsquare calculation
GDT_train_r2= rsquare(train_data[,25],predictions_GDT_train)
GDT_test_r2 = rsquare(test_data[,25],predictions_GDT_test)

# RMSE calculation
GDT_train_rmse = RMSE(train_data[,25],predictions_GDT_train)
GDT_test_rmse = RMSE(test_data[,25],predictions_GDT_test)

print(GDT_train_mape)#0.522
print(GDT_test_mape)#0.243
print(GDT_train_r2)#0.811
print(GDT_test_r2)#0.798
print(GDT_train_rmse)#833.48
print(GDT_test_rmse)#885.59

#RANDOM FOREST
#lets build the random forest model
RF_model = randomForest(count~.,data = train_data,n.trees = 500)
print(RF_model)

#lets predict for both train and test data
predictions_RF_train = predict(RF_model,train_data[-25])
predictions_RF_test = predict(RF_model,test_data[-25])

#MAPE calculation
RF_train_mape = MAPE(predictions_RF_train,train_data[,25])
RF_test_mape = MAPE(predictions_RF_test,test_data[,25])

#Rsquare calculation
RF_train_r2 = rsquare(predictions_RF_train,train_data[,25])
RF_test_r2 = rsquare(predictions_RF_test,test_data[,25])

#RMSE calculation
RF_train_rmse = RMSE(train_data[,25],predictions_RF_train)
RF_test_rmse = RMSE(test_data[,25],predictions_RF_test)

print(RF_train_mape)#0.07
print(RF_test_mape)#0.11
print(RF_train_r2)#0.965
print(RF_test_r2)#0.912
print(RF_train_rmse)#371.06
print(RF_test_rmse)#586.72

#Random search CV random forest
#setting parameters for training using caret library
control = trainControl(method="repeatedcv", number=5, repeats=3,search='random')
maxdepth = c(1:30)
tunegrid = expand.grid(maxdepth=maxdepth)

#lets build Random forest model using the above parameters
RRF_model = caret::train(count~.,data=train_data,method ='rf',trcontrol=control,tunegrid=tunegrid)
print(RRF_model)
best_parameter = RRF_model$bestTune
print(best_parameter)
#mtry = 13
#lets again build the random forest by above paremeters
RRF_bestmodel = randomForest(count~.,data = train_data,method = 'rf',mtry = 13,importance = TRUE)
print(RRF_bestmodel)

#lets predict for both train and test data
prediction_RRF_train = predict(RRF_bestmodel,train_data[-25])
prediction_RRF_test = predict(RRF_bestmodel,test_data[-25])

#MAPE calculation
RRF_train_mape = MAPE(train_data[,25],prediction_RRF_train)
RRF_test_mape = MAPE(test_data[,25],prediction_RRF_test)

#Rsquare calculation
RRF_train_r2 = rsquare(train_data[,25],prediction_RRF_train)
RRF_test_r2 = rsquare(test_data[,25],prediction_RRF_test)

#RMSE calculation
RRF_train_rmse = RMSE(train_data[,25],prediction_RRF_train)
RRF_test_rmse = RMSE(test_data[,25],prediction_RRF_test)

print(RRF_train_mape)#0.241
print(RRF_test_mape)#0.159
print(RRF_train_r2)#0.971
print(RRF_test_r2)#0.907
print(RRF_train_rmse)#333.513
print(RRF_test_rmse)#602.26

#GRID SEARCH CV RANDOM FOREST
#lets set require parameters using caret library
control = trainControl(method="repeatedcv", number=5, repeats=4,search='grid')
maxdepth = c(6:30)
tunegrid = expand.grid(maxdepth=maxdepth)

#lets build Random forest model using the above parameters
GRF_model = caret::train(count~.,data=train_data,method ='rf',trcontrol=control,tunegrid=tunegrid)
print(GRF_model)
best_parameter = GRF_model$bestTune
print(best_parameter)
#mtry = 13
#lets again build the same model using bestparameter
GRF_bestmodel = randomForest(count~.,data = train_data,mtry =13,importance = TRUE,method='rf')
print(GRF_bestmodel)

#lets predict on train and test data,
predictions_GRF_train = predict(GRF_bestmodel,train_data[-25])
predictions_GRF_test = predict(GRF_bestmodel,test_data[-25])

#MAPE calculation
GRF_train_mape = MAPE(predictions_GRF_train,train_data[,25])
GRF_test_mape = MAPE(predictions_GRF_test,test_data[,25])

#Rsquare calculation
GRF_train_r2 = rsquare(predictions_GRF_train,train_data[,25])
GRF_test_r2 = rsquare(predictions_GRF_test,test_data[,25])

#RMSE calculation
GRF_train_rmse = RMSE(predictions_GRF_train,train_data[,25])
GRF_test_rmse = RMSE(predictions_GRF_test,test_data[,25])

print(GRF_train_mape)#0.06
print(GRF_test_mape)#0.12
print(GRF_train_r2)#0.972
print(GRF_test_r2)#0.90
print(GRF_train_rmse)#335.18
print(GRF_test_rmse)#597.59

#MODEL SELECTION
Model_name = c('Linear regression',
               'Decision tree','Random search CV decision tree','Grid search CV decision tree',
               'Random forest','Random search CV random forest','Grid search CV random forest')

MAPE_train = c(LR_train_mape,DT_train_mape,RDT_train_mape,GDT_train_mape,
               RF_train_mape,GRF_train_mape,GRF_train_mape)

MAPE_test = c(LR_test_mape,DT_test_mape,RDT_test_mape,GDT_test_mape,
              RF_test_mape,GRF_test_mape,GRF_test_mape)

Rsquare_train = c(LR_train_r2,DT_train_r2,RDT_train_r2,GDT_train_r2,
                  RF_train_r2,GRF_train_r2,GRF_train_r2)

Rsquare_test = c(LR_test_r2,DT_test_r2,RDT_test_r2,GDT_test_r2,
                 RF_test_r2,GRF_test_r2,GRF_test_r2)

RMSE_train =  c(LR_train_rmse,DT_train_rmse,RDT_train_rmse,GDT_train_rmse,
                RF_train_rmse,GRF_train_rmse,GRF_train_rmse)

RMSE_test = c(LR_test_rmse,DT_test_rmse,RDT_test_rmse,GDT_test_rmse,
              RF_test_rmse,RRF_test_rmse,GRF_test_rmse)

FINAL_RESULTS = data.frame(Model_name,MAPE_train,MAPE_test,Rsquare_train,Rsquare_test,
                           RMSE_train,RMSE_test)

print(FINAL_RESULTS)


#Index       Model_name           MAPE_train MAPE_test Rsquare_train  Rsquare_test RMSE_train RMSE_test
#1              Linear regression 0.15497164 0.1829289     0.8311816    0.8671739   789.6785  717.2833
#2                  Decision tree 0.52210598 0.2438791     0.8119266    0.7986807   833.4855  885.5906
#3 Random search CV decision tree 0.52210598 0.2438791     0.8119266    0.7986807   833.4855  885.5906
#4   Grid search CV decision tree 0.52210598 0.2438791     0.8119266    0.7986807   833.4855  885.5906
#5                  Random forest 0.07220796 0.1200109     0.9652279    0.9132608   371.1086  585.0001
#6 Random search CV random forest 0.06441047 0.1222848     0.9718028    0.9077133   332.6579  593.6970
#7   Grid search CV random forest 0.06441047 0.1222848     0.9718028    0.9077133   332.6579  601.4685

#  Based on the above inferences,we came to know that Random forest performs very well in our dataset
#lets finalise that model.