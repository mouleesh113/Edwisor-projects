#************************************************************************#
# Cab fare  Prediction   
#************************************************************************#

# Clean the environment
rm(list=ls())

# Set working directory
setwd("D:/Data Science/Cab Fare")


# Load required Libraries for analysis  ----------------------------------
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced","C50",
      "dummies", "e1071", "Information", "MASS", "rpart", "gbm", "ROSE",
      'sampling', 'DataCombine', 'inTrees',"scales","gplots")

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)

# Load the data -----------------------------------------------------------
Train_Cab = read.csv("train_cab.csv")


# Explore the data  -------------------------------------------------------
# Check class of the data
class(Train_Cab)

#Check the dimensions(no of rows and no of columns)
dim(Train_Cab)

#Check names of dataset(no need of renaming variables)
names(Train_Cab)

#Check top(first) rows of dataset 
head(Train_Cab)

#Check bottom(last) rows of dataset 
tail(Train_Cab)

#Check structure of dataset(data structure of each variable)
str(Train_Cab)

#Check summary of dataset 
summary(Train_Cab)

# Variable Identification 
# Target variable - fare_amount 
str(Train_Cab$fare_amount) # fare_amount is a continous variabe 

# Data type conversion
# As observed, we have to change fare_amount from factor to numeric
Train_Cab$fare_amount = as.numeric(as.character(Train_Cab$fare_amount))

summary((Train_Cab$fare_amount))

# we need to convert pickupdatetime as well we did it in feature engineering 

# Missing Value Analysis --------------------------------------------------

# Total number of missing values present in whole datset 
Missing_val = sum(is.na(Train_Cab)) 
Missing_val
#lets drop those missing values
Train_Cab = na.omit(Train_Cab)
#lets verify after dropping missing values.
sum(is.na(Train_Cab)) 

# We need to change pickup_datetime from factor to datetime
# But first, let's replace UTC in pickup_datetime variable with ''(space)

Train_Cab$pickup_datetime = gsub('// UTC','',Train_Cab$pickup_datetime)

# Now convert variable pickup_dattime to date time format by creating
# new variable with name Date 

Train_Cab$date = as.Date(Train_Cab$pickup_datetime)

# Lets split this new variable Date into year,month,weekday 
# Extract the year
Train_Cab$year = substr(as.character(Train_Cab$date),1,4)

# Extract the month
Train_Cab$month =substr(as.character(Train_Cab$date),6,7)

# Extract the weekday 
Train_Cab$day = weekdays(as.POSIXct(Train_Cab$date),abbreviate = F)

# Extract the date 
Train_Cab$date = substr(as.character(Train_Cab$date),9,10)

# Extract the time / hour
Train_Cab$hour = substr(as.factor(Train_Cab$pickup_datetime),12,13)

#Lets delete picupdate time as we converted this variable into day,month,year,hour
Train_Cab$pickup_datetime = NULL

dim(Train_Cab)

head(Train_Cab)

# Lets check summary again after new feature creation
summary(Train_Cab)

# Lets check for NA after conversion.if present removing those variables.
sum(is.na(Train_Cab))
Train_Cab = na.omit(Train_Cab)

#lets check the dimension after removing missing values
dim(Train_Cab)

# Outlier analysis --------------------------------------------------------
# Boxplots-Distribution and outlier check
numeric_index = sapply(Train_Cab,is.numeric)# Selecting only numeric 
numeric_index
numeric_data =Train_Cab[,numeric_index]
cnames = colnames(numeric_data)
cnames 

# Boxplot for all continous varaibles 
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = Train_Cab)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=2)

# We have different methods to remove outliers.
#here i am going to remove all outliers one by one and 
#capping with minimum and maximum values for some variables



######  fare_amount ######
# Let use summary function to check min max values and identify outliers 
summary(Train_Cab$fare_amount)
#by seeing the summary we say that amount cant be in negative value as well as fare amount cannot exceed 500

Train_Cab$fare_amount[Train_Cab$fare_amount < 1] = NA
Train_Cab$fare_amount[Train_Cab$fare_amount >500] = NA
sum(is.na(Train_Cab))
Train_Cab = na.omit(Train_Cab)
#lets very after performing outlier analysis
summary(Train_Cab$fare_amount)
------------------------------------------------------------------------------------
# pickup_longitude
------------------------------------------------------------------------------------
#Originally, Latitudes range from -90 to 90.
#Originally, Longitudes range from -180 to 180.

  
  summary(Train_Cab$pickup_longitude)

Q1 <- quantile(Train_Cab$pickup_longitude,0.25)#-73.99126
Q3 <- quantile(Train_Cab$pickup_longitude,0.75)#-73.96684
UL <- Q3 + (1.5*IQR(Train_Cab$pickup_longitude))# -73.92884
LL <- Q1-(1.5*IQR(Train_Cab$pickup_longitude)) # -74.03013

Train_Cab[Train_Cab$pickup_longitude < LL ,"pickup_longitude"] <- LL 
Train_Cab[Train_Cab$pickup_longitude > UL ,"pickup_longitude"] <- UL 
---------------------------------------------------------------------------------------
# pickup_latitude
---------------------------------------------------------------------------------------
summary(Train_Cab$pickup_latitude)  
  
Q1 = quantile(Train_Cab$pickup_latitude,0.25)
Q3 = quantile(Train_Cab$pickup_latitude,0.75)
UL = Q3 + (1.5*IQR(Train_Cab$pickup_latitude))
LL = Q1 - (1.5*IQR(Train_Cab$pickup_latitude))

Train_Cab[Train_Cab$pickup_latitude < LL,"pickup_latitude"] = LL
Train_Cab[Train_Cab$pickup_latitude > UL,"pickup_latitude"] = UL


#### Dropoff longitude #####
summary(Train_Cab$dropoff_longitude)

Q1 = quantile(Train_Cab$dropoff_longitude,0.25) 
Q3 = quantile(Train_Cab$dropoff_longitude,0.75)
UL = Q3 + (1.5*IQR(Train_Cab$dropoff_longitude))
LL = Q1 - (1.5*IQR(Train_Cab$dropoff_longitude))

Train_Cab[Train_Cab$dropoff_longitude < LL,"dropoff_longitude"] = LL
Train_Cab[Train_Cab$dropoff_longitude > UL,"dropoff_longitude"] = UL

-----------------------------------------------------------------------------------------
# dropoff_lattitude
-----------------------------------------------------------------------------------------
summary(Train_Cab$dropoff_latitude)

Q1 = quantile(Train_Cab$dropoff_latitude,0.25)
Q3 = quantile(Train_Cab$dropoff_latitude,0.75)
UL = Q3 + (1.5*IQR(Train_Cab$dropoff_latitude))
LL = Q1 - (1.5*IQR(Train_Cab$dropoff_latitude))

Train_Cab[Train_Cab$dropoff_latitude < LL,"dropoff_latitude"] = LL
Train_Cab[Train_Cab$dropoff_latitude > UL,"dropoff_latitude"] = UL

# passenger_count
summary(Train_Cab$passenger_count)



# practically maximum 6 passenger can travel in a cab 
Train_Cab[Train_Cab$passenger_count < 1,"passenger_count"] = NA
Train_Cab[Train_Cab$passenger_count > 6,"passenger_count"] = NA

sum(is.na(Train_Cab))
Train_Cab = na.omit(Train_Cab)
#lets very after performing outlier analysis
summary(Train_Cab$passenger_count)


# Lets visualize boxplots again after outlier removal 

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = Train_Cab)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}

gridExtra::grid.arrange(gn1,gn2,gn3,gn4,gn5,gn6,ncol=2)

# Lets check dimensions of data after outlier removal 
dim(Train_Cab)

# Now, let's create distance using Haversine Formula 
# Calculates the geodesic distance between two points specified by
# radian latitude/longitude using the Haversine formula 

library(purrr)
library(geosphere)
library(rlist)

get_geo_distance = function(long1, lat1, long2, lat2) {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(distance_list, position = 1)
  #if (units == "km") {
  distance = distance_m / 1000.0;

  distance
}

# Applying distance formula for train data
for(i in (1:nrow(Train_Cab)))
{
  Train_Cab$distance[i]= get_geo_distance(Train_Cab$pickup_longitude[i],Train_Cab$pickup_latitude[i],Train_Cab$dropoff_longitude[i],Train_Cab$dropoff_latitude[i])
}

# Lets check data after distance variable creation
head(Train_Cab)

# Lets check whether distance variables has any outlier using boxplot 
ggplot(aes_string(y = 'fare_amount', x = "distance"), data = subset(Train_Cab))+
  stat_boxplot(geom = "errorbar", width = 0.25) +
  geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
               outlier.size=1, notch=FALSE) +theme(legend.position="bottom")+
   ggtitle(paste("Box plot of distnce variable with outliers"))

# Lets check summary of distance variable no outliers 
summary(Train_Cab$distance)

# we can notice this variable has values  which are less than 1 around 2978 in No
# we will replace these values with average distance as the numner of 0's are more
length(Train_Cab$distance[Train_Cab$distance < 1])#2978

Train_Cab$distance[Train_Cab$distance < 1] = mean(Train_Cab$distance)
summary(Train_Cab$distance)

# The data left after all preprocessing 
df = Train_Cab
dim(df)
head(Train_Cab)

# Exploratory Analysis with visualizations after data  cleaning --------------------------------

# Lets check distribution of each numeric and categorical variables
names(Train_Cab)

# Univariate Analysis of continous variables 

# Lets save numeric varaibles 
num_var = c("fare_amount","pickup_longitude" ,"pickup_latitude", "dropoff_longitude",
             "dropoff_latitude","passenger_count","distance")

# Histogram for continuous variables to check  distribution of each variable 

# fare_amount 
ggplot(Train_Cab, aes_string(x = Train_Cab$fare_amount)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("Fare amount") + ylab("Frequency")+ggtitle("distribution of fare_amount")
#right skewed 

# pickup_longitude 
ggplot(Train_Cab, aes_string(x = Train_Cab$pickup_longitude)) + 
  geom_histogram(fill="skyblue", colour = "black",bins = 100,boundry =1) + geom_density() +
  theme_bw() + xlab("pickup_longitude") + ylab("Frequency")+ggtitle("distribution of pickup_longitude")
# almost normally distributed 

# pickup_latitude
ggplot(Train_Cab, aes_string(x = Train_Cab$pickup_latitude)) + 
  geom_histogram(fill="skyblue", colour = "black",bins = 100) + geom_density() +
  theme_bw() + xlab("pickup_latitude") + ylab("Frequency")+ggtitle("Frequency of pickup_latitude")
# almost normally distributed

# dropoff_longitude
ggplot(Train_Cab, aes_string(x = Train_Cab$dropoff_longitude)) + 
  geom_histogram(fill="skyblue", colour = "black",bins = 100) + geom_density() +
  theme_bw() + xlab("dropoff_longitude") + ylab("Frequency")+ggtitle("Frequency of dropoff_longitude")
# almost normally distributed

# dropoff_latitude
ggplot(Train_Cab, aes_string(x = Train_Cab$dropoff_latitude)) + 
  geom_histogram(fill="skyblue", colour = "black",bins = 100) + geom_density() +
  theme_bw() + xlab("dropoff_latitude") + ylab("Frequency")+ggtitle("Frequency of dropoff_latitude")
# almost normally distributed

# passenger_count
ggplot(Train_Cab, aes_string(x = Train_Cab$passenger_count)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("passenger_count") + ylab("Frequency")+ggtitle("distribution of passenger_count")

# distance 
ggplot(Train_Cab, aes_string(x = Train_Cab$distance)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("distance") + ylab("Frequency")+ggtitle(" distribution of distance ")
#right skewed 

# Bivariate Analysis ------------------------------------------------------
# Bar plot for categorical and target variables 

# Visualization between fare_amount and years.
ggplot(data = Train_Cab, aes(x = year,y = fare_amount))+
  geom_bar(stat = "identity",color ="DarkSlateBlue")+
  labs(title = "Fare Amount Vs. year", x = "years", y = "Fare")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.text.x = element_text( color="black", size=6, angle=45))
# We can see, in year 2013 there were rides which got high fare_amount

# Visualization between fare_amount and months.
ggplot(Train_Cab, aes(x = month,y = fare_amount))+ 
  geom_bar(stat = "identity",color = "DarkSlateBlue")+
  labs(title = "Fare Amount Vs. Month", x = "Month", y = "Fare")+
  theme(axis.text.x = element_text( color="navy blue", size=8))
# We can see month March collects the highest fare_amount


# Visualization between fare_amount and weekday.
ggplot(data = Train_Cab, aes(x = day,y = fare_amount))+
  geom_bar(stat = "identity",color = "DarkSlateBlue")+
  labs(title = "Fare Amount Vs. weekday", x = "Days of the week", y = "Fare")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.text.x = element_text( color="black", size=6, angle=45))
# We can see that  Thursday to Saturday rides has the highest fare_amount 

# Visualization between fare_amount and time.
ggplot(data = Train_Cab, aes(x = hour, y = fare_amount))+
  geom_bar(stat = "identity",color = "DarkSlateBlue")+
  labs(title = "Fare Amount Vs.hour", x = "hour", y = "Fare")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme(axis.text.x = element_text( color="black", size=6, angle=45))
# Rides taken during 6 pm to 10 pm gives highest fare_amount
# Lets plot scatter plot for target and continous variables 

# Visualization between fare_amount and pickup_longitude.
ggplot(Train_Cab,aes(pickup_longitude,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w pickup_longitude and fare", x = "pickup_longitude", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

ggplot(Train_Cab,aes(pickup_latitude,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w pickup_latitude and fare", x = "pickup_latitude", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

ggplot(Train_Cab,aes(dropoff_longitude,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w dropoff_longitude and fare", x = "dropoff_longitude", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

ggplot(Train_Cab,aes(dropoff_latitude,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w dropoff_latitude and fare", x = "dropoff_latitude", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()

ggplot(Train_Cab,aes(passenger_count,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w passengercount and fare", x = "passenger_count", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()
# single passenger are frequent travellers 

ggplot(Train_Cab,aes(distance,fare_amount)) + 
  geom_point(alpha=0.5,color="DarkSlateBlue") +
  labs(title = "Scatter Plot b/w distance and fare", x = "Distance", y = "Fare Amount")+
  scale_color_gradientn(colors=c('blue','light blue','dark blue','light green','yellow','dark orange','black')) +
  theme_bw()
# We can say as distance increases fare amount also increases 


# Feature selection ------------------------------------------------------
numeric_index = sapply(Train_Cab,is.numeric)# Selecting only numeric 
numeric_index
numeric_data =Train_Cab[,numeric_index]
cnames = colnames(numeric_data)
cnames

# Correlation Plot for to select significant continous variables 

#correlation matrix
correlation_matrix = cor(Train_Cab[,cnames])
correlation_matrix
#> correlation_matrix
#                    fare_amount   pickup_longitude pickup_latitude dropoff_longitude dropoff_latitude passenger_count distance
#fare_amount        1.000000000       0.17862221    -0.128015223       0.198325233    -0.1114277641   -0.0058405835  0.613251903
#pickup_longitude   0.178622206       1.00000000     0.260474290       0.422659388     0.0733661713   -0.0111382828  0.096693374
#pickup_latitude   -0.128015223       0.26047429     1.000000000       0.040207181     0.5136679924    0.0012413550 -0.121125036
#dropoff_longitude  0.198325233       0.42265939     0.040207181       1.000000000     0.2517345813   -0.0095745379  0.219839696
#dropoff_latitude  -0.111427764       0.07336617     0.513667992       0.251734581     1.0000000000   -0.0002042214 -0.066734644
#passenger_count   -0.005840583      -0.01113828     0.001241355      -0.009574538    -0.0002042214    1.0000000000 -0.006582182
#distance           0.613251903       0.09669337    -0.121125036       0.219839696    -0.0667346444   -0.0065821817  1.000000000
 
#correlation plot
corrgram(Train_Cab[,numeric_index],order = F,upper.panel = panel.pie,
         text.panel = panel.txt,main = 'Correlation plot')

# We can say distance variable is moderately correlated with fare amount 
# rest of the variables also correlated positively and negative but we can 
# say them as weakly correlated we can use them in model 

# Anova Test is performed between cat_var (categorical independent variables) & fare_amount (continuous target variable) 
str(Train_Cab)
names(Train_Cab)
cat_var =c("date","year","month","day","hour")

# aov(Train_Cab$fare_amount~Train_Cab$year)
# for all categorical variables
for(i in cat_var){
  print(i)
  Anova_test_result = summary(aov(formula = fare_amount~Train_Cab[,i],Train_Cab))
  print(Anova_test_result)
}
names(Train_Cab)

# From the anova result, we can observe Date and day 
# has p value > 0.05, so delete this variable not consider in model.
# lets delete date and day variable
Train_Cab$day = NULL
Train_Cab$date = NULL

head(Train_Cab)

# Feature Scaling ---------------------------------------------------------

# In our dataset fare amount and distance are the two continous
# variables whose disribution is slightly skewed

# Checking distance variable distribution using histogram
ggplot(Train_Cab, aes_string(x = Train_Cab$distance)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("distance") + ylab("Frequency")+ggtitle(" distribution of distance ")
# this variable is right skewed 

# Lets take log transformation to remove skewness
# Lets define function for log transformation of variables
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

# Applying log function to distance variable
Train_Cab$distance = signedlog10(Train_Cab$distance)

# Checking distance distribution after applying function
ggplot(Train_Cab, aes_string(x = Train_Cab$distance)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("distance") + ylab("Frequency")+ggtitle(" distribution of distance after log transformation")

head(Train_Cab)


# Model development -------------------------------------------------------
# Let's clean R Environment, as it uses RAM which is limited
library(DataCombine)
rmExcept("Train_Cab")

# Split the data set into train and test 
set.seed(1234)
train.index = createDataPartition(Train_Cab$fare_amount, p = .80, list = FALSE)
train_data = Train_Cab[train.index,]
test_data  = Train_Cab[-train.index,]

#check multicollinearity
library(usdm)
vif(Train_Cab[,cnames])

vifcor(Train_Cab[,cnames], th = 0.9)

#No variable from the 7 input variables has collinearity problem. 

#The linear correlation coefficients ranges between: 
 # min correlation ( distance ~ passenger_count ):  0.0003133683
#max correlation ( dropoff_latitude ~ pickup_latitude ):  0.5237966 

#---------- VIFs of the remained variables -------- 
 # Variables      VIF
#1       fare_amount 1.410975
#2  pickup_longitude 1.385007
#3   pickup_latitude 1.599655
#4 dropoff_longitude 1.426348
#5  dropoff_latitude 1.565798
#6   passenger_count 1.000346
#7          distance 1.396560


# Linear Regression model -------------------------------------------------

# fit linear regression model
# we will use the lm() function in the stats package
lm_model = lm(fare_amount ~.,data = train_data)
summary(lm_model)

#Residual standard error: 9.153 on 12691 degrees of freedom
#Multiple R-squared:  0.3237,	Adjusted R-squared:  0.3212 
#F-statistic:   132 on 46 and 12691 DF,  p-value: < 2.2e-16


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
Predictions_LR_train = predict(lm_model,train_data)
Predictions_LR_test = predict(lm_model,test_data)

#let us check performance of our model

#let us check performance of our model

#mape calculation
LR_train_mape = MAPE(Predictions_LR_train,train_data[,1])
LR_test_mape = MAPE(test_data[,1],Predictions_LR_test)

#Rsquare calculation
LR_train_r2 = rsquare(train_data[,1],Predictions_LR_train)
LR_test_r2 = rsquare(test_data[,1],Predictions_LR_test)

#rmse calculation
LR_train_rmse = RMSE(train_data[,1],Predictions_LR_train)
LR_test_rmse = RMSE(test_data[,1],Predictions_LR_test)


print(LR_train_mape)#1.26
print(LR_test_mape)#0.527
print(LR_train_r2)#0.32
print(LR_test_r2)#0.44
print(LR_train_rmse)#9.13
print(LR_test_rmse)#7.06


#Decision tree regression
library(rpart)
DT_model = rpart(fare_amount ~ ., data = train_data, method = "anova")
DT_model


# Lets predict for train and test data
predictions_DT_train= predict(DT_model,train_data)
predictions_DT_test= predict(DT_model,test_data)

# MAPE calculation
DT_train_mape = MAPE(train_data[,1],predictions_DT_train)
DT_test_mape = MAPE(test_data[,1],predictions_DT_test)

# Rsquare calculation
DT_train_r2= rsquare(train_data[,1],predictions_DT_train)
DT_test_r2 = rsquare(test_data[,1],predictions_DT_test)

# RMSE calculation
DT_train_rmse = RMSE(train_data[,1],predictions_DT_train)
DT_test_rmse = RMSE(test_data[,1],predictions_DT_test)

print(DT_train_mape)#0.338
print(DT_test_mape)#0.324
print(DT_train_r2)#0.456
print(DT_test_r2)#0.599
print(DT_train_rmse)#8.19
print(DT_test_rmse)#6.02


#Random search CV in decision tree
#setting parameters for training using caret library
control = trainControl(method="repeatedcv", number=5, repeats=1,search='random')
maxdepth = c(1:30)
tunegrid = expand.grid(.maxdepth=maxdepth)

# Lets build a model using above parameters on train data 
RDT_model = caret::train(fare_amount~., data=train_data, method="rpart2",trControl=control,tuneGrid= tunegrid)
print(RDT_model)

#lets look best parameter
best_parameter = RDT_model$bestTune
print(best_parameter)
#maximum depth =11

#again build a decsion tree using best parameters
RDT_bestmodel = rpart(fare_amount~.,train_data,method = 'anova',maxdepth=11)
print(RDT_bestmodel)

#lets predict for train and test data
predictions_RDT_train = predict(RDT_bestmodel,train_data)
predictions_RDT_test = predict(RDT_bestmodel,test_data)

#model performance
# MAPE calculation
RDT_train_mape = MAPE(train_data[,1],predictions_RDT_train)
RDT_test_mape = MAPE(test_data[,1],predictions_RDT_test)

# Rsquare calculation
RDT_train_r2= rsquare(train_data[,1],predictions_RDT_train)
RDT_test_r2 = rsquare(test_data[,1],predictions_RDT_test)

# RMSE calculation
RDT_train_rmse = RMSE(train_data[,1],predictions_RDT_train)
RDT_test_rmse = RMSE(test_data[,1],predictions_RDT_test)

print(RDT_train_mape)#0.338
print(RDT_test_mape)#0.324
print(RDT_train_r2)#0.456
print(RDT_test_r2)#0.599
print(RDT_train_rmse)#8.19
print(RDT_test_rmse)#6.02

#Grid search CV decision tree
#setting parameters for training using caret library
control = trainControl(method="repeatedcv", number=5, repeats=2,search='grid')
maxdepth = c(6:30)
tunegrid = expand.grid(maxdepth=maxdepth)

# Lets build a model using above parameters on train data 
GDT_model = caret::train(fare_amount~., data=train_data, method="rpart2",trControl=control,tuneGrid= tunegrid)
print(GDT_model)

#lets look best parameter
best_parameter = GDT_model$bestTune
print(best_parameter)
#maximum depth = 12

#again build a decsion tree using best parameters
GDT_bestmodel = rpart(fare_amount~.,train_data,method = 'anova',maxdepth=12)
print(GDT_bestmodel)

#lets predict for train and test data
predictions_GDT_train = predict(GDT_bestmodel,train_data)
predictions_GDT_test = predict(GDT_bestmodel,test_data)

#model performance
# MAPE calculation
GDT_train_mape = MAPE(train_data[,1],predictions_GDT_train)
GDT_test_mape = MAPE(test_data[,1],predictions_GDT_test)

# Rsquare calculation
GDT_train_r2= rsquare(train_data[,1],predictions_GDT_train)
GDT_test_r2 = rsquare(test_data[,1],predictions_GDT_test)

# RMSE calculation
GDT_train_rmse = RMSE(train_data[,1],predictions_GDT_train)
GDT_test_rmse = RMSE(test_data[,1],predictions_GDT_test)

print(GDT_train_mape)#0.338
print(GDT_test_mape)#0.324
print(GDT_train_r2)#0.456
print(GDT_test_r2)#0.599
print(GDT_train_rmse)#8.19
print(GDT_test_rmse)#6.02

#RANDOM FOREST
#lets build the random forest model
RF_model = randomForest(fare_amount~.,data = train_data,n.trees = 500)
print(RF_model)
#Call:
 # randomForest(formula = fare_amount ~ ., data = train_data, n.trees = 500) 
#Type of random forest: regression
#Number of trees: 500
#No. of variables tried at each split: 3
#Mean of squared residuals: 58.7358 % Var explained: 52.41

#lets predict for both train and test data
predictions_RF_train = predict(RF_model,train_data)
predictions_RF_test = predict(RF_model,test_data)

#MAPE calculation
RF_train_mape = MAPE(predictions_RF_train,train_data[,1])
RF_test_mape = MAPE(predictions_RF_test,test_data[,1])

#Rsquare calculation
RF_train_r2 = rsquare(predictions_RF_train,train_data[,1])
RF_test_r2 = rsquare(predictions_RF_test,test_data[,1])

#RMSE calculation
RF_train_rmse = RMSE(train_data[,1],predictions_RF_train)
RF_test_rmse = RMSE(test_data[,1],predictions_RF_test)

print(RF_train_mape)#0.09
print(RF_test_mape)#0.20
print(RF_train_r2)#0.910
print(RF_test_r2)#0.715
print(RF_train_rmse)#3.67
print(RF_test_rmse)#5.077

#Random search CV random forest= 0.01,trace = T,plot = T)
control = trainControl(method="repeatedcv", number=5, repeats=3,search='random')
#maxdepth = c(1:30)
#tunegrid = expand.grid(maxdepth=maxdepth)

#lets build Random forest model using the above parameters
RRF_model = caret::train(fare_amount~.,data=train_data,method ='rf',trcontrol=control)
print(RRF_model)
best_parameter = RRF_model$bestTune
print(best_parameter)
#mtry = 13
#lets again build the random forest by above paremeters
RRF_bestmodel = randomForest(count~.,data = train_data,method = 'rf',mtry = 13,importance = TRUE)
print(RRF_bestmodel)

#lets predict for both train and test data
prediction_RRF_train = predict(RRF_bestmodel,train_data)
prediction_RRF_test = predict(RRF_bestmodel,test_data)

#MAPE calculation
RRF_train_mape = MAPE(train_data[,1],prediction_RRF_train)
RRF_test_mape = MAPE(test_data[,1],prediction_RRF_test)

#Rsquare calculation
RRF_train_r2 = rsquare(train_data[,1],prediction_RRF_train)
RRF_test_r2 = rsquare(test_data[,1],prediction_RRF_test)

#RMSE calculation
RRF_train_rmse = RMSE(train_data[,1],prediction_RRF_train)
RRF_test_rmse = RMSE(test_data[,1],prediction_RRF_test)

print(RRF_train_mape)
print(RRF_test_mape)
print(RRF_train_r2)
print(RRF_test_r2)
print(RRF_train_rmse)
print(RRF_test_rmse)

#GRID SEARCH CV RANDOM FOREST
#lets set require parameters using caret library
control = trainControl(method="repeatedcv", number=5, repeats=4,search='grid')
maxdepth = c(6:30)
tunegrid = expand.grid(maxdepth=maxdepth)

#lets build Random forest model using the above parameters
GRF_model = caret::train(fare_amount~.,data=train_data,method ='repeatedcv',trcontrol=control,tunegrid=tunegrid)
print(GRF_model)
best_parameter = GRF_model$bestTune
print(best_parameter)
#mtry = 13
#lets again build the same model using bestparameter
GRF_bestmodel = randomForest(count~.,data = train_data,mtry =13,importance = TRUE,method='rf')
print(GRF_bestmodel)

#lets predict on train and test data,
predictions_GRF_train = predict(GRF_bestmodel,train_data)
predictions_GRF_test = predict(GRF_bestmodel,test_data)

#MAPE calculation
GRF_train_mape = MAPE(predictions_GRF_train,train_data[,1])
GRF_test_mape = MAPE(predictions_GRF_test,test_data[,1])

#Rsquare calculation
GRF_train_r2 = rsquare(predictions_GRF_train,train_data[,1])
GRF_test_r2 = rsquare(predictions_GRF_test,test_data[,1])

#RMSE calculation
GRF_train_rmse = RMSE(predictions_GRF_train,train_data[,1])
GRF_test_rmse = RMSE(predictions_GRF_test,test_data[,1])

print(GRF_train_mape)
print(GRF_test_mape)
print(GRF_train_r2)
print(GRF_test_r2)
print(GRF_train_rmse)
print(GRF_test_rmse)

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

# Model evaluation  -------------------------------------------------------

# Lets do all we have done for Train_Cab data

Test_Cab = read.csv("test.csv")

#Check summary ofs dataset 
summary(Test_Cab)

# we are going to change pickup_datetime from factor to datetime
# But first, let's replace UTC in pickup_datetime variable with ''(space)

Test_Cab$pickup_datetime = gsub('// UTC','',Test_Cab$pickup_datetime)

# Now convert variable pickup_dattime to date time format by creating
# new variable with name Date 
Test_Cab$date = as.Date(Test_Cab$pickup_datetime)

# Lets split this new variable Date into year,month,weekday 
# Extract the year
Test_Cab$year = substr(as.character(Test_Cab$date),1,4)

# Extract the month
Test_Cab$month =substr(as.character(Test_Cab$date),6,7)

# Extract the weekday 
Test_Cab$day = weekdays(as.POSIXct(Test_Cab$date),abbreviate = F)

# Extract the date 
Test_Cab$date = substr(as.character(Test_Cab$date),9,10)

# Extract the time 
Test_Cab$hour = substr(as.factor(Test_Cab$pickup_datetime),12,13)

# Let us delete pickup_dataetime as we extracted required substitutes of date 
Test_Cab$pickup_datetime = NULL


# Data after datatype conversion 
Data1 = Test_Cab # will keep a copy of original data 
head(Test_Cab)


# Missing Value Analysis --------------------------------------------------
sum(is.na(Test_Cab)) # no missing values 

# Outlier analysis --------------------------------------------------------
summary(Test_Cab) # no outliers

# Lets calculate distance for test data using function which we already created for train data
# # #Applying distance formula for train data
get_geo_distance = function(long1, lat1, long2, lat2) {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list.extract(distance_list, position = 1)
  #if (units == "km") {
  distance = distance_m / 1000.0;
  
  distance
}


for(i in (1:nrow(Test_Cab)))
{
  Test_Cab$distance[i]= get_geo_distance(Test_Cab$pickup_longitude[i],Test_Cab$pickup_latitude[i],Test_Cab$dropoff_longitude[i],Test_Cab$dropoff_latitude[i])
}

head( Test_Cab)

#write.csv(Test_Cab,"distance_test.csv")

# Lets check whether distance variables has any outlie
summary(Test_Cab$distance)

# distance can not be less than 1 so we replace with average distance
Test_Cab$distance[Test_Cab$distance < 1] = mean(Test_Cab$distance)

# During model development, we deleted few varaibles based on anova test and correlation analysis 
# The variables in the test case should exactly match with the variables in the trained model 
Test_Cab$date = NULL
Test_Cab$day = NULL

# Feature scaling for test data -------------------------------------------

# Checking distance variable distribution using histogram
ggplot(Test_Cab, aes_string(x = Test_Cab$distance)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("distance") + ylab("Frequency")+ggtitle(" distribution of distance ")

# We are going to define function using log
signedlog10 = function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

# Applying log function to distance variable
Test_Cab$distance = signedlog10(Test_Cab$distance)

# Checking distance distribution after applying function 
ggplot(Test_Cab, aes_string(x = Test_Cab$distance)) + 
  geom_histogram(fill="skyblue", colour = "black") + geom_density() +
  theme_bw() + xlab("distance") + ylab("Frequency")+ggtitle(" distribution of distance ")

# Let's look at summary again
summary(Test_Cab$distance)


# Model evaluation using this test data -----------------------------------
# Code for development of model
#RF_model = randomForest(fare_amount~.,data = train_data,n.trees = 500)
#print(RF_model)
# Predicting model on Test_Cab data

RFTest_Cab = predict(RF_model, Test_Cab)

# Adding our obtained predictions as Predicted Fare Amount variable to test_cab dataset

Test_Cab$Predicted_fare_amount = RFTest_Cab

# lets have a look of our predicted fare amount data
head(Test_Cab)
summary(Test_Cab$Predicted_fare_amount)
summary(Train_Cab$fare_amount)

# Finally, we designed a model, which predicts the cab fare.




