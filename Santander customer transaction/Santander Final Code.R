 #loding required libraries 
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
library(usdm)
library(pROC)
library(caret)
library(rpart)
library(DataCombine)
library(ROSE)
library(e1071)
library(xgboost)
  
#let us clear our r environment
rm(list=ls())
  
#set working diretory
setwd("D:/Data Science/Assignments/Project")
  
#load test and train data
train =read.csv('train.csv')
test =read.csv('test.csv')

#check dimension of train and test data
dim(train)
dim(test)
  
#basic Descriptive stats 
summary(train)
summary(test)
#from the above summary, we came to know that mean and median of all variables are almost same
  
#separating ID_code variable from train and test data 
train_ID = train$ID_code
test_ID  = test$ID_code
  
# Removing the ID_code from the train and test data
train$ID_code=NULL
test$ID_code=NULL
  
#check dimension of dataset after removing column
print(dim(train))
print(dim(test))
  
#count of target variable 
table(train$target)
  
#Missing value analysis
# this function takes dataframe as input and calulate precentage of missing values in each 
# columns and returns that dataframe 
  
  findMissingValue =function(df){
    missing_val =data.frame(apply(df,2,function(x){sum(is.na(x))}))
    missing_val$Columns = row.names(missing_val)
    names(missing_val)[1] =  "Missing_percentage"
    missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(train)) * 100
    missing_val = missing_val[order(-missing_val$Missing_percentage),]
    row.names(missing_val) = NULL
    missing_val = missing_val[,c(2,1)]
    return (missing_val)
  }
############# No missing value in test and train data ###########
  
  
# creating target and independent variable from train dataset
independent_var= (colnames(train)!='target')
X=train[,independent_var]
Y=train$target
  
  
#Multicolinearity Analysis
#checking is variable are correlated
cor=vifcor(X)
print(cor)
############## No varible are correlated ################
 

# Distribution plot
#This function plots distribution plot from given data set
  plot_distribution =function(X)
  {
    variblename =colnames(X)
    temp=1
    for(i in seq(10,dim(X)[2],10))
    {
      plot_helper(temp,i ,variblename)
      temp=i+1
    }
  }
  
  # helper function takes start and stop index to print subset distribution plot
  plot_helper =function(start ,stop, variblename)
  { 
    par(mar=c(2,2,2,2))
    par(mfrow=c(4,3))
    for (i in variblename[start:stop])
    {
      plot(density(X[[i]]) ,main=i )
    }
  }
# plot density plot for trainig data 
  plot_distribution(X)
#plot density plot for testing data
  plot_distribution(test)
  
# Allmost all variables are normally distributed
# Both dataset are similar to each other in terms of distribution
 
  
#outlier analysis
#loop to remove outlier from all variables
  
  cnames = colnames(train[,3:201])
  for(i in cnames){
    print(i)
    val = train[,i][train[,i] %in% boxplot.stats(train[,i])$out]
    print(length(val))
    train_df = train[which(!train[,i] %in% val),]
    
  }
  
  
###STANDARDIZATION
  
# This function takes data frame as input and standardize dataframe
# df =data frame 
# formula=(x-mean(x))/sd(x) ##x is total no of observation##
#function to perform standardisation
  standardizing=function(df)
  {
    cnames =colnames(df)
    for( i in   cnames ){
      df[,i]=(df[,i] -mean(df[,i] ,na.rm=T))/sd(df[,i])
    }
    return(df)
    
  }
  
#standardize train data 
X=standardizing(X)
  
#standardise test data
test =standardizing(test)
  
# combine independent and dependent variables
train_STD = cbind(X,Y)
  
#Let split test and train data using statifed sampling method(MODEL 1)
set.seed(123)
train.index =createDataPartition(train_STD$Y , p=.70 ,list=FALSE)
#here we took over 70% of data from the original data set and remaing is in test

train = train_STD[train.index,]
test  = train_STD[-train.index,]
  
#random over sampling (MODEL 2)
train_rm= ovun.sample(Y~. ,data =train  , method='over' )$data
  
#function for evaluation metrics of trained data
  getmodel_accuracy=function(conf_matrix)
  {
    model_parm =list()
    tn =conf_matrix[1,1]
    tp =conf_matrix[2,2]
    fp =conf_matrix[1,2]
    fn =conf_matrix[2,1]
    p =(tp)/(tp+fp)
    r =(fp)/(fp+tn)
    f1=2*((p*r)/(p+r))
    print(paste("accuracy",round((tp+tn)/(tp+tn+fp+fn),2)))
    print(paste("precision",round(p ,2)))
    print(paste("recall",round(r,2)))
    print(paste("fpr",round((fp)/(fp+tn),2)))
    print(paste("fnr",round((fn)/(fn+tp),2)))
    print(paste("f1",round(f1,2)))
    
  }
####MODEL TRAINING#####
  
#LOGISTIC REGRESSION
  
#fitting logistic model BASE(MODEL 1)
over_logit =glm(formula = Y~. ,data =train ,family='binomial')


#get model predicted  probality 
y_prob =predict(over_logit , test[-201] ,type = 'response' )


# convert   probality to class according to thresshold
y_pred = ifelse(y_prob >0.5, 1, 0)


#create confusion matrix 
conf_matrix= table(test[,201] , y_pred)
  

#print model accuracy
getmodel_accuracy(conf_matrix)


# get auc 
roc=roc(test[,201], y_prob)
print(roc )


# plot roc _auc plot 
plot(roc ,main ="Logistic Regression base Roc ")
  
  ################## MODEL 1 ###########
  #[1] "accuracy 0.91"
  #[2] "precision 0.69"
  #[3] "recall 0.01"
  #[4] "fpr 0.01"
  #[5] "fnr 0.73"
  #[6] "f1 0.03"
  #Area under the curve: 0.86
  
  
  # accuracy of model is very good 91%
  # model has very low recall 1% only
  # f1 score is just 3%
  # very low very poor model

################## OVER SAMPLING(MODEL 2) ####################
  
#fitting logistic model
over_logit =glm(formula = Y~. ,data =train_rm ,family='binomial')


#PREDICTING PROBALITY
y_prob =predict(over_logit , test[-201] ,type = 'response' )


# convert   probality to class according to thresshold
y_pred = ifelse(y_prob >0.5, 1, 0)


#create confusion matrix 
conf_matrix= table(test[,201] , y_pred)


#print model accuracy
getmodel_accuracy(conf_matrix)


#get Auc 
roc=roc(test[,201], y_prob )
print(roc)


#plot roc curve 
plot(roc ,main="Logistic Regression roc-auc oversampled")


  ####### MODEL 2 ##################
  #[1] "accuracy 0.78"
  #[2] "precision 0.28"
  #[3] "recall 0.22"
  #[4] "fpr 0.22"
  #[5] "fnr 0.22"
  #[6] "f1 0.25"
  #Area under the curve: 0.8596

  # accuracy of model is ok 78%  but has decreased from 92%
  #fpr and fnr rate is 22% which is ok
  #f1 score is 25% which is very low  but base model had only 3%
  #auc is 86% which is good 
  #overall better than base logistic function 
  

#DECISION TREE
#USING OVERSAMPLED DATA(MODEL 1)
  
#train model
rm_model = rpart( Y~. ,data =train_rm )


#PREDICTING PROBALITY
y_prob =predict(rm_model , test[-201] )

# convert   probality to class according to thresshold
y_pred = ifelse(y_prob >0.5, 1, 0)

#create confusion matrix 
conf_matrix= table(test[,201] , y_pred)

#print model accuracy
getmodel_accuracy(conf_matrix)

#get Auc score 
roc=roc(test[,201], y_prob )
print(roc)

# plot roc_auc curve 
plot(roc ,main="Roc _ auc Decision tree (Over sampled)")
  #####################  MODEL 1  #######################
  # [1] "accuracy 0.65"
  # [1] "precision 0.14"
  # [1] "recall 0.33"
  # [1] "fpr 0.33"
  # [1] "fnr 0.51"
  # [1] "f1 0.2"
  #Area under the curve: 0.5843
  
  # model perform poorer  than Logistic regression model 
  # f1 score is only 20%
  # fnr rate is 51 % which is high 
  # precision recall is very poor 
  # Roc _ auc  around 58%  over all model is very poor 
  
  #coverting target back to 0's and 1's 
  train_rm$Y =ifelse(train_rm$Y =='No',0,1)
  

########### Manually trying to reduce depth of tree with c =0.1 (MODEL 2) ########################

#rpart control variable 
ctrl =rpart.control(cp = 0.01,maxdepth = 5)

# train model 
rm_model = rpart( Y~. ,data =train_rm , control = ctrl)

#predicting probality
y_prob =predict(rm_model , test[-201] ,type='prob')

# convert   probality to class according to thresshold (positive class )
y_pred = ifelse(y_prob[,2] >0.5, 1, 0)

#create confusion matrix 
conf_matrix= table(test[,201] , y_pred)

#print model accuracy
getmodel_accuracy(conf_matrix)
 
  
#get auc 
roc=roc(test[,201], y_prob[,2] )
print(roc)

# plot roc_auc curve
plot(roc ,main="Roc _ auc Decision tree Model  2")
  
  
  
  #######   MODEL 2  ######
  #[1] "accuracy 0.55" 
  #[1] "precision 0.14"
  #[1] "recall 0.47" 
  #[1] "fpr 0.47" 
  #[1] "fnr 0.33" 
  #[1] "f1 0.21" 
  #Area under the curve: 0.6124
  
  # accuracy of model has decresed from 80% to 55%
  # recall is incresed  47% 
  # fnr has decreased to 33%
  # f1 score has also increased by 1 %
  # This model performs better than base and tunned model still inferior to Logistic Regression  model 

#NAIVE BAYES
#USING NON OVER SAMPLED DATA(MODEL 1)

# coverting target to factor 
train$Y = factor(train$Y ,levels = c(0,1))

# train model 
nb_model  =naiveBayes(Y~.  , data =train )  

#PREDICTING PROBALITY
y_prob =predict(nb_model , test[-201]  ,type='raw')

# convert   probality to class according to thresshold
y_pred = ifelse(y_prob[,2] >0.5, 1, 0)

#create confusion matrix 
conf_matrix= table(test[,201] , y_pred)

#print model accuracy
getmodel_accuracy(conf_matrix)
  
# get Auc 
roc=roc(test[,201], y_prob[,2] )
print(roc)

# plot Roc_Auc curve 
plot(roc ,main="Roc _ auc  Naive Bayes MODEL 1 ")
  
  ##################  MODEL 1 ##############
  # [1] "accuracy 0.92"
  # [1] "precision 0.72"
  # [1] "recall 0.02"
  # [1] "fpr 0.02"
  # [1] "fnr 0.63"
  # [1] "f1 0.03"
  #Area under the curve: 0.8894 
 
  # accuracy of model is good 92%
  #roc_auc is 88% which also good.  
  # but fnr is high 64%  and f1 score very poor only 3%
  # very poor  model performance 
  
#USING OVER SAMPLED DATA(MODEL 2)  

#train model 
nb_model  =naiveBayes(Y~.  , data =train_rm  )  
  
#PREDICTING PROBALITY
y_prob =predict(nb_model , test[-201]  ,type='raw')

# convert   probality to class according to thresshold
y_pred = ifelse(y_prob[,2] >0.5, 1, 0)

#create confusion matrix 
conf_matrix= table(test[,201] , y_pred)

#print model accuracy
getmodel_accuracy(conf_matrix)

# get auc 
roc=roc(test[,201], y_prob[,2] )
print(roc)

# plot Roc_auc curve 
plot(roc ,main="Roc _ auc  Naive Bayes MODEL 2 ")
  

  ###############   MODEL 2 ##############
  # [1] "accuracy 0.81"     
  # [1] "precision 0.32"   
  # [1] "recall 0.19" 
  # [1] "fpr 0.19" 
  # [1] "fnr 0.21"
  # [1] "f1 0.24" 
  # Area under the curve: 0.8893
  
  # accuracy of model has decreased to 81%
  # precision has reduced from 72% to 32%
  # recall for model has increased from  2% to  18%
  # fpr have increased from 2% to 18% 
  # fnr has decreased from 64% to 21 %
  # f1 score has increased from 3% to 23%
  # Auc is same 
  # model has improved but this is also poor model ,still best so far model slightly better than logistic regression
  
#XGBOOST MODEL

# convertion target varible to factor 
train$Y <- as.numeric(as.factor(train$Y)) - 1 
test$Y <- as.numeric(as.factor(test$Y)) - 1 
train_rm$Y <- as.numeric(as.factor(train_rm$Y)) - 1 

# coverting data into dmatrix as it required in xgboost 
trainD =xgb.DMatrix(data =as.matrix(train[,-201]),label= train$Y)
testD =xgb.DMatrix(data=as.matrix(test[,-201]) ,label  =test$Y)
train_rmD =xgb.DMatrix(data =as.matrix(train_rm[,-201]) ,label=train_rm$Y)

#USING NON OVER SAMPLED DATA(MODE 1)

###prameters  used 
# max.depth : max depth tree is allowed to grow
# eta: similar to learing rate 
# nrounds: maximum round algorithmis allowed to run 
# scale_pos_weight: make postive weight 11 times more than neagtive 

#train model  
 xgb1 = xgb.train(
    data = trainD,
    max.depth = 3,
    eta = 0.1,
    nrounds = 500,
    scale_pos_weight =11,
    objective = "binary:logistic"
  )
  
#PREDICTING PROBALITY
y_prob =predict(xgb1 , as.matrix(test[,-201] ) )

# convert   probality to class according to thresshold
y_pred = ifelse(y_prob >0.5, 1, 0)

#create confusion matrix 
conf_matrix= table(test[,201] , y_pred)

#print model accuracy
getmodel_accuracy(conf_matrix)

# get roc 
roc=roc(test[,201], y_prob )
print(roc)

# plot roc
plot(roc ,main="Roc _ auc  xgboost MODEL 1 ")


########## MODEL 1 ###############
  # [1] "accuracy 0.81"
  # [1] "precision 0.32"
  # [1] "recall 0.18"
  # [1] "fpr 0.18"
  # [1] "fnr 0.21"
  # [1] "f1 0.23"
  # Area under the curve: 0.8845

  # accuracy of model is  81%
  # precision is  32%
  # recall is  18%
  # fpr  is 18% 
  # fnr is 21 %
  # f1 score is  23%
  # Auc is  .8845
  # This model performs allmost same as naive bayes with over sampled dataset
  # only difference is that naive bayes model has sightly low auc by .0048
  
#USIN OVER SAMPLED DATA(MODEL 2)

# train model
xgb1 = xgb.train(
  data = train_rmD,
  max.depth = 3,
  eta = 0.1,
  nrounds = 500,
  scale_pos_weight=2,
  objective = "binary:logistic"
)

#PREDICTING PROBALITY
y_prob =predict(xgb1 , as.matrix(test[,-201] ) )

# convert   probality to class according to thresshold
y_pred = ifelse(y_prob >0.5, 1, 0)

#create confusion matrix 
conf_matrix= table(test[,201] , y_pred)

#print model accuracy
getmodel_accuracy(conf_matrix)

# get roc 
roc=roc(test[,201], y_prob )
print(roc)

# plot roc
plot(roc ,main="Roc _ auc  xgboost MODEL 2 ")

  #########  MODEL 2 ############
  # [1] "accuracy 0.71" 
  # [1] "precision 0.24" 
  # [1] "recall 0.3" 
  # [1] "fpr 0.3" 
  # [1] "fnr 0.13" 
  # [1] "f1 0.27" 
  # Area under the curve: 0.8822

  # accuracy of model has decreased from  81% to 71% compared to MODEL 1 
  # precision of model has decreased from 32% to 24 % compared to MODEL 1
  # recall of model has increased  from 18% to 30% compared to MODEL 1
  # fpr of model has increased from 18% to 30 % compared to MODEL 1
  # fnr of model has decreased from 21% to 13 % compared to MODEL 1
  # f1 score  of model has increasd  from 23% to 27 % compared to MODEL 1 
  # going by fnr and f1 score MODEL 2 is improvement over  MODEL 1 


    

   ########## FINAL MODEL SELECTION AND CONCLUSION#########

  # looking at all models with different algorithms 
  # naive bayes MODEL 2  is the best model
  # naive bayes has balanced fnr and fpr
  # f1 score is amongs the highest achieved so far 
  # auc is also over 88%
  
  ### below is model report  of naive bayes MODEL 2 
    # [1] "accuracy 0.81"      - 
    # [1] "precision 0.32"   -
    # [1] "recall 0.18" +
    # [1] "fpr 0.18" +
    # [1] "fnr 0.21" -
    # [1] "f1 0.23" +
    # Area under the curve: 0.8865
    