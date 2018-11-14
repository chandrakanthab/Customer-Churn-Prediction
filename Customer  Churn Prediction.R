#get Working directory

setwd("D:/Edwisor assignments/churn and unchurn")

getwd()



#  Load  Require Libraries



#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)

# Install  Libraries
library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")
library(tidyr)


#  Function to Converty  Data Types as  Factor  or numeric   based on given type

convert_factor_type= function(df,cat_names,convert_type) {
  
  if (convert_type== "factor")
    df[cat_names] <- lapply(df[cat_names], as.factor)
  else 
    df[cat_names] <- lapply(df[cat_names], as.numeric)
  df
}

# This Function will take input as data frame  and Numeric Columns and gives output as
# box plot relation ship between  Target Variable and  Independent numeric variable

plot_box = function(df, cols, col_x = 'Churn'){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    p = ggplot(df, aes_string(col_x, col)) + 
      geom_boxplot() +
      ggtitle(paste('Box plot of', col, '\n vs.', col_x))
    print(p)
  }
}


# This Function will take input as data frame  and Numeric Columns and gives output as
# Violin  plot relation ship between  Target Variable and  Independent numeric variable


plot_violin = function(df, cols, col_x = 'Churn'){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    p = ggplot(df, aes_string(col_x, col)) + 
      geom_violin() +
      ggtitle(paste('Box plot of', col, '\n vs.', col_x))
    print(p)
  }
}





# This  Function will  take data frame and categorical columns as input
# and give  group plots between  independenta and target variable

plot_group_bar <- function(data,cat_columns,col_y="Churn"){
  for(col in cat_columns) {
    
    plot=ggplot(data) + geom_bar(aes_string(x=col,fill=col_y),position = "dodge")
    
    print(plot)
  }
}

# This Function will take dataframe  and numeric columns as input and 
# it treat outliers  using  boxplot and  return dataframe  after treating
treat_outliers  <- function(data,numeric_columns) {
  
  for (col in numeric_columns) {
    val = data[,col][data[,col] %in% boxplot.stats(data[,col])$out]
    
    df_churn_out = data[which(!data[,col] %in% val),]
    
  }  
  df_churn_out
}

# this  function will take data and categorical variables and gives chisquare
# p values as  output
chi_square_test  <- function(data,cat_col,target="Churn") {
  
  for (col in cat_col)
  {
    print(col)
    print(chisq.test(table(data[,target],data[,col])))
  }
}



# this function  will take data frame and numeric data as input and give 
# dataframe as output after  convering  numeric variables values into standardization form
standardForm_convert  <- function(data,num_col) {
  
  for(col in num_col){
    print(col)
    data[,col] = (data[,col] - mean(data[,col]))/sd(data[,col])
  }
  data
}



# This Function will take Actual y value and Predicted  values and it will give
# Output as  Accuracy , Precision , Recall etc
model_evaluation <- function(test_y,predicted_y) {
  
  table_matrix= table(test_y,predicted_y)
  print (confusionMatrix(table_matrix))
  precision=  table_matrix[4]/(  table_matrix[4] +  table_matrix[3])
  print(paste("Precision  is--" ,precision))
  recall  = table_matrix[4]/(  table_matrix[4] +  table_matrix[2])
  print(paste("recall  is--" ,recall))
  Accuracy = sum(diag(table_matrix)) / sum(table_matrix)
  print(paste("Accuracy   is--" ,Accuracy))
  
  FNR  = table_matrix[2]/(  table_matrix[4] +  table_matrix[2])
  print(paste("FNR     is--" ,FNR))
}

#  This function will take  data frame and categorical  as iput and gives output as data frame with encoded categorical data
encode_categorical  <- function(data,cat_columns) {
  
  for(col in cat_columns ) {
    data[,col]=as.numeric(as.factor(data[,col]))
  }
  data
}

# this function  will take data frame and numeric data as input and give 
# dataframe as output after  convering  numeric variables values into standardization form


standardForm_convert  <- function(data,num_col) {
  
  for(col in num_col){
    print(col)
    data[,col] = (data[,col] - mean(data[,col]))/sd(data[,col])
  }
  data
}






#Load Customer Churn Train and test Data

df_churn_train=read.csv("Train_data.csv")
df_churn_test=read.csv("Test_data.csv")


# drop  Phone number column from data frame 

df_churn_train= subset(df_churn_train,select=-c(phone.number))
df_churn_test= subset(df_churn_test,select=-c(phone.number))

# understanding  data 

head(df_churn_train)

# Summary Of Data

summary(df_churn_train)

# this data set contains 3333 rows and 20 columns  out of this 20 columns  five columns are  categorical and remaining  
#columns are  Numeric

# It is showing  that Mean of the  total_day__minutes,total_eve__minutes,total_night__minutes and total_day__calls
#total_night__calls and total_eve__calls are almost looking the same  and we have to  check how is the  co-releation between
#  between variables  to the other variables




colnames(df_churn_train)

# Catgorical Column in the  Data frame
cat_columns=c("state","area.code","international.plan","voice.mail.plan","Churn")

# Convert Given given  Categorical Columns into type  factor
df_churn_train <-convert_factor_type(df_churn_train,cat_columns,"factor")

str(df_churn_train)

# Numerical Column s in the  data Frame
nums_column <-names(df_churn_train)[sapply(df_churn_train, is.numeric)]

# COnvert all numeric column into type  numerics
df_churn_train <-convert_factor_type(df_churn_train,cat_columns,"factor")

# Univariate  Analysis


# in the  below  Histogram graph it is  showing that almost all the variables are  normally distributes  except
#number_vmail_message,number_customer_service_calls and and total_initoial_calls
# if you see the ranges  between  the variables  number_customer_service_calls  having  less range (0.7.5) and highest range
# is having for total night minutes  nearly (0,400)

ggplot(gather(df_churn_train[,nums_column]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')
 

#analyse   Target Variable
table(df_churn_train$Churn)

barplot(table(df_churn_train$Churn), main ="Churn Distribution")
#  Customer Churn False Proportion is  more than than True  here chance of class
# Imbalance  we should concentrate more  during  evaluation of medel

############################# Bivariate Analysis

# Bivariate  Analysis  between Numerical Variable and target  Variable  using Boxplot
plot_box(df_churn_train,nums_column)

######################

#Bivariate  Analysis  between Numerical Variable and target  Variable  using Violin plot

plot_violin(df_churn_train,nums_column)

#showing  that  “Total_day_charge” , “Total_intl_charge” and “Number_customer_service_charge”  FOr medians ,IQR and Ranges of  Boxplot is different  for “Unchur” and “Churn”  
#so these features are  clearly showing  are important to prediction.

#For other features  Boxplot  Median , IQeeriR, Ranges are  looking  almost same. Here it is stating  Feature Engineering is important to find the relationship 
#between  the variables.

##############  Bivariate Analysis between Categorical Variables  ##########
                                
##

cat_ind_columns = c("state" ,"area.code","international.plan","voice.mail.plan")

plot_group_bar(df_churn_train,cat_ind_columns)


################ Missing  Values  ###############

#As  summary Function shows there is not missing  value present in the data

###########  Outlier  Analysis  ###################

#As wecame to know that ring  Univariate  there are utliers in few columns
#so will  treat those outliers and will chick analyse  what is the impact


# Create  one dummy   Data frame and copy train data frame df_churn_T
df_churn_T= treat_outliers(df_churn_T,nums_column)
# check the  dimensions of data frame
dim(df_churn_T)   # it contains 3066 rows and 20 columns

# Plot  univariate   plot for all  numeric variables 



ggplot(gather(df_churn_T[,nums_column]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

#  We are losing almost 10% of data  after treating aoutliers
#  after removing  most of the data losing for "number_of_customers_calls" and and still right skewness is present for
# "number_vmail_message" variable and "total_intl_calls" variable
# those deleted information might be the  important information especially for "number_of_customers_calls" so, here going to develop
# the model without treating outliers


##################### Feature Selection ###########################

# verify correleation between   Numeric variable

corrgram(df_churn_train[,nums_column], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

# This plot is showing clearly that relation ship between "total_day_charge- total_day_minutes" , "total_night_charge- total_night_minutes"
# "total_eve_charge- total_eve_minutes" and "total_intl_charge- total_intl_minutes" are very high so out of this any one variable 
# is require   to build the model

nums_column

df_churn_train = subset(df_churn_train,select=-c(total.day.minutes,total.eve.minutes,total.night.minutes,total.intl.minutes))
df_churn_test = subset(df_churn_test,select=-c(total.day.minutes,total.eve.minutes,total.night.minutes,total.intl.minutes))

##  Verify   variable importance of  categorical variable using chi square test

# analyse chi-square test p values for all independent categorical variable

chi_square_test(df_churn_train,cat_ind_columns)

#p - values fo columns
#  state =0.002296
# area_code = 0.9151
#international.plan=2.2e-16
#voice.mail.plan = 5.151e-09
#It is  showing  clearly that  relation ship  between "area_code" and "Churn" is very low  so better to drop this  column

df_churn_train=subset(df_churn_train,select=-c(area.code))
df_churn_test=subset(df_churn_test,select=-c(area.code))

dim(df_churn_train)
dim(df_churn_test)

#  one more step in  one variable  account length which is  seems like categorical , might be account length are small are 
# old accounts and Churn rate may be more
# will turn account numbers into ranges and make it as categorical columns


df_churn_train[,"acc_length"] <-cut(df_churn_train$account.length, breaks=c(0,50,100,150,200,250),labels =c("0","1","2","3","4") )
df_churn_test[,"acc_length"] <-cut(df_churn_test$account.length, breaks=c(0,50,100,150,200,250),labels =c("0","1","2","3","4") )

str(df_churn_test) 
#  Now drop the actual account length  from train and test data

df_churn_train=subset(df_churn_train,select=-c(account.length))
df_churn_test=subset(df_churn_test,select=-c(account.length))

dim(df_churn_train)
dim(df_churn_test)


#############################################  Scaling Data ##########################################################
# As we see that almost all the  numeric variables are in  normalal distribution except  two variables
# since our data is also contains  few Outliers  we are better to go standardization for scaling

str(df_churn_train)

#  get numeric columns  from data frame

nums_column_1 <-names(df_churn_train)[sapply(df_churn_train, is.numeric)]

# this function  will take data frame and numeric data as input and give 
# dataframe as output after  convering  numeric variables values into standardization form

df_churn_train = standardForm_convert(df_churn_train,nums_column_1)
df_churn_test=standardForm_convert(df_churn_test,nums_column_1)
View(df_churn_train)

#### encoding  categorical variables 

# this function will take   data frame and categorical values as  input
#  encoding  in numerica  form and return data frame

cat_columns_1=c("state","international.plan","voice.mail.plan","Churn")

df_churn_train<-encode_categorical(df_churn_train,cat_columns_1)
df_churn_test<-encode_categorical(df_churn_test,cat_columns_1)


## Preapare  train and test data 

x_train =  subset(df_churn_train,select=-c(Churn))
y_train = subset(df_churn_train,select=c(Churn))
x_test = subset(df_churn_test,select = -c(Churn))
y_test = subset(df_churn_test,select=-c(Churn))

dim(x_train)
dim(x_test)
dim(y_train)
dim(y_test)

#######################################  Model   Decision Tress  ###################################

install.packages("rpart.plot")


library(rpart)
library(rpart.plot)

#  Build Decision tree Model and Evaluate  the Model

decition_tree_fit = rpart(Churn~., data = df_churn_train, method = 'class')

#  Prune decision tree using  below paameters 
# minsplit=5, minbucket= misplit/3  and maxdepth = 10
pune_tree=prune(decition_tree_fit,cp=.01, minsplit=5, minbucket=round(minsplit/3),maxdepth=10)                   


# Predict Test data  Churn using Predict function

predict_prune <-predict(pune_tree, df_churn_test, type = 'class')

############# Model Evaluation  #

model_evaluation(df_churn_test$Churn, predict_prune)


#"Precision  is-- 0.907407407407407"
# "recall  is-- 0.65625"
# "Accuracy   is-- 0.944811037792441"
# "FNR     is-- 0.34375"

############ Build Randam forest Model ###################


random_rf=randomForest(Churn ~., data =df_churn_train ,ntree=500 ,nodesize =10 ,importance =TRUE)

random_rf

predict_rf <-  predict(random_rf, x_test)
##### Evaluation  Random FOrest


model_evaluation(df_churn_test$Churn, predict_rf)

# Performance of this model is  good  when compare  to decision  Tree

# Here Precision = 0.97 
#  recall is  0.72 
#and  F1 score is 0.95








