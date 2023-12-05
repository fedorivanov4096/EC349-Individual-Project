---
  title: "EC349 Project u2102899 Code RMD"
author: "Fedor Ivanov"
date: "2023-12-04"
output: html_document
---
  ```{r all_code}
install.packages('jsonlite')
install.packages('caret')
install.packages('tidyverse')
install.packages('lubridate')
install.packages('glmnet')
install.packages('hexbin')
install.packages('glue')
install.packages('SentimentAnalysis')
install.packages('SnowballC')
install.packages('tree')
install.packages('rpart')
install.packages('rpart.plot')
install.packages('randomForest')
install.packages('adabag')
install.packages('Hmisc')
install.packages('janitor')
install.packages('tidytext')
install.packages('textdata')
install.packages('corrplot')
install.packages('mlbench')
install.packages('ipred')
library(jsonlite)
library(caret)
library(tidyverse)
library(lubridate)
library(glmnet)
library(hexbin)
library(glue)
library(SentimentAnalysis)
library(SnowballC)
library(tree)
library(rpart)
library(rpart.plot)
library(randomForest)
library(adabag)
library(Hmisc)
library(janitor)
library(tidytext)
library(textdata)
library(corrplot)
library(mlbench)
library(ipred)
library(knitr)
#Clear
cat("\014") 
rm(list=ls())
#Set Directory as appropriate
setwd("C:/Users/Fedor/OneDrive/UNI/R/EC349 Invidual Project")
#Load Different Data Files
business_data <- stream_in(file("yelp_academic_dataset_business.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
checkin_data <- stream_in(file("yelp_academic_dataset_checkin.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
tip_data <- stream_in(file("yelp_academic_dataset_tip.json")) #note that stream_in reads the json lines (as the files are json lines, not json)
load(file='yelp_user_small.Rda')
load(file='yelp_review_small.Rda')
##############################################################################################
#Data generation
checkin_data<-checkin_data %>% 
  mutate(checkins=1+(str_count(date, ","))) %>% 
  select(-date)
#I have replaced the date variable in the check-in data with the number of check-ins as each 
#establishment by counting the number of commas and adding 1. 
#This should have a significant positive correlation with ratings as establishments with higher reviews are 
#more likely to get more visits from people using Yelp Check-in.
all_data <- review_data_small %>% 
  left_join(business_data, suffix = c(".review", ".business"), by=c("business_id")) %>% 
  left_join(user_data_small, suffix = c(".review", ".user"), by=c("user_id")) %>% 
  left_join(checkin_data)
# I start the code by joining the other datasets onto the final review using business_id of user_id.
# Unfortunately, due to the limited processing power available to me, 
# I will have to use the small version of the review and user datasets. 
# The user and review datasets do not match onto each other fully.
#Data unpacking and filtering
all_data <- all_data%>% 
  mutate(stars.review=factor(stars.review)) %>% 
  unnest(attributes) %>% 
  unnest(hours)
#I then unnest the attributes and hours variables to get 88 total variables in my dataset.
all_data<-all_data[complete.cases(all_data[ , 76]),] 
#I have also decided to restrict my dataset to only the observations with user data. 
#This is important as the average stars of the user review has a strong correlation with the reveiw stars. 
#makes sense intuitively as most users only write a very limited number of reviews. 
#This is also later reflected in the correlation matrix I create for all the numeric variables.
all_data<-all_data[complete.cases(all_data[ , 88]),] 
#I remove the observations which have NAs for checkins. 
#This variable has a significant positive correlation, as one would expect. 
#Setting all the NAs to 0 creates significantly worse model results than just dropping the observations, 
#as there are few NAs in this variable. As such I think dropping the NA variables is sensible. 
#Since we still have over 270,000 observations which should be a good amount for creating a model.
#Sentiment analysis code
#One very important, review-specific variable is the text of the review. 
# wanted to take this variable into account because it would allow me to understand cases 
#when the user and business behaved in an uncharacteristic way, such as a generally highly-rated business 
#getting a negative review. In order to implement the sentiment analysis I used the R sentiment analysis package. 
#I will use the pre-built GI library, which has characterised 3642 english words into positive and negative connotations.
small_data<-NULL
stacked_data<-NULL
all_data<-all_data %>%
  rename(review_text = text)
for(x in 1:272){
  small_data<- all_data[((x-1)*1000+1):(1000*x),] %>%
    select(-date) %>%
    left_join(tip_data, multiple = "last") %>%
    select(-date) %>%
    mutate(mood_review=analyzeSentiment(review_text)$SentimentGI) %>%
    mutate(mood_tip=analyzeSentiment(text)$SentimentGI) %>%
    mutate(mood_review=case_when(is.nan(mood_review) ~ 0, TRUE ~ mood_review)) %>%
    mutate(mood_tip=case_when(is.nan(mood_tip) ~ 0, TRUE ~ mood_tip)) %>%
    mutate(mood_review_direction=convertToDirection(mood_review)) %>%
    mutate(mood_tip_direction=convertToDirection(mood_tip))
  stacked_data<-stacked_data %>%
    rbind(small_data)
  print(x)#This is just to keep track of progress
}
stacked_data<-distinct(stacked_data)
small_data=NULL
small_data<- all_data[272001:272209,] %>%
  select(-date) %>%
  left_join(tip_data, multiple = "last") %>%
  select(-date) %>%
  mutate(mood_review=analyzeSentiment(review_text)$SentimentGI) %>%
  mutate(mood_tip=analyzeSentiment(text)$SentimentGI) %>%
  mutate(mood_tip=case_when(is.nan(mood_tip) ~ 0, TRUE ~ mood_tip)) %>%
  mutate(mood_review=case_when(is.nan(mood_review) ~ 0, TRUE ~ mood_review)) %>%
  mutate(mood_review_direction=convertToDirection(mood_review)) %>%
  mutate(mood_tip_direction=convertToDirection(mood_tip))
stacked_data<-rbind(stacked_data, small_data)
save(stacked_data, file="sentiment_data.Rda")
#Here I save the data as the sentiment analysis takes a while to run.
#I load it straight back in
rm(stacked_data)
load(file='sentiment_data.Rda')
#Creating binary variable which shows whether user has left a tip_data
all_data<-stacked_data %>% 
  rename(stars.user=average_stars)
all_data<-all_data %>% 
  mutate(tip_exists=case_when(is.na(text)~0, TRUE~1))
mean(all_data$tip_exists)
#There is only a tip for 5% of observations, so I have chosen to ignore this variable to avoid overfitting.
selected_data <- all_data %>% 
  select(-tip_exists, -mood_tip_direction, -text, -compliment_count, -mood_tip)
# I run the analyzeSentiment function on the review text in order to find the average sentiment score of the words 
# in each review. I also run it over any tips that the user left about the restaurant and the text that was left behind. 
# However, these tips exist for only exist for 5% of observations, so I decided to not proceed with this variable to 
# avoid overfitting.
##############################################################################################
#Data investigation
#NUMERIC VARIABLES
numeric_all<-selected_data %>%
  mutate(stars.review=as.integer(stars.review)) %>%
  select_if(is.numeric)
numeric_all<-numeric_all[complete.cases(numeric_all),]
cor(numeric_all)
corr_mat<-cor(numeric_all)
corrplot(corr_mat, type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, tl.cex=0.5)
#correlation matrix
selected_data<-selected_data %>% 
  select(-latitude, -funny.user, -cool.user, -compliment_hot, -compliment_more, -compliment_profile, 
         -compliment_cute, -compliment_list, -compliment_note, -compliment_plain, -compliment_cool, 
         -compliment_funny, -compliment_photos)
#Dropped variable with less than a 0.01 correlation, as well as the user and compliment characteristics 
#expect the best of each (compliment_writer and useful.user) and these are highly correlated among each other
#SPECIFICS
selected_data<-selected_data %>% 
  select(-review_id, -user_id, -business_id, -review_text, -name.review, -address, -city, -friends, name.user)
#review_id, user_id, business_id, text, date, name.review, address, city, friends, name.user
#are too specific, so they will not be included in the regression directly
#Chi-Sq 
table<-table(selected_data$stars.review, selected_data$DietaryRestrictions)
chisq <- chisq.test(table)
chisq
#Due to the abundance of data a chi-sq test will give us significance, 
#even for clearly sub-optimal variables like Dietary Restrictions which is 99.8% empty
selected_data <-selected_data %>% 
  select(stars.review, useful.review, funny.review, cool.review, state, longitude, stars.business,
         review_count.review, HasTV, Alcohol, GoodForKids, NoiseLevel, review_count.user, useful.user,
         fans, stars.user, compliment_writer,checkins, mood_review_direction) %>% 
  mutate(HasTV=as.factor(case_when(is.na(HasTV)~"None", TRUE~HasTV))) %>% 
  mutate(GoodForKids=as.factor(case_when(is.na(GoodForKids)~"None", TRUE~GoodForKids))) %>% 
  mutate(Alcohol=as.factor(case_when(is.na(Alcohol)~"None",
                                     Alcohol=="u'beer_and_wine'" ~ "'beer_and_wine'",
                                     Alcohol=="u'full_bar'" ~ "'full_bar'",
                                     Alcohol=="u'none'" ~ "'none'",
                                     TRUE~Alcohol))) %>% 
  mutate(NoiseLevel=as.factor(case_when(is.na(NoiseLevel)~"None",
                                        NoiseLevel=="u'quiet'" ~ "'quiet'",
                                        NoiseLevel=="u'average'" ~ "'average'",
                                        NoiseLevel=="u'loud'" ~ "'loud'",
                                        NoiseLevel=="u'very_loud'" ~ "'very_loud'",
                                        TRUE~NoiseLevel))) %>% 
  add_count(state) %>% 
  mutate(state=factor(case_when(n < 100 ~ 'Other', TRUE ~ state))) %>% 
  select(-n)
#I set the NAs to "None" as otherwise they prevent the model from running. This also help to deal with the value of
#"None", which is rare so could lead to overfitting. The documentation offers no explanation for what "None" means.
#I decided to for 4 factor predictors that I thought would have a significant effect, 
#were not directly correlated with average stars and enough data. 
#These were NoiseLevel, Alcohol, HasTV and GoodforKids.
#I talk more about this in the project write-up
##############################################################################################
#Modelling
set.seed(1)
percentage_sampled = 10000/272209-0.0000077 
#The small subtraction is to avoid a rounding error which gaves me 10003 test observations
testIndex <- createDataPartition(all_data$stars.review, p = percentage_sampled, list = 0)
test_reviews=selected_data[ testIndex,]
train_reviews=selected_data[ -testIndex,]
#Tuning grid
#Random Forest main tuning parameters
# mtry: The number of variables that each split considers. This is usually used as the square root of the number 
# of variables, but I will check around to see whether a different number can give a better prediction.
# ntree: The number of trees the model avergaes it over. This should maximised, but has dimishing returns after 
# a point.
# maxnodes: A setting to control the maximum number of terminal nodes.Setting this too high may causeoverfitting, 
# setting too low means the model isn't as accurate.
tune<-function(n_var, n_trees, max_nodes){
  rf_gridsearch <- randomForest(stars.review~.,
                                data=train_reviews, 
                                cp=0.001,
                                mtry=n_var,
                                ntrees=n_trees,
                                maxnodes=max_nodes,
                                type="prob",
                                method=class)
  print(glue("mtry = {n_var} ntree = {n_trees} maxnodes = {max_nodes}"))
  print(" ")
  print(" ")
  pred_RF_train = predict(rf_gridsearch, train_reviews)
  print(confusionMatrix(pred_RF_train, train_reviews$stars.review))
  pred_RF_test = predict(rf_gridsearch, test_reviews)
  print(confusionMatrix(pred_RF_test, test_reviews$stars.review))
}
for(x in 4:4){
  for(y in 2:2){
    for(z in 6:6){
      tune(x,(y*50),(z*1000))
    }
  }
}
# 1. Start with the default number of variables (4 is the closest integer closest to the square root of 19 which is 
# our number of variables). I will keep increasing the number of nodes until I find overfitting. I say that there is 
# significant evidence of overfitting if the model becomes significantly more accurate on the training data than on 
# the test data. In this case this happened when maxnodes was greater than 2000. 
#Beyond 2000 maxnodes, we observe significant differences between
for(x in 2:12){
  for(y in 2:2){
    for(z in 2:2){
      tune(x,(y*50),(z*1000))
    }
  }
}
# 2. I then vary the number of variables we include in each split of the random forest, 
# starting from 2 all the way up to 12. Here we see that the best prediction is when the number of variables is 
# much bigger than 4. This is because we have a few variables which are better predictors than all of the others, 
# such as stars.user (I will show this on a graph later). This means random forest is not the best model for
# simulating this data, but as discussed earlier, I was unable to run a boosting algorithm due to computational 
# limitations, and a bagging produced, very bad results.
for(x in 8:8){
  for(y in 2:4){
    for(z in 2:2){
      tune(x,(10^y),(z*1000))
    }
  }
}
# 3. Once we settle on an optimal mtry of 8, we can now start to vary the number of trees until the increase no 
# longer significantly changes the results.
##############################################################################################
#Evaluation
#Final model
set.seed(1)
model_RF<-randomForest(stars.review~.,data=train_reviews, ntree=1000, cp=0.001, method=class, mtry=8, type="prob", maxnodes=2000)
pred_RF_train = predict(model_RF, train_reviews)
confusionMatrix(pred_RF_train, train_reviews$stars.review)
pred_RF_test = predict(model_RF, test_reviews)
confusionMatrix(pred_RF_test, test_reviews$stars.review)
varImpPlot(model_RF)
#Longer discussion of this in write-up
#Bagging model code (this model did not perform well)
# model_bag<-bagging(stars.review~.,data=train_reviews,nbagg = 100, coob = TRUE, control = rpart.control(minsplit = 2, cp = 0.001))
# 
# 
# pred_bag_train = predict(model_bag, train_reviews)
# confusionMatrix(pred_bag_train, train_reviews$stars.review)
# 
# 
# pred_bag_test = predict(model_bag, test_reviews)
# confusionMatrix(pred_bag_test, test_reviews$stars.review)
# #Boosting code (this took too long to run)
# model_adaboost <- boosting(stars.review~., data=train_reviews, boos=TRUE, mfinal=50)
# summary(model_adaboost)
# 
# pred_ada_test = predict(model_adaboost, test_review)
# pred_ada_test
#Exporting to PDF:
knitr::stitch_rhtml('EC349 Project u2102899 Code RMD.Rmd') 
knitr::stitch_rhtml('EC349-Individual-Project-Writeup.RmD') 
