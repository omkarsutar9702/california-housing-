setwd("D:/R files/R_file/california housing")
#import libraries
library(MASS)
library(dplyr)
library(ggplot2)
library(rpart)
library(e1071)
library(leaps)
library(caret)

#import data set
housing <- read.csv("D:/R files/R_file/california housing/housing.csv", stringsAsFactors=TRUE)
#explore and visualize the data
dim(housing)
str(housing)
summary(housing)
sum(is.na(housing))
levels(housing$ocean_proximity)

#plot
housing_num<-subset(housing , select = -c(ocean_proximity))

par(mfrow=c(3,3))
invisible(lapply(names(housing_num),
                 function(col_names)truehist(housing_num[,col_names],
                                             main=paste("histogram of " , col_names) , 
                                             xlab = "")))

#scatter plot with ggplot
g <- ggplot(housing, aes(x = longitude, y = latitude, colour = median_income))
g + geom_point() + scale_color_distiller(palette = "Spectral") +
  labs(title = "Plot of data points by location and median_income") + 
  theme(plot.title = element_text(color="black", size=14, face="bold.italic"))


cor(subset(housing , select = -c(ocean_proximity)) ,
    use = "pairwise.complete.obs")[,"median_house_value"]

#dealing with missing data
housing_new<-subset(housing , select = -c(total_bedrooms))#delete this column
housing_new<- housing_new[complete.cases(housing_new),]#remove missing entries
sum(is.na(housing_new))

housing_new<-housing_new %>% 
  filter(median_house_value < 500000) %>% 
  mutate(rooms_per_house = total_rooms / households) %>% 
  mutate(population_per_house = population / households) %>% 
  mutate(ocean_proximity = as.factor(ocean_proximity)) %>% 
  mutate_at(vars(-ocean_proximity , -median_house_value , -median_income) , funs(scale)) %>% 
  data.matrix %>% data.frame

#random sampling
set.seed(10)
train_index<-sample(nrow(housing_new) , size = 0.8*nrow(housing_new))
train_set<-housing_new[train_index ,]
test_set<-housing_new[-train_index,]
print(paste(nrow(train_set),"train +" , nrow(test_set) , "test"))

#strtified sampling
par(mfrow = c(1,2))
truehist(housing_new[,"median_income"] , main="histgram of medain income", xlab=NA)


housing_new<-housing_new %>%  #categorize median income
  mutate(income_level = ceiling(median_income/2)) %>%
  mutate(income_level = factor(ifelse(income_level >=5 , 5 , income_level))) %>%
  select(-median_income)


plot(housing_new$income_level , main="bar plot of income level" , xlab = NA)

train_str_id <-createDataPartition(housing_new$income_level,p=.8 ,
                                   list = FALSE , times = 1)
train_str <-housing_new[train_str_id ,]
test_str <-housing_new[-train_str_id ,]

#test to see if we achieve strtified sampling
table(housing_new$income_level) / nrow(housing_new)
table(train_str$income_level) / nrow(housing_new)

#compare performance of 2 sampling method
overall<-as.vector(table(housing_new$income_level) / nrow(housing_new))
normal_sampling <-factor(sapply(ceiling(test_set$median_income/2),
                                function(value)ifelse(value >=5,5,value)))
normal_sampling<-as.vector(table(normal_sampling) / length(normal_sampling))
str_sampling<-as.vector(table(test_str$income_level) / nrow(test_str))
compare<-data.frame(overall , str_sampling , normal_sampling) %>% 
  mutate(rend_error = 100*normal_sampling/overall - 100) %>% 
  mutate(strat_error = 100*str_sampling/overall -100)
compare

# fit models

#linear model
model_lm <-lm(median_house_value~.,train_str)
summary(model_lm)
predict_lm_train <-predict(model_lm , train_str)
sqrt(mean((train_str$median_house_value - predict_lm_train)^2))#RMSE

#decision tree
model_decision_tree <-rpart(median_house_value~.,
                            data = train_str , 
                            method = "anova",
                            control = rpart.control(minsplit = 2,
                                                    cp=0.001))
predict_decision_tree <-predict(model_decision_tree , train_str)
sqrt(mean((train_str$median_house_value - predict_decision_tree)^2))#RMSE

#SVM
model_svm <- svm(median_house_value~.,
                 data = train_str, cost = 10)


predict_svm <- predict(model_svm , train_str)
sqrt(mean((train_str$median_house_value - predict_svm)^2))#RMSE


#10 FOLD cross validation
housing_copy<-housing[sample(nrow(housing)),]#randomly shuffle data 

folds<- cut(seq(1,nrow(housing_copy)) , breaks = 10 , labels = FALSE)


#perform 10 fold validation
mse_lm<- 0 
mse_tree<-0
mse_svm<-0


for (i in 1:10){
  #segment data by using which function
  testindex<-which(folds==i,arr.ind = TRUE)
  testdata<-housing_copy[testindex , ]
  traindata<-housing_copy[-testindex,]
  
  #fit the model
  lm_model<-lm(median_house_value~.,traindata)
  tree_model<-rpart(median_house_value~., data=traindata,
                    method="anova" , 
                    control = rpart.control(minsplit = 2 , cp=0.0001))
  svm_model<-svm(median_house_value~., data=traindata , cost=10)
  
  #make predictions
  predict1<-predict(lm_model , testdata)
  predict2<-predict(tree_model , testdata)
  predict3<-predict(svm_model , testdata)
  
  #update mse
  mse_lm<-mse_lm+sum(folds==i)/nrow(housing_copy)*mean((predict1 - testdata$median_house_value)^2)
  mse_tree<-mse_tree+sum(folds==i)/nrow(housing_copy)*mean((predict2 - testdata$median_house_value)^2)
  mse_svm<-mse_svm+sum(folds==i)/nrow(housing_copy)*mean((predict3 - testdata$median_house_value)^2)
  
}

sqrt(mse_lm)
sqrt(mse_tree)
sqrt(mse_svm)

#tuning parameters

#decision tree
tune_tree <- tune.rpart(median_house_value~.,
                        data = train_str , 
                        minsplit = c(5,10,15,20) ,
                        cp=c(0.1,0.001,0.0001))
summary(tune_tree)
plot(tune_tree)


best_tree <- tune_tree$best.model
predict_tree <-predict(best_tree , train_str)
sqrt(mean((train_str$median_house_value - predict_tree)^2))

#svm
tune_svm <- tune.svm(median_house_value~.,
                     data= train_str ,
                     cost = 10^(-1:2) , gamma=c(0.1 ,0 ,1))
summary(tune_svm)
plot(tune_svm)
best_svm <- tune_svm$best.model
predict_svm <- predict (best_svm, train_str)
sqrt(mean((train_str$median_house_value - predict_svm)^2))
#applying on test data
predict_tree_final <- predict(best_tree, test_str)
sqrt(mean((test_str$median_house_value - predict_tree_final)^2))


predict_svm_final <- predict(best_svm, test_str)
sqrt(mean((test_str$median_house_value - predict_svm_final)^2))



