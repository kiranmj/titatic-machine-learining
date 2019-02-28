rm(list = ls())
setwd("E:\\R.csv file")
datatask<-read.csv("train.csv",header = TRUE,sep = ",")
str(datatask)
summary(datatask)
sum(is.na(datatask))
library(DMwR)
datatask1<-centralImputation(datatask)
str(datatask1)
sum(is.na(datatask1))
datatask1$PassengerId<-NULL
datatask1$Ticket<-NULL
datatask1$Name<-NULL
datatask1$Cabin<-NULL
datatask1$Embarked<-NULL

any(is.na(datatask1))
#split the data into train AND validation 
set.seed(2)
ind<-sample(2,nrow(datatask1), replace = TRUE, prob = c(0.7,0.3))
set.seed(453)
train1<-datatask1[ind == 1,]
validation1<- datatask1[ind == 2,]




log_reg <- glm(Survived ~., data = train1, family = "binomial")

# Get the summary of the model and understand the output
summary(log_reg)





#newp <- predict(log_reg,t ,type = 'response')
prob_train <- predict(log_reg,train1, type = "response")


pred_class <- ifelse(prob_train> 0.5, "0", "1")
train1table<-table(train1$Survived,pred_class)
accuracy1 <- sum(diag(train1table))/sum(train1table)

print(accuracy1)
str(train1)
str(validation1)

prob_valid <- predict(log_reg,validation1, type = "response")
pred_valid <- ifelse(prob_valid> 0.5, "0", "1")

validtable<-table(validation1$Survived,pred_valid)
accuracy2 <- sum(diag(validtable))/sum(validtable)

print(accuracy2)


#give test
datatest<-read.csv("test.csv",header = TRUE,sep = ",")
str(datatest)

testg<-datatest[,-c(1,3,8,10,11)]
str(testg)
prob_test <- predict(log_reg,testg, type = "response")
pred_test <- ifelse(prob_test> 0.5, "0", "1")
pred_test

sample1<-data.frame(datatest)
sample1$Survived<-pred_test
View(sample1)

sample1<-sample1[,-c(2:8)]
View(sample1)

write.csv(sample1,"saikiran_sample.csv",row.names = FALSE )
