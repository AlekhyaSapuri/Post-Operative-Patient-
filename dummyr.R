#libraries used
library(ggplot2)
library(dslabs)
library(dplyr)
library(tidyverse)
library(GGally)
library(plyr)
library(ROCR)

#imported the dummypop.xlsx file
dummypop
colnames(dummypop) <-c("L-CORE", "L_SURF", "L-O2","L-BP","SURF-STBL","CORE-STBL","BP-STBL","COMFORT","ADM-DECS")
is.na(dummypop) # check for NA values

#mean imputation
replace <- round(mean(dummypop$COMFORT,na.rm = TRUE))
dummypop$COMFORT[is.na(dummypop$COMFORT)] <- mean(replace)

View(dummypop$COMFORT)
head(dummypop)
str(dummypop)

#using factor function for changing input and output variables to factor
dummypop$`L-CORE` <-factor(dummypop$`L-CORE`,levels = c(0,1,2),ordered = FALSE)
dummypop$L_SURF <-factor(dummypop$L_SURF,levels = c(0,1,2),ordered = FALSE)
dummypop$`L-O2` <-factor(dummypop$`L-O2`,levels = c(0,1),ordered = FALSE)
dummypop$`L-BP` <-factor(dummypop$`L-BP` ,levels = c(0,1,2),ordered = FALSE)
dummypop$`SURF-STBL` <-factor(dummypop$`SURF-STBL`,levels = c(0,1),ordered = FALSE)
dummypop$`CORE-STBL` <-factor(dummypop$`CORE-STBL`,levels = c(0,1,2),ordered = FALSE)
dummypop$`BP-STBL` <-factor(dummypop$`BP-STBL`,levels = c(0,1,2),ordered = FALSE)
dummypop$`ADM-DECS` <-factor(dummypop$`ADM-DECS`,levels = c(0,1),ordered = FALSE)
dummypop$COMFORT <-factor(dummypop$COMFORT,levels = c(0,1),ordered = FALSE)  # (0-10) taken as 0 and (10-20) as 1 (changed in excel)

#logistic regression 
set.seed(1234)
indexset <- sample(2,nrow(dummypop),replace = T,prob = c(0.8,0.2))
train <- dummypop[indexset==1,]
test <- dummypop[indexset==2,]
dim(train)
dim(test)
indexset
class(dummypop$`ADM-DECS`)
mod <- glm(`ADM-DECS` ~ . , data = train, family = binomial())
mod

S = train$`ADM-DECS` == 1
training_predictions <- predict(mod, type = "response")

#compute training error use an outcome cutoff at 0.5
training_error <-sum((training_predictions >= 0.5) != S)/nrow(train)
training_error
1-training_error

#training error for predictions in {0,1}
test_predictions = predict(mod, test, type = "response")
test_predictions
#using a probability cutoff of 0.5 
test_predictions[test_predictions >=0.5] <- 1
test_predictions[ test_predictions != 1] <- 0
test_predictions[is.na(test_predictions)] <- 0
table(test_predictions)
table(test$`ADM-DECS`)

  
#acc
misac <- mean(test_predictions!=test$`ADM-DECS`)
ac= 1-misac
ac

#res
library(ROCR)
pr<-prediction(test_predictions,test$`ADM-DECS`)
prf<- performance(pr,measure = "tpr",x.measure = "fpr")
plot(prf)
auc<- performance(pr,measure = "auc")
auc <-auc@y.values[[1]]
auc


cm <-as.matrix(table(test$`ADM-DECS`,test_predictions))
cm
n = sum(cm)
nc = nrow(cm)
diag = diag(cm)
rowsums = apply(cm,1,sum)
colsums = apply(cm,2,sum)
p = rowsums/n
q = colsums/n
precision = diag/colsums
recall = diag/rowsums
recall
f1 = 2* precision*recall/(precision + recall)
f1
data.frame(precision,recall,f1)




