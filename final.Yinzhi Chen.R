setwd('/Users/evangelinechen/Desktop')
rm(list=ls())
data = read.csv('versionA.csv')
RNGversion(vstr = '3.6.1')
library(ggplot2)

#2
set.seed(617)
split = sample(1:nrow(data), nrow(data)*0.75)
train = data[split,]
test = data[-split,] 
nrow(train)+nrow(test)==nrow(data)
nrow(train)

#3
str(data)
str(data$y)

#4
meanprice4 = dplyr::filter(train, carat>1,cut=="Ideal")
mean(meanprice4$price)

#5
ggplot(data = train, aes(x = carat, y = price))+
  geom_point()+
  geom_smooth(method='lm')

#6
cor(train$table,train$y)

#7
model1 = lm(price~y,data = train)
summary(model1)$r.squared

#8
summary(model1)

#9
model2 <- lm(price~cut,data=train)
summary(model2)

#10
model3 = lm(price~.-price_hilo-x-y-z,data=train)
summary(model3)$r.squared

#11
summary(model3)

#12
pred3=predict(model3,newdata=train)
tail(pred3,1)
tail(pred3)
head(pred3)

#13
pred3.1 = predict(model3,newdata = test)
rmse3 = sqrt(mean((pred3.1-test$price)^2))
rmse3

#14
library(caret)
library(lm.beta)
library(ggplot2)
library(mice)
library(car)
library(caTools)
library(ROCR)
library(leaps)
library(rpart)
library(rpart.plot)
library(randomForest)
library(e1071)
model4 = lm(price~carat+depth+table+x+y+z,data = train)
vif(model4)

#15
start_mod = lm(price~1,data=train)
empty_mod = lm(price~1,data=train)
full_mod = lm(price~carat+depth+table+x+y+z, data = train)
hybrid_model4 = step(start_mod,
                   scope=list(lower=empty_mod, upper=full_mod), direction='both')
summary(hybrid_model4)


#16
model5 = glm(price_hilo~carat+cut+color+clarity+depth+table,data=train,family = "binomial")
summary(model5)
summary(model5)$aic


#17
summary(model5)
coefficients(model5)[3]
percent_increase = 100* (exp(coefficients(model5)[3])-1)
percent_increase

#18
pred5 = predict(model5, newdata = test, type = "response")
ct5 = table(test$price_hilo,pred5>0.4)
(accuracy5 = sum(ct5[1,1],ct5[2,2])/nrow(test))


#19
model6=rpart(price_hilo~carat+cut+color+clarity+depth+table,
             data=train,method='class')
summary(model6)

#20

library(rpart.plot)
rpart.plot(model6)


#21
rpart.plot(model6)

#22
set.seed(617)
model7 = randomForest(factor(price_hilo)~carat+cut+color+clarity+depth+table,
                      data =train,ntree=100)
summary(model7)
importance(model7)[order(importance(model7), decreasing = T),]


#23
pred7 = predict(model7,newdata=test, type = "prob")
ROCRpred7 = prediction(pred7[,2],test$price_hilo)
as.numeric(performance(ROCRpred7,"auc")@y.values)


#24
model8 = svm(factor(price_hilo)~carat+cut+color+clarity+depth+table, 
             data = train, kernel='radial',type='C-classification')
pred8 = predict(model8, data = train)
table(actual = train$price_hilo, predicted8 = pred8)


#25
pred8test = predict(model8, newdata = test)
ct8 = table(test$price_hilo, pred8test)
(accuracy8 = sum(ct8[1,1],ct8[2,2])/nrow(test))





