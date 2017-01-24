library(lars)
library(glmnet)
library(ggplot2)
library(pROC)
#Importing the data 
Credit_data<-read.csv('germancredit.csv', header = TRUE)
View(Credit_data)
str(Credit_data)
Credit_data$Default<-as.factor(Credit_data$Default)
#Identifying the most important factors
#Beginning with some feature transformation
CDN<-data.matrix(Credit_data, rownames.force = TRUE)
x<-CDN[,2:21]
y<-CDN[,1]
x<-as.matrix(x)
y<-as.matrix(y)
#LASSO functions accept only matrix input, hence after doing the required
#transformations doing a LASSO regression to identify the factors with the
#best co-relation excluding multi-colinearity
model1<-lars(x,y,type = 'lasso')
#Verifying the LASSO model
cv<-cv.lars(x,y,plot.it = TRUE,type = "step")
cv$cv.error
cv2<-cv.glmnet(x,y)
cv2$lambda.min
cv2$lambda.1se
c<-coef(cv2, s="lambda.1se")
c2<-coef(cv2, s='lambda.min')
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables
#After identifying the most important variables time to compare with other techniques
attach(Credit_data)
log1<-glm(Default~.,family = 'binomial' ,data = Credit_data)
log2<-glm(Default~checkingstatus1+duration+history+amount+savings+employ+installment+status+others+property+otherplans+foreign, family = 'binomial' ,data = Credit_data)
preds1<-predict.glm(log1)
preds2<-predict.glm(log2)
roc1<-roc(Default ~ preds1)
roc2<-roc(Default ~ preds2)
plot(roc1)
plot(roc2, add=TRUE, col='red')
roc1$auc
roc2$auc
