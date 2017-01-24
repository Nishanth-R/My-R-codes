library(glmnet)
library(lars)
library(ggplot2)
library(stats)
#Importing the data
Week1<-read.csv("Dry Run.csv", header = TRUE)
Week2<-read.csv("Week2.csv", header = TRUE)

#Doing a bit of feature engineering 
str(Week1)
Week1$Encoded.Values<-as.factor(Week1$Encoded.Values)
Week1$Channel.Name<-as.character(Week1$Channel.Name)
Week1$Channel.Type<-as.factor(Week1$Channel.Type)
Week1$Date.Started<-as.Date(Week1$Date.Started)

#Feature Engineering the data and applying LASSO through LARS package
Week1N<-Week1[,-c(1,2,5,6)]
Week1N<-cbind(Week1N,0)
Week1N<-as.matrix(Week1N)
colnames(Week1N)<-NULL
class(Week1N)<-"numeric"
x<-as.matrix(Week1N[,1:15])
y<-as.matrix(Week1N[,16])
#Assigning some nonsense to Vibrancy Index, please suggest a better formula if you can find one
y=(2-Week1N[,4])/Week1N[,1]+Week1N[,2]+Week1N[,11]+(Week1N[,13]/Week1N[,12])
#First run of LASSO regression
#<Refer ISLR.pdf for the theory behind Lassoing
m1<-lars(x,y,type = "lasso")
#Co-efficients of the first 15 runs are given
coef(m1)
#Cross validating the LASSO model, this will give an ideal Lambda value
#http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
cv<-cv.lars(x,y,plot.it = FALSE, mode = "step")
#Running the LASSO for a second time with improved shrinkage
m1<-glmnet(x,y,type = "lasso",lambda = cv$lambda.min)
#Alternate Approach, read http://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
m2<-glmnet(x,y)
plot(m2)
plot(cv)#SD should be reduced as much as possible
cv$lambda.min #We should be trying to reduce this for a better model
#Selecting the best Model from LASSO 
cv2<-cv.glmnet(x,y)
c<-coef(cv2, s="lambda.1se")
#Model suggests a Beta-0, Posts Generated and Traffic across last week 
inds<-which(c!=0)
variables<-row.names(c)[inds]
#Encode the values of Vibrancy-Week1N[,16] to either 0 or 1 then apply glm 
#fit<-glm(Vibrancy~ Posts Generated+Traffic across last week, data = Week1)


