library(glmnet)
library(C50)
library(pROC)
library(ggplot2)

##Reading and summarising the data
Data_In<-read.csv('Retail_Lending.csv', header = TRUE)
summary(Data_In)
str(Data_In)

##Feature Engineering to bring variables back to native format
#Bringing all dates to an Easy to digest format
Data_In$end_effective_date<-as.Date(strptime(Data_In$end_effective_date,'%d-%b-%y'))
Data_In$maturity_date<-as.Date(strptime(Data_In$maturity_date,'%d-%b-%y'))
Data_In$NEXT_DUE_DATE<-as.Date(strptime(Data_In$NEXT_DUE_DATE,'%d-%b-%y'))
Data_In$pmt_next_due_date<-as.Date(strptime(Data_In$pmt_next_due_date,'%d-%b-%y'))
#Re-Converting back factors to native formats
Data_In$hpi_uCLTV_since_orig2<-as.numeric(Data_In$hpi_uCLTV_since_orig2)
Data_In$Upd.Shaw.Fico<-as.numeric(Data_In$Upd.Shaw.Fico)
Data_In$DTI<-as.numeric(Data_In$DTI)
Data_In$tr_1_to_2<-as.factor(Data_In$tr_1_to_2)
#Splitting the data into train and test datasets
smp_size <- floor(0.60 * nrow(Data_In))
set.seed(123)
train_ind <- sample(seq_len(nrow(Data_In)), size = smp_size)
train <- Data_In[train_ind, ]
test <- Data_In[-train_ind, ]

##Building a LASSO model to identify the most significant Variables
Num_Mat<-Data_In[-c(1,2,4,7,8,10,11,16,17,20)]
str(Num_Mat)
Num_Mat<-as.matrix(Num_Mat)
x<-as.matrix(Num_Mat[,1:11])
y<-as.matrix(Num_Mat[,12])
set.seed(123)
lasso<-cv.glmnet(x,y)
c<-coef(lasso, s="lambda.1se")
inds<-which(c!=0)
variables<-row.names(c)[inds]
#The lasso model shows that the feild nx_status is highly propotional to tr_1_to_2 and looking at 
#data there seems to be a high amount of correlation between the two feidls

#Looking through a C50 decison tree to get better insights
Tree_model<-C5.0(train[-c(2,4,7,16,19,20,22)],train$tr_1_to_2)
summary(Tree_model)

#More desperation
attach(train)
fit1<-glm(train$tr_1_to_2~train$nx_status+train$status, family = 'binomial', data = train )
fit2<-glm(train$tr_1_to_2~end_effective_date+Curr.Balance+maturity_date+hpi_uCLTV_since_orig2+PAYMENT_AMOUNT+NEXT_DUE_DATE+OPEN_OR_CLOSED+Upd.Shaw.Fico+no_of_pmts_rcd+num_of_missed_pmts+Delinquency+months_to_maturity+mly_unemp, family = 'binomial', data = train)
summary(fit1)
summary(fit2)
preds1<-predict.glm(fit1)
preds2<-predict.glm(fit2)
roc1<-roc(train$tr_1_to_2~preds1)
roc2<-roc(train$tr_1_to_2~preds2)
plot(roc1)
plot(roc2,add=TRUE,col='Red')

fit3<-glm(tr_1_to_2~end_effective_date+Curr.Balance+maturity_date+hpi_uCLTV_since_orig2+PAYMENT_AMOUNT+NEXT_DUE_DATE+OPEN_OR_CLOSED+Upd.Shaw.Fico+no_of_pmts_rcd+num_of_missed_pmts+Delinquency+months_to_maturity+mly_unemp+DTI+hpi_life_change, family = 'binomial', data = Data_In)
preds3<-predict.glm(fit3)
roc3<-roc(Data_In$tr_1_to_2~preds3)
plot(roc3,add=TRUE,col='Blue')

#Exploratory Analysis
p1<-ggplot(data = train,aes(y=mean(Curr.Balance),x=tr_1_to_2))+geom_bar()
plot(p1)
