#Data Manipulation and Clenasing

variant_1163<-subset(FMCG_data,Variant=='variant-1163',select = c(Jan.12.R:Dec.13.R))
variant_1163$Sum_rev<-rowSums(variant_1163)
Revenue_sum<-variant_1163$Sum_rev
Revenue_sum<-as.data.frame(Revenue_sum)
Revenue_sum<-t(Revenue_sum)
colnames(Revenue_sum)<-c('Sum of Revenue')
Summary(Revenue_sum)
variant_734<-subset(FMCG_data,Variant=='variant-734',select = c(Jan.12.R:Dec.13.R))
variant_734$Sum_rev<-rowSums(variant_734)
Revenue_sum1<-variant_734$Sum_rev
Revenue_sum1<-as.data.frame(Revenue_sum1)
Revenue_sum1<-t(Revenue_sum1)
lmdata<-cbind(brand-311;variant-734,Revenue_sum,brand.434..variant.1163,Revenue_sum1)
View(lmdata)

#Linear Model for Variant 1163
lin1<-lm(Var_1163_Rev_Sum~Media_Spends_1163,data = lmdata)
summary(lin1)
#R Squared Value =21%
#Exploratory Analysis
p1<-ggplot(lmdata,aes(x=Var_1163_Rev_Sum,y=Media_Spends_1163))+geom_point(colour='blue')+geom_smooth(colour='blue',fill='grey',size=0.2)+geom_abline(data = lin1,colour='red')
plot(p1)

lin2<-lm(Var_734_Rev_Sum~Media_Spends_734,data = lmdata)
summary(lin2)
#R-Squared Value= 0.5% 
#Exploratory Anaysis
p2<-ggplot(lmdata,aes(x=Var_734_Rev_Sum,y=Media_Spends_734))+geom_point(colour='blue')+geom_smooth(colour='blue',fill='grey')+geom_abline(data=lin2,colour='red')
plot(p2)

#Further Analysis 
anova(lin1)
anova(lin2)


#Standard error
se1<-sqrt(diag(vcov(lin1)))
se2<-sqrt(diag(vcov(lin2)))
