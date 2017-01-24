trend<-matrix(,nrow = 2051,ncol = 5, byrow = TRUE)
Variant<-NULL
for(i in 2:2052)
{
j<-i+4        
a<-Performance[,i]
Variant[i-1]<-colnames(Performance[i])
trend[i-1,]<-classifier(a)
a<-NULL
}



classifier<-function(a)
{
  count<-1
  final<-matrix(, nrow = 1,ncol = 5, byrow = FALSE)
  for (i in 1:20)
  {
   if(i==1||i%%5==0)
   {first<-a[i:i+1]
   second<-a[i+2:i+3]
   
   difference<-first-second
   signs=sign(difference)
   pos=signs[signs>0]
   neg=signs[signs<0]
   zero=signs[signs==0]
   if(length(pos)>length(neg))
   {final[count]=2
   count=count+1}
   if(length(pos)<length(neg))
   {final[count]=1
   count=count+1}
   if(length(pos)==length(neg))
   {final[count]=0
   count=count+1}
   if(length(zero)==2)
   {final[count]=0
   count=count+1}        
   }
   }
return(final)
}

