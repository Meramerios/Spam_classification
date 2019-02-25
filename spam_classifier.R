## Purpose: Spam classification using K nearest neighbour
# MOdel :Knearest neighbour.
# written by: Merhawi
# merastat@gmail.com

dataspam<-read.csv("spambas.csv",sep = ",",header = TRUE)

n=dim(dataspam)[1] # n is number of emails
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=dataspam[id,]
test=dataspam[-id,]

knearest=function(data,k,newdata) { ## KNN from scratch
  
  n1=dim(data)[1]
  n2=dim(newdata)[1]
  p=dim(data)[2]
  Prob=numeric(n2)
  target<-data[,p]
  X=as.matrix(data[,-p])
  Xn=as.matrix(newdata[,-p])
  X=X/matrix(sqrt(rowSums(X^2)), nrow=n1, ncol=p-1)
  Xn=Xn/matrix(sqrt(rowSums(Xn^2)), nrow=n1, ncol=p-1)
  C<-abs(X%*%t(Xn))
  D<-1-C
  
  nearest<-as.matrix(apply(D,2,order))
  if(k==1)
  { 
    target1<-as.matrix(nearest[1,])
    for (i in 1:n2 )
    {
      Prob[i]<-mean(target[target1[i]])
    }
    return(Prob)
  }
  else
  {
    target1<-as.matrix(nearest[1:k,])
    for (i in 1:n2 )
    {
      
      Prob[i]<-mean(target[target1[,i]])
      
    }
    return(Prob)
  }
}

class_categorize<-function(x,y)
{
  pre_class<-ifelse(x>=y,1,0)
  return(pre_class)
}

knearest_k5_prob<-knearest(train,5,train) # when K is 5
knearest_k5_class<-class_categorize(knearest_k5_prob,0.5)
confusion_matrix<-table(train[,49],knearest_k5_class)
confusion_matrix

knearest_k1_prob<-knearest(data=train,k=1,newdata=train) # when K is 1
knearest_k1_class<-class_categorize(knearest_k1_prob,0.5)
confusion_matrix1<-table(train[,49],knearest_k1_class)
confusion_matrix1
