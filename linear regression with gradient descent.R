x<-c(1,2,4,3,5)
y<-c(1,3,3,2,5)
alpha<-0.01
B0<-0
B1<-0
p<-c()

for(j in seq_len(4)){
  for(i in seq_len(length(y))){
    p[i]<-B0+B1*x[i]
    error<-p[i]-y[i]
    B0<-B0-alpha*error
    B1<-B1-alpha*error*x[i]
  }
}

  

