setwd("C:/Users/andrei.deusteanu/Desktop/Master ML")
path<-"C:/Users/andrei.deusteanu/Desktop/Master ML"
fileRead=paste(path,"spreadsheets","06-LinearDiscriminantAnalysis.xlsx",sep="/")
require(xlsx)

Data<-read.xlsx(file=fileRead,sheetName = "Sheet2",startRow = 4,endRow = 44)
Data<-data.table(Data)

#calculate mean for each class
meanX_Y0<-mean(Data[Y==0,X])
meanX_Y1<-mean(Data[Y==1,X])

#calculate class probabilities
p_Y0<-nrow(Data[Y==0])/nrow(Data)
p_Y1<-nrow(Data[Y==1])/nrow(Data)

#calculate the sum of squared differences for each class
sqDiff_Y0<-sum(Data[Y==0,(X-meanX_Y0)^2])
sqDiff_Y1<-sum(Data[Y==1,(X-meanX_Y1)^2])

#calculate the variance
varData<-(sqDiff_Y0+sqDiff_Y1)/(nrow(Data)-length(unique(Data$Y)))

#make predictions using the equation
discrim<-function (x){
  discrim0<-x*meanX_Y0/varData-(meanX_Y0^2)/(2*varData)+log(p_Y0,base=exp(1))
  discrim1<-x*meanX_Y1/varData-(meanX_Y1^2)/(2*varData)+log(p_Y1,base=exp(1))
  if(discrim0>=discrim1){
    0
  }else{
    1
  }
}
#mapply applies the function to each element of the argument
Data<-Data[,discrimPrediction:=mapply(discrim,X)]
