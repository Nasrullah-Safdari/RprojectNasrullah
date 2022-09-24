#Nasrullah Safdari
##FDT_Assignment
datasetIris= read.csv(url('https://gist.githubusercontent.com/curran/a08a1080b88344b0c8a7/raw/0e7a9b0a5d22642a06d3d5b9bcbad9890c8ee534/iris.csv'))
head(datasetIris)
View(datasetIris)
summary(datasetIris$petal_length)

###blank vector 
numaricIris=c()
####Transforming the Numerical variable into Categorical var using Selection
 for(n in 1:length(datasetIris$petal_length)) {
   if(datasetIris$petal_length[n]<3) {
     numaricIris[n]= "Short Petal Length"
   } else if(datasetIris$petal_length[n]>=3 & datasetIris$petal_length[n]<5){
     numaricIris[n]= "Meduim Petal Length"
   } else{
     numaricIris[n]="Long Petal Length"
   }
 }

petalLength = cbind(datasetIris$petal_length, numaricIris)
View(petalLength)
#######FDT For Numerical Variable(petal_length)#######
FDT_Num=function(x){
  abs_freq= table(x)
  rel_freq= prop.table(abs_freq)
  cum_freq= cumsum(rel_freq)
  FDT= cbind(abs_freq, rel_freq, cum_freq)
  return(FDT)
}
FDT_Num(numaricIris)
