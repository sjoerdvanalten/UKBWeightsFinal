
library(future.apply)
library(dplyr)
library(data.table)
plan(multisession) ## => parallelize on your local computer
  
set.seed(85454)
  
FullData<-fread("../TEMP/UKBandCensusPriorToTesting.csv",header=TRUE,sep=",")
FullData<-as.data.frame(FullData)

FullData$Empstat[FullData$Empstat==7]<-6

FullData<-FullData[c("f.eid","ukb_selected","Education","region","birthyear","sex","carsnoc","HealthSelfReport",
                     "Empstat","Tenure","SingleHousehold","popweight","Ethnicity")]


FullData$Education<-as.factor(FullData$Education)

NROW(FullData[rowSums(is.na(FullData))>0&FullData$"ukb_selected"==1,])
NROW(FullData[rowSums(is.na(FullData))>0&FullData$"ukb_selected"==0,])


#Impute missing variables (carsnoc,HealthSelfReport,Empstat,Tenure) 
#by exact matching. (Randomly draw if there are multiple individuals who have the same values for available variables)
ImputedData<-FullData

ImputeMissingCol<-function(var,NAData,OutData,EvalData,vImputeVars){

for (i in 1:length(vImputeVars)){
  vImputeVars[i]
}  
  
ExactNeighbors<-future_lapply(rownames(EvalData[is.na(EvalData[,var]), ]), function(i){
  inner_join(NAData[,c(var,vImputeVars)],NAData[i,c(vImputeVars)],by=vImputeVars)[,var]
  })
head(ExactNeighbors)
names(ExactNeighbors)<-rownames(EvalData[is.na(EvalData[,var]), ])
ExactNeighbors<-lapply(ExactNeighbors, function(x) x[!is.na(x)])
j<-1

for (i in rownames(EvalData[is.na(EvalData[,var]), ])){
  if (length(ExactNeighbors[[j]])<=0){
    print(paste("Exact matching of missing variable",var,"not possible for row index",i))
  }else{
    OutData[i,var]<-sample(ExactNeighbors[[j]],size=1)
  }
  j<-j+1
}
return(OutData)
}


print(paste("imputing HealtSelfReport try 1 of 2"))
ImputedData<-ImputeMissingCol("HealthSelfReport",FullData,ImputedData,FullData,c("region","birthyear","sex","Empstat","ukb_selected","Education","Ethnicity"))
print(paste("imputing HealthSelfReport try 2 of 2"))
ImputedData<-ImputeMissingCol("HealthSelfReport",FullData,ImputedData,ImputedData,c("birthyear","sex","Empstat","ukb_selected","Education"))

print(paste("imputing Empstat try 1 of 2"))
ImputedData<-ImputeMissingCol("Empstat",FullData,ImputedData,FullData,c("region","birthyear","sex","HealthSelfReport","ukb_selected","Education","Ethnicity"))
print(paste("imputing Empstat try 2 of 2"))
ImputedData<-ImputeMissingCol("Empstat",FullData,ImputedData,ImputedData,c("birthyear","sex","HealthSelfReport","ukb_selected","Education"))

print(paste("imputing Education, try 1 of 2"))
ImputedData<-ImputeMissingCol("Education",FullData,ImputedData,FullData,c("region","birthyear","sex","HealthSelfReport","Empstat","ukb_selected","Ethnicity"))
print(paste("imputing Education, try 2 of 2"))
ImputedData<-ImputeMissingCol("Education",FullData,ImputedData,ImputedData,c("birthyear","sex","HealthSelfReport","Empstat","ukb_selected"))

print(paste("imputing SingleHousehold, try 1 of 2"))
ImputedData<-ImputeMissingCol("SingleHousehold",FullData,ImputedData,FullData,c("region","birthyear","sex","HealthSelfReport","Empstat","ukb_selected","Education","Ethnicity"))
print(paste("imputing SingleHousehold, try 2 of 2"))
ImputedData<-ImputeMissingCol("SingleHousehold",FullData,ImputedData,ImputedData,c("birthyear","sex","HealthSelfReport","Empstat","ukb_selected","Education"))

print(paste("imputing carsnoc, try 1 of 2"))
ImputedData<-ImputeMissingCol("carsnoc",FullData,ImputedData,FullData,c("region","birthyear","sex","HealthSelfReport","Empstat","ukb_selected","Education","Ethnicity"))
print(paste("imputing carsnoc, try 2 of 2"))
ImputedData<-ImputeMissingCol("carsnoc",FullData,ImputedData,ImputedData,c("birthyear","sex","HealthSelfReport","Empstat","ukb_selected","Education"))

print(paste("imputing Tenure try 1 of 2"))
ImputedData<-ImputeMissingCol("Tenure",FullData,ImputedData,FullData,c("region","birthyear","sex","Empstat","HealthSelfReport","ukb_selected","Education","Ethnicity"))
print(paste("imputing Tenure try 2 of 2"))
ImputedData<-ImputeMissingCol("Tenure",FullData,ImputedData,ImputedData,c("birthyear","sex","Empstat","HealthSelfReport","ukb_selected","Education"))

summary(FullData)
summary(ImputedData)
print(summary(FullData))
print(summary(ImputedData))

#ImputedData$Empstat[ImputedData$Empstat==6]<-7
print("before imputing")
print("UK Census missing observations:")
print(NROW(which(is.na(FullData[FullData$ukb_selected==0,]))))
print("UKB missing observations:")
print(NROW(which(is.na(FullData[FullData$ukb_selected==1,]))))

print("after imputing")
print("UK Census missing observations:")
print(NROW(which(is.na(ImputedData[ImputedData$ukb_selected==0,]))))
print("UKB missing observations:")
print(NROW(which(is.na(ImputedData[ImputedData$ukb_selected==1,]))))

write.csv(ImputedData, "../TEMP/UKBandCensusPriorToTestingImputed.csv",row.names=FALSE)

