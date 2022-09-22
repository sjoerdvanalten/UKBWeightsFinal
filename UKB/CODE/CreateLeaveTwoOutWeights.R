
#Goal: estimate probit type weights (as in UKBFirstStageProbit), using random forest.
rm(list = ls())
options(bitmapType="cairo")
library(plyr)
library(tidyverse)
library(glmnet)
library(bigmemory)
library(biglasso)
library(Matrix)
library(pROC)
library(vip)
library(dplyr)
library(doParallel)
library(ggplot2)
library(stargazer)
library(nleqslv)

set.seed(2435)

outcomes<-readRDS("../TEMP/OutcomesLeaveOneOut.rds")
FullData<-read.table("../TEMP/UKBandCensusPriorToTestingImputed.csv",header=TRUE,sep=",")
TestAssign <- read.table("../TEMP/UKBandCensusPriorToTesting.csv",header=TRUE,sep=",")

UKBWeights<-read.table("../OUTPUT/UKBWeightsKFolded.csv",header=TRUE,sep=",")
UKBWeights<-UKBWeights[c("f.eid","LassoWeight")]
names(UKBWeights)<-c("f.eid","LassoWeightOriginal")
#make 5 folds to assign to test sample
FullData$Test<-TestAssign$Test

table(FullData$Test)

FullData$region<-as.factor(FullData$region)
FullData$birthyear<-as.factor(FullData$birthyear)
FullData$sex<-as.factor(FullData$sex)
FullData$carsnoc<-as.factor(FullData$carsnoc)
FullData$HealthSelfReport<-as.factor(FullData$HealthSelfReport)
FullData$Empstat<-as.factor(FullData$Empstat)
FullData$Tenure<-as.factor(FullData$Tenure)
FullData$Education<-as.factor(FullData$Education)
FullData$Ethnicity<-as.factor(FullData$Ethnicity)

TempData<-FullData[c("f.eid","ukb_selected","region","birthyear","sex","carsnoc","Education","HealthSelfReport","SingleHousehold","Ethnicity",
                     "Empstat","Tenure","popweight","Test")]

#TempData<-sample_n(TempData,size=20000)

rm(FullData)

NROW(TempData)
TempData<-TempData[!is.na(TempData$"ukb_selected"), ]
NROW(TempData)
TempData<-TempData[!is.na(TempData$"region"), ]
NROW(TempData)
TempData<-TempData[!is.na(TempData$"birthyear"), ]
NROW(TempData)
TempData<-TempData[!is.na(TempData$"sex"), ]
NROW(TempData)
TempData<-TempData[!is.na(TempData$"HealthSelfReport"), ]
NROW(TempData)
TempData<-TempData[!is.na(TempData$"Empstat"), ]
NROW(TempData)
TempData<-TempData[!is.na(TempData$"Tenure"), ]
NROW(TempData)
TempData<-TempData[!is.na(TempData$"Education"), ]
NROW(TempData)
TempData<-TempData[!is.na(TempData$"carsnoc"), ]
NROW(TempData)
NROW(TempData)
TempData<-TempData[!is.na(TempData$"SingleHousehold"), ]
TempData<-TempData[!is.na(TempData$"Ethnicity"), ]

ProbitNum<-glm(formula = ukb_selected ~ 1, family = binomial(link = "probit") ,weights = popweight,
               data=TempData)
WeightNum<-mean(ProbitNum$fitted.values[TempData$ukb_selected==1])
print(WeightNum)

vars<-c("region","birthyear","sex","carsnoc","Empstat","Tenure","SingleHousehold","Ethnicity","HealthSelfReport","Education")

#for (K in c(1:5)){
 K<-1 
  for (i in 20:nrow(outcomes)){
    
    outcome<-outcomes[i,"orgvar"]
    input <- outcomes[i,"orgvarInput"]
    varsTemp<-vars[vars!=outcome&vars!=input]
    
    f <- as.formula(paste("ukb_selected~(",paste(varsTemp,collapse="+"),")^2"))
    print(f)
    
    w_train<-TempData$popweight[TempData$Test!=K]
    w_test<-TempData$popweight[TempData$Test==K]
    
    y_train<-TempData$ukb_selected[TempData$Test!=K]
    y_test<-TempData$ukb_selected[TempData$Test==K]
    
    select<-c("ukb_selected",varsTemp)
    x_train<-TempData[select][TempData$Test!=K,]
    x_test<-TempData[select][TempData$Test==K,]
    x_full<-TempData[select]
    
    
    x_train <- model.matrix(f, x_train)[,-1]
    x_test <- model.matrix(f, x_test)[,-1]
    x_full <- model.matrix(f, x_full)[,-1]
    
    
    x_trainSmall<-Matrix(x_train, sparse=TRUE)
    x_testSmall<-Matrix(x_test, sparse=TRUE)
    
    print(NCOL(x_trainSmall))
    
    print(paste0("length of train data",NROW(x_trainSmall)))
    print(paste0("length of test data",NROW(x_testSmall)))
    
    Ncores<-38
    print(paste("the number of cores used in CVlasso estmiation is ",Ncores))
    
    registerDoParallel(Ncores)
    
    CVLassoProbit<-cv.glmnet(x_trainSmall, y_train, family = binomial(link = "probit"),alpha=1,weights=w_train, trace.it=1, nfolds=5)
    saveRDS(CVLassoProbit,paste0("../TEMP/CVLassoProbitLeaveTwoOut",outcome,input,K,".rds"))
    
    CVLassoProbit <- readRDS(paste0("../TEMP/CVLassoProbitLeaveTwoOut",outcome,input,K,".rds"))
  
    TempWeightData<-TempData[TempData$Test==K,]
    
    fittedLassoMod <- predict(CVLassoProbit,newx = x_testSmall,type="response",s = "lambda.min")
    fittedLassoMod<-unname(fittedLassoMod)
    #Make leave-one-out weights and save
    TempWeightData$LassoWeight<- WeightNum/fittedLassoMod
    TempWeightData<-TempWeightData[TempWeightData$ukb_selected==1,]
    print(head(TempWeightData))
    #Correlation with original weights here:
    TempWeightData <- join(TempWeightData,UKBWeights, by="f.eid",match="first")
    print(cor.test(TempWeightData$LassoWeight,TempWeightData$LassoWeightOriginal))
    #print(NROW(WeightData$LassoWeight))
    #print(paste("correlation between full weights and leave-one-out for",var,cor(WeightData$LassoWeight,TempWeightData$LassoWeight,use="pairwise.complete.obs")[1]))
    
    TestAssign <- read.table("../TEMP/TestAssign.csv",header=TRUE,sep=",") 
    TempWeightData<-TempWeightData[c("f.eid","LassoWeight")]
    TempWeightData<-join(TempWeightData,TestAssign, by="f.eid",match="first")

    write.csv(TempWeightData[c("f.eid","LassoWeight","Test")], paste0("../TEMP/UKBLassoWeightsLeaveTwoOut",outcome,input,K,".csv"),row.names=FALSE)
    rm(xFeatureMod)
    
    print(paste(outcome,input,"done"))
  }

  
  