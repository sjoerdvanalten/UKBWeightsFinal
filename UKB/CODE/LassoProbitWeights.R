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

imv<-function(y,p1,p2) {
  ##
  ll<-function(x,p) {
    z<-log(p)*x+log(1-p)*(1-x)
    z<-sum(z)/length(x)
    exp(z)
  }    
  loglik1<-ll(y,p1)
  loglik2<-ll(y,p2)
  print(loglik1)
  print(loglik2)
  getcoins<-function(a) {
    f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
    nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
  }
  c1<-getcoins(loglik1)
  c2<-getcoins(loglik2)
  ew<-function(p1,p0) (p1-p0)/p0
  imv<-ew(c2,c1)
  imv
}

set.seed(8790)

FullData<-read.table("../TEMP/UKBandCensusPriorToTestingImputed.csv",header=TRUE,sep=",")
TestAssign <- read.table("../TEMP/UKBandCensusPriorToTesting.csv",header=TRUE,sep=",")

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

vars<-c("region","birthyear","sex","carsnoc","Empstat","Tenure","SingleHousehold","Ethnicity","HealthSelfReport","Education")

varlabs<-c("Region","Year of Birth","Sex","No. of cars","Employment","Tenure of dwelling","One-person household","Ethnicity","Health (self-reported)","Education")


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

for (K in c(1:5)){

  w_train<-TempData$popweight[TempData$Test!=K]
  w_test<-TempData$popweight[TempData$Test==K]

  y_train<-TempData$ukb_selected[TempData$Test!=K]
  y_test<-TempData$ukb_selected[TempData$Test==K]

  select<-c("ukb_selected",vars)
  x_train<-TempData[select][TempData$Test!=K,]
  x_test<-TempData[select][TempData$Test==K,]
  x_full<-TempData[select]
  
  f <- as.formula(paste("ukb_selected~(",paste(vars,collapse="+"),")^2"))
  print(f)

  # Second step: using model.matrix to take advantage of f
  print(NROW(x_train))
  print(summary(x_train))
  
  x_train <- model.matrix(f, x_train)[,-1]
  x_test <- model.matrix(f, x_test)[,-1]
  x_full <- model.matrix(f, x_full)[,-1]
  
  #print(colnames(x_train))
  #print(colnames(x_test))
  print(object.size(x_train),units="auto")
  print(object.size(x_test),units="auto")
  
  x_trainSmall<-Matrix(x_train, sparse=TRUE)
  print(object.size(x_trainSmall),units="auto")
  x_testSmall<-Matrix(x_test, sparse=TRUE)
  print(object.size(x_testSmall),units="auto")

  print(paste0("length of train data",NROW(x_trainSmall)))
  print(paste0("length of test data",NROW(x_testSmall)))
  
  rm(x_train)
  rm(x_test)
  rm(x_full)
  
  print(NCOL(x_trainSmall))
  
  #LassoProbit<-glmnet(x_trainSmall, y_train, family = binomial(link = "probit"),alpha=1,weights=w_train)
  
  #coef(LassoProbit)[,LassoProbit$dim[2]]
  #NROW(coef(LassoProbit)[,LassoProbit$dim[2]])
  #NROW(which(coef(LassoProbit)[,LassoProbit$dim[2]]!=0))
  #which(coef(LassoProbit)[,LassoProbit$dim[2]]==0)
  
  #saveRDS(LassoProbit,"../TEMP/LassoProbit.rds")
  
  set.seed(8791)
  
  Ncores<-38
  print(paste("the number of cores used in CVlasso estmiation is ",Ncores))
  
  registerDoParallel(Ncores)
  
  #Re-estimate the model using cross-validation:
  CVLassoProbit<-cv.glmnet(x_trainSmall, y_train, family = binomial(link = "probit"),alpha=1,weights=w_train, trace.it=1, nfolds=5)
  saveRDS(CVLassoProbit,paste0("../TEMP/CVLassoProbitVarSet",K,".rds"))
  
  CVLassoProbit <- readRDS(paste0("../TEMP/CVLassoProbitVarSet",K,".rds"))
  #Check number of zero coefficients:
  coef<-CVLassoProbit$glmnet.fit$beta[,which(CVLassoProbit$lambda==CVLassoProbit$lambda.min)]
  print(paste("for fold",K,sum(coef==0),"coefficients out of",NROW(coef),"were shrunk to zero at minimum lambda",CVLassoProbit$lambda.min))
  
  
  png(paste0("../OUTPUT/FIGURES/CVLassoProbitFit",K,".png"))
  plot(CVLassoProbit)
  dev.off()
  
  print(paste("lambda for fold",K,CVLassoProbit$lambda.min))
  
  #Sumamrize the glmnet path at each step
  
  fittedLasso_train <- predict(CVLassoProbit,newx = x_trainSmall,type="response",s = "lambda.min")
  fittedLasso_test <- predict(CVLassoProbit,newx = x_testSmall,type="response",s = "lambda.min")
  
  roc<- roc(y_test,fittedLasso_test)
  CVLassoProbit$AUCTest<-round(roc$auc,digits=3)
  #print(CVLassoProbit$AUCTest)
  
  roc<- roc(y_train,fittedLasso_train)
  CVLassoProbit$AUCTrain<-round(roc$auc,digits=3)
  #print(CVLassoProbit$AUCTrain)
  
  #Incorporate weights in the IMV: expxand data according to their weight:
  testpredicted<-predict(CVLassoProbit,newx = x_testSmall,type="response",s = "lambda.min")
  colnames(testpredicted)<-"testpredicted"
  testIMV<-as.data.frame(cbind(testpredicted,y_test,w_test))
  testIMV<-testIMV %>%
    uncount(round(w_test))
  
  trainpredicted<-predict(CVLassoProbit,newx = x_trainSmall,type="response",s = "lambda.min")
  colnames(trainpredicted)<-"trainpredicted"
  trainIMV<-as.data.frame(cbind(trainpredicted,y_train,w_train))
  trainIMV<-trainIMV %>%
    uncount(round(w_train))
  
  IMVtest<-imv(testIMV$y_test,mean(testIMV$testpredicted),testIMV$testpredicted)
  IMVtrain<-imv(trainIMV$y_train,mean(trainIMV$trainpredicted),trainIMV$trainpredicted)
  
  print(paste("for fold",K,"In testing sample: AUC on Lasso probitmodel:",CVLassoProbit$AUCTest,"IMV:",IMVtest))
  print(paste("for fold",K,"In training sample: AUC on Lasso probitmodel:",CVLassoProbit$AUCTrain,"IMV:",IMVtrain))
  
  rm(trainIMV)
  rm(testIMV)
  
  #Make weights on holdout data
  
  xFeature<-TempData[TempData$Test==K,]
  xFeature$fittedLasso<-predict(CVLassoProbit,newx = x_testSmall,type="response",s = "lambda.min")
  xFeature$LassoWeight<- WeightNum/xFeature$fittedLasso
  
  WeightData<-xFeature[xFeature$ukb_selected==1,]
  summary(WeightData$LassoWeight)
  
  TestAssign <- read.table("../TEMP/TestAssign.csv",header=TRUE,sep=",") 
  WeightData<-WeightData[c("f.eid","LassoWeight")]
  WeightData<-join(WeightData,TestAssign, by="f.eid",match="first")
  head(WeightData)
  summary(WeightData)
  
  #Assign random weights, with replacement, to run a weighted GWAS with ``mis-estimated'' weights, which will serve as a Placebo analysis. 
  WeightData$PlaceboWeight<-NA
  WeightData$PlaceboWeight<-sample(WeightData$LassoWeight, replace=TRUE)
  
  cor.test(WeightData$LassoWeight,WeightData$PlaceboWeight,use="pairwise.complete.obs") #Expected correlation is zero. 
  #save weightData here:
  write.csv(WeightData[c("f.eid","LassoWeight","PlaceboWeight")], paste0("../TEMP/UKBLassoWeights",K,".csv"),row.names=FALSE)
  
  #Winsorize lasso weights to prevent outliers
  #WinsorData<-WeightData 
  #TrimValue<-quantile(WinsorData$LassoWeight,.99,na.rm=TRUE)
  #LowTrimValue<-quantile(WinsorData$LassoWeight,.01,na.rm=TRUE)
  #WinsorData$LassoWeight[WinsorData$LassoWeight>TrimValue]<-TrimValue
  #WinsorData$LassoWeight[WinsorData$LassoWeight<LowTrimValue]<-LowTrimValue
  
  #Assign random weights, with replacement, to run a weighted GWAS with ``mis-estimated'' weights, which will serve as a Placebo analysis. 
  #WinsorData$PlaceboWeight<-NA
  #WinsorData$PlaceboWeight[WeightData$InTraining==0]<-sample(WinsorData$LassoWeight[WeightData$InTraining==0], replace=TRUE)
  #WinsorData$PlaceboWeight[WeightData$InTraining==1]<-sample(WinsorData$LassoWeight[WeightData$InTraining==1], replace=TRUE)
  
  #cor.test(WinsorData$LassoWeight[WeightData$InTraining==0],WinsorData$PlaceboWeight[WeightData$InTraining==0],use="pairwise.complete.obs") #Expected correlation is zero. 
  #cor.test(WinsorData$LassoWeight[WeightData$InTraining==1],WinsorData$PlaceboWeight[WeightData$InTraining==1],use="pairwise.complete.obs") #Expected correlation is zero. 
  #cor.test(WinsorData$LassoWeight,WinsorData$PlaceboWeight,use="pairwise.complete.obs") #Expected correlation is zero. 
  
  #summary(WinsorData[c("f.eid","LassoWeight","PlaceboWeight")])
  #write.csv(WinsorData[c("f.eid","LassoWeight","PlaceboWeight")], paste0("../TEMP/UKBLassoWeightsTrim",K,".csv"),row.names=FALSE)
  
  rm(x_testSmall)
  rm(x_trainSmall)
  
  #xFeature<-xFeature[select]
  print(NROW(xFeature))
  print(NROW(y_test))
  #Program a permutation-based approach to arrive at a variable importance plot:
  PermuteColumn <- function(originalFile, colName, iseed = 317+K) {
    set.seed(iseed)
    varNames <- colnames(originalFile)
    colIndex <- which(varNames == colName)
    x <- originalFile[ ,colIndex]
    y <- sample(x)
    outFrame <- originalFile
    outFrame[ ,colIndex] <- y
    return(outFrame)
  }
  
  varnum<-length(vars)
  VIP<-tibble(Variable=vars,
              AUCIncrease=c(rep(0,varnum)),
              AUCRelIncrease=c(rep(0,varnum)))
  
  for (var in vars){
    print(var)
    xFeatureMod<-PermuteColumn(xFeature[select],var)
    xFeatureMod <- model.matrix(f, xFeatureMod)[,-1]
    xModSmall<-Matrix(xFeatureMod, sparse=TRUE)
    
    TempWeightData<-xFeature
    
    fittedLassoMod <- predict(CVLassoProbit,newx = xModSmall,type="response",s = "lambda.min")
    fittedLassoMod<-unname(fittedLassoMod)
    #Make leave-one-out weights and save
    TempWeightData$LassoWeight<- WeightNum/fittedLassoMod
    print(head(TempWeightData))
    TempWeightData<-TempWeightData[TempWeightData$ukb_selected==1,]
    print(NROW(WeightData$LassoWeight))
    print(NROW(TempWeightData$LassoWeight))
    
    print(summary(WeightData$LassoWeight))
    print(summary(TempWeightData$LassoWeight))
    

    print(paste("correlation between full weights and leave-one-out for",var,cor(WeightData$LassoWeight,TempWeightData$LassoWeight,use="pairwise.complete.obs")[1]))
    
    TestAssign <- read.table("../TEMP/TestAssign.csv",header=TRUE,sep=",") 
    TempWeightData<-TempWeightData[c("f.eid","LassoWeight")]
    TempWeightData<-join(TempWeightData,TestAssign, by="f.eid",match="first")
    print(head(TempWeightData))
    write.csv(TempWeightData[c("f.eid","LassoWeight","Test")], paste0("../TEMP/UKBLassoWeightsLeaveOneOut",var,K,".csv"),row.names=FALSE)
    rm(xFeatureMod)
    
    rocMod<- roc(y_test,fittedLassoMod)
    print(var)
    print(round(rocMod$auc,digits=3))
    
    VIP$AUCIncrease[VIP$Variable==var]<-round(roc$auc-rocMod$auc,digits=3)
    VIP$AUCRelIncrease[VIP$Variable==var]<-round(((roc$auc-rocMod$auc)/(roc$auc-0.5))*100,digits=3)
    
    rm(xModSmall)
    rm(fittedLassoMod)
    print(paste(var,"done"))
  }
  
  
  saveRDS(VIP,paste0("../TEMP/VIPData",K,".rds"))
  
  VIP<-readRDS(paste0("../TEMP/VIPData",K,".rds"))
  
  VIP$Variable <- factor(VIP$Variable, levels = VIP$Variable[order(VIP$AUCIncrease)], labels=varlabs[order(VIP$AUCIncrease)])
  # Plot VI scores
  png(paste0("../OUTPUT/FIGURES/VI",K,".png"), width=2400, height=1920, res=288)
  p<-ggplot(data=VIP, aes(x=Variable,y=AUCIncrease)) + geom_bar(stat="identity") + coord_flip() + ylab("Absolute increase in AUC \n due to variable") + theme_bw(base_size=20)
  print(p)
  dev.off()
  
  VIP$Variable <- factor(VIP$Variable, levels = VIP$Variable[order(VIP$AUCRelIncrease)])
  # Plot VI scores
  png(paste0("../OUTPUT/FIGURES/RelVI",K,".png"), width=2400, height=1920, res=288)
  p<-ggplot(data=VIP, aes(x=Variable,y=AUCRelIncrease)) + geom_bar(stat="identity") + coord_flip() + ylab("Relative increase in AUC \n due to variable (%)") + theme_bw(base_size=20)
  print(p)
  dev.off()
}
