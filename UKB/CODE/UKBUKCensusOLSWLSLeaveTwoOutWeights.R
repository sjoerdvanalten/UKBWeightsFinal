rm(list=ls())

library(plyr)
library(dplyr)
library(stargazer)
library(Hmisc)
library(reporttools)
library(pROC)
library(lmtest)
library(sandwich)
library(stats)
library(purrr)
library(doParallel)
library(foreach)
library(forcats)
library(ggpubr)
library(ggrepel)
library(colorspace)

options(bitmapType="cairo")

outcomes<-readRDS("../TEMP/OutcomesLeaveOneOut.rds")
inputs<-readRDS("../TEMP/InputsLeaveOneOut.rds")
UKB<-readRDS("../TEMP/DataForLeaveOneOut.rds")
output<-readRDS("../TEMP/OutputBinaryWithLasso.rds")

output$SE<-NULL
output$SENonRobust<-NULL

source("functions/UKBUKCensusOLSWLSFunctions.R")

group.colors <- c("UKB" = "#0072B2", "UK Census" = "#E69F00", "Weighted UKB, sex + birthyear + region" ="#CC79A7", 
                  "Leave-vars-out" = "#FC1303", "Weighted UKB, no Educ." = "#999999",
                  "Weighted UKB, all variables" = "#23C4B1", "Lasso" = "#23C4B1","Placebo" = "#009E73")


bivars=c('Employed','OwnsHouse','University','ReportsPoorHealth','Female','HasCar','BornBefore1950')

bivars<-as.data.frame(combn(bivars,2))
formula<- bivars %>% summarize_all(paste, collapse="~")

Woutput<-as.data.frame(matrix(NA,length(outcomes$outcomes),5))
count<-1

outcomes$formula<-paste0(outcomes$outcomes,"~",outcomes$input)

for (var in outcomes$outcomes){
  print(var)
  orgvar<-outcomes[count,"orgvar"]
  orgvarInput<-outcomes[count,"orgvarInput"]
  Weights<-read.table(paste0("../TEMP/UKBLassoWeightsLeaveTwoOut",orgvar,orgvarInput,1,".csv"),header=TRUE,sep=",")
  Weights$Test<-1
  #for (K in c(2:5)){
  #  temp<-read.table(paste0("../TEMP/UKBLassoWeightsLeaveTwoOut",orgvar,orgvarInput,K,".csv"),header=TRUE,sep=",")
  #  temp$Test<-K
  #  Weights<-rbind(Weights,temp)
  #}
  names(Weights)<-c("f.eid","LassoWeightLOU","Test")
  
  print(summary(Weights$LassoWeightLOU))
  
  p1<-quantile(Weights$LassoWeightLOU,0.01)
  p99<-quantile(Weights$LassoWeightLOU,0.99)
  
  print(paste("weights are cut off at",p1,"&",p99))
  Weights$LassoWeightLOU[Weights$LassoWeightLOU<p1]<-p1
  Weights$LassoWeightLOU[Weights$LassoWeightLOU>p99]<-p99
  
  print(summary(Weights$LassoWeightLOU))
  
  UKB <- join(UKB,Weights, by="f.eid",match="first")
  #temp1 <- formula[count] %>% 
  temp1 <- outcomes$formula[count] %>% 
      map(as.formula) %>%
    map(UKBWLS,UKB,w=UKB$LassoWeightLOU,lab="Leave-vars-out")
  names(temp1)<-formula[count]
  temp1<-as.data.frame(do.call(rbind,temp1))
  Woutput[count,]<-temp1
  count<-count+1
}

Woutput$P<-NA
Woutput$Corrected<-1
Woutput$pSig<-""


names(Woutput)<-names(output)

Woutput$point<-as.numeric(Woutput$point)
Woutput$CILow<-as.numeric(Woutput$CILow)
Woutput$CIHigh<-as.numeric(Woutput$CIHigh)
Woutput$P<-as.character(Woutput$P)

head(Woutput)
head(output)

output<-rbind(output,Woutput)

output$data<-factor(output$data,levels=c("Leave-vars-out","Lasso",
                                         "UKB","UK Census"))

print(output)

#Plot to compare simple bivariate OLS estimates in Census and UKB:
png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBLeaveTwoOut.png"), width=2880, height=2520, res=288)
ggplot(data=output, aes(x=formula,colour = data)) + geom_pointrange(data=subset(output,Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                    lwd = 6, fatten=000.1, shape="-",show.legend=FALSE) +
  geom_pointrange(data=subset(output,Corrected==1),aes(y=point,ymax=CIHigh, ymin=CILow), 
                  lwd = 1, fatten=1.5,position = position_dodge(width = 1/2), shape = 21, fill = "WHITE")  + 
  geom_text(data=subset(output,data=="UKB"),aes(x=formula, y=0.55, label = paste("P",P)),hjust=0,color="black",
            na.rm = TRUE) +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.6, 0.8)) +  xlab("") + ylab("Point estimate") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_colour_manual(values=group.colors, 
                                                                                name="Data", breaks=c("UKB","UK Census","Lasso",
                                                                                                      "Leave-vars-out"),
                                                                                labels=c("UKB unweighted","UK Census (UKB-eligible subsample)","UKB weighted",
                                                                                         "Weighted UKB, Leave-vars-out"),
                                                                                guide = guide_legend(override.aes = list(linetype = c(0,0,0,0), 
                                                                                                                         shape = c(15,15,21,21),lwd=2))
  )  + theme_bw()
dev.off()


#Estimate bias per association, and the average amount of reduction. 
CoutputLeaveOut<-output[output$data=="UK Census",]
BoutputLeaveOut<-output[output$data=="UKB",]
LoutputLeaveOut<-output[output$data=="Leave-vars-out",]

CoutputLeaveOut<-CoutputLeaveOut[c("formula","point")]
BoutputLeaveOut<-BoutputLeaveOut[c("formula","point")]
names(BoutputLeaveOut)[names(BoutputLeaveOut)=="point"]<-"point_UKB"
LoutputLeaveOut<-LoutputLeaveOut[c("formula","point")]
names(LoutputLeaveOut)[names(LoutputLeaveOut)=="point"]<-"point_LeaveOut"

CoutputLeaveOut<-merge(CoutputLeaveOut,BoutputLeaveOut)
CoutputLeaveOut<-merge(CoutputLeaveOut,LoutputLeaveOut)

BiasLeaveOut<-abs(CoutputLeaveOut$point-CoutputLeaveOut$point_UKB)
BiasWeightLeaveOut<-abs(CoutputLeaveOut$point-CoutputLeaveOut$point_LeaveOut)

BiasReductionLeaveOut<-(mean(BiasLeaveOut)-mean(BiasWeightLeaveOut))/mean(BiasLeaveOut)


print(paste("The reduction of selection bias across all models with binary regressors when leaving the input and output variables out is",BiasReductionLeaveOut*100,"%"))

