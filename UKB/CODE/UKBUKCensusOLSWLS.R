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

source("functions/UKBUKCensusOLSWLSFunctions.R")

FullData<-read.table("../TEMP/UKBandCensusPriorToTesting.csv",header=TRUE,sep=",")

UKBWeights<-read.table("../OUTPUT/UKBWeightsKFolded.csv",header=TRUE,sep=",")

table(FullData$HealthSelfReport)
UKB<-FullData[FullData$ukb_selected==1,]
UKCensus<-FullData[FullData$ukb_selected==0,]

UKBWeights<-UKBWeights[c("f.eid","LassoWeight")]

UKB <- join(UKB,UKBWeights, by="f.eid",match="first")

names(UKB)[names(UKB)=='birthyearcenter']<-'Birthyear'
names(UKCensus)[names(UKCensus)=='birthyearcenter']<-'Birthyear'

names(UKB)[names(UKB)== 'sex']<-'Female'
names(UKCensus)[names(UKCensus)== 'sex']<-'Female'

UKB$OwnsHouse<-as.numeric(UKB$Tenure==1|UKB$Tenure==2)
UKCensus$OwnsHouse<-as.numeric(UKCensus$Tenure==1|UKCensus$Tenure==2)

UKB$Employed<-as.numeric(UKB$Empstat==1)
UKCensus$Employed<-as.numeric(UKCensus$Empstat==1)

UKB$Unemployed<-as.numeric(UKB$Empstat==5)
UKCensus$Unemployed<-as.numeric(UKCensus$Empstat==5)

UKB$Retired<-as.numeric(UKB$Empstat==2)
UKCensus$Retired<-as.numeric(UKCensus$Empstat==2)

UKB$HasCar<-as.numeric(UKB$carsnoc>=1)
UKCensus$HasCar<-as.numeric(UKCensus$carsnoc>=1)

UKB$BornBefore1950<-as.numeric(UKB$Birthyear<=1948)
UKCensus$BornBefore1950<-as.numeric(UKCensus$Birthyear<=1948)

UKB$Welsh<-as.numeric(UKB$BirthCountry==4)
UKCensus$Welsh<-as.numeric(UKCensus$BirthCountry==4)

UKB$University<-as.numeric(UKB$Education==4)
UKCensus$University<-as.numeric(UKCensus$Education==4)

UKB$White<-as.numeric(UKB$Ethnicity==1)
UKCensus$White<-as.numeric(UKCensus$Ethnicity==1)

UKB$Black<-as.numeric(UKB$Ethnicity==4)
UKCensus$Black<-as.numeric(UKCensus$Ethnicity==4)

table(UKCensus$ReportsPoorHealth)
table(UKB$Education)
table(UKB$ReportsPoorHealth)

#Set colours for groups in ggplot:
group.colors <- c("UKB" = "#0072B2", "UK Census" = "#E69F00", "Weighted UKB, sex + birthyear + region" ="#CC79A7", 
                  "Leave-vars-out" = "#FC1303", "Weighted UKB, no Educ." = "#999999",
                  "Weighted UKB, all variables" = "#23C4B1", "Lasso" = "#23C4B1","Placebo" = "#009E73")


bivars=c('Employed','OwnsHouse','University','ReportsPoorHealth','Female','HasCar','BornBefore1950')

bivars<-as.data.frame(combn(bivars,2))
formula<- bivars %>% summarize_all(paste, collapse="~")

output<-formula %>% 
  map(as.formula) %>% 
  map(UKBUKCensus,UKCensus,UKB,pMethod="zTest",SEs=TRUE)

names(output)<-formula
output<-as.data.frame(do.call(rbind,output))
output[,-c(1:2)]<-lapply(output[,-c(1:2)],as.numeric)

output$Corrected<-0

print(output)

output$pSig[as.numeric(output$P)<0.05/(NROW(output)/2)]<-"*"
output$pSig[as.numeric(output$P)>=0.05/(NROW(output)/2)]<-""
output$P<-PClean(output$P)

png(paste0("../OUTPUT/FIGURES/PhenoUKCUKB.png"), width=2880, height=1800, res=288)
ggplot(data=output, aes(x=formula,colour = data)) + 
  geom_pointrange(data=subset(output,Corrected==0&pSig=="*"),aes(y=point,ymax=CIHigh, ymin=CILow), 
                  lwd = 6, fatten=000.1, shape="-")+
    scale_colour_manual(values=group.colors, 
                      name="Data",breaks=c("UKB","UK Census"),
                      labels=c("UKB unweighted","UK Census  (UKB-eligible subsample)"),
                      guide = guide_legend(override.aes = list(linetype = c(0,0), shape = c(15,15),lwd=2))) +
  geom_text(data=subset(output,data=="UKB"),aes(x=formula, y=0.55, label = paste("P",P)),hjust=0,color="black",
                   na.rm = TRUE) +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.6, 0.7)) +  xlab("") + ylab("Point estimate") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
    theme_bw() + theme(legend.position="bottom")
dev.off()

png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBOwnsHouseReportsPoorHealth.png"), width=1440, height=720, res=144)
ggplot(data=output[output$formula=="OwnsHouse ~ ReportsPoorHealth",], aes(x=formula,colour = data)) + geom_pointrange(data=subset(output[output$formula=="OwnsHouse ~ ReportsPoorHealth",],Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                    lwd = 6, fatten=000.1, shape="-")+
  scale_colour_manual(values=group.colors, 
                      name="Data & Method",breaks=c("UKB","UK Census"),
                      labels=c("UKB unweighted","UK Census (UKB-eligible subsample)"),
                      guide = guide_legend(override.aes = list(linetype = c(0,0), shape = c(15,15), lwd=2))) +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.6, 0.3)) +  xlab("Formula (y~x)") + ylab("Point estimate (95% CI)") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  theme_bw() + theme(legend.position="bottom")
dev.off()

png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBOwnsHouseUniversity.png"), width=1440, height=720, res=144)
ggplot(data=output[output$formula=="OwnsHouse ~ University",], aes(x=formula,colour = data)) + geom_pointrange(data=subset(output[output$formula=="OwnsHouse ~ University",],Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                                                                      lwd = 6, fatten=000.1, shape="-")+
  scale_colour_manual(values=group.colors, 
                      name="Data & Method",breaks=c("UKB","UK Census"),
                      labels=c("UKB unweighted","UK Census (UKB-eligible subsample)"),
                      guide = guide_legend(override.aes = list(linetype = c(0,0), shape = c(15,15), lwd=2))) +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.1, 0.3)) +  xlab("Formula (y~x)") + ylab("Point estimate (95% CI)") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  theme_bw() + theme(legend.position="bottom")
dev.off()

png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBReportsPoorHealthBornBefore1950.png"), width=1440, height=720, res=144)
ggplot(data=output[output$formula=="ReportsPoorHealth ~ BornBefore1950",], aes(x=formula,colour = data)) + geom_pointrange(data=subset(output[output$formula=="ReportsPoorHealth ~ BornBefore1950",],Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                                                                      lwd = 6, fatten=000.1, shape="-")+
  scale_colour_manual(values=group.colors, 
                      name="Data & Method",breaks=c("UKB","UK Census"),
                      labels=c("UKB unweighted","UK Census (UKB-eligible subsample)"),
                      guide = guide_legend(override.aes = list(linetype = c(0,0), shape = c(15,15), lwd=2))) +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.05, 0.1)) +  xlab("Formula (y~x)") + ylab("Point estimate (95% CI)") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  theme_bw() + theme(legend.position="bottom")
dev.off()

#Add lasso weights
WoutputLasso<-formula %>% 
  map(as.formula) %>% 
  map(UKBWLS,UKB,w=UKB$LassoWeight,lab="Lasso",odds=FALSE,SEs=TRUE)

names(WoutputLasso)<-formula
WoutputLasso<-as.data.frame(do.call(rbind,WoutputLasso))
WoutputLasso[,-c(1:2)]<-lapply(WoutputLasso[,-c(1:2)],as.numeric)
WoutputLasso$P<-NA
WoutputLasso$Corrected<-1
WoutputLasso$pSig<-""

output<-rbind(output,WoutputLasso)

saveRDS(output,file="../TEMP/OutputBinaryWithLasso.rds")
#Calculate effective sample sizes
outputForN<-output
#outputForN$SE<-(outputForN$point-outputForN$CILow)/qnorm(0.975) #figure out std. error
#outputForN$YVar<-do.call("cbind",strsplit(outputForN[,"formula"],split=" ~"))[1,]
#outputForN$VarY<-apply(UKB[,outputForN$YVar],2,var,na.rm=TRUE) 

outputForN$XVar<-do.call("cbind",strsplit(outputForN[,"formula"],split="~ "))[2,]
outputForN$VarX<-apply(UKB[,outputForN$XVar],2,var,na.rm=TRUE) 
outputForN$N_eff<-outputForN$varEpsilon/((outputForN$SE^2)*outputForN$VarX)
outputForN$N_effAlt<-outputForN$varEpsilon/((outputForN$SENonRobust^2)*outputForN$VarX)

#outputForN$N_effAlt<-(outputForN$point^2)/(outputForN$SE^2)

outputForN[43:NROW(outputForN),]$N_eff
mean(outputForN[43:NROW(outputForN),]$N_eff)

outputForN[43:NROW(outputForN),]$N_effAlt
mean(outputForN[43:NROW(outputForN),]$N_effAlt)


outputOrig<-output

output$data<-factor(output$data,levels=c("Lasso","Weighted UKB, sex + birthyear + region",
                                         "Weighted UKB, no Health, no Educ.",
                                         "Weighted UKB, no Educ.",
                                          "UKB","UK Census"))

#plot with Lasso:
png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBWeightLassoOwnsHouseReportsPoorHealth.png"), width=2880, height=1720, res=288)
ggplot(data=output[output$formula=="OwnsHouse ~ ReportsPoorHealth",], aes(x=formula,colour = data)) + geom_pointrange(data=subset(output[output$formula=="OwnsHouse ~ ReportsPoorHealth",],Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                    lwd = 6, fatten=000.1, shape="-",show.legend=FALSE)+
  geom_pointrange(data=subset(output[output$formula=="OwnsHouse ~ ReportsPoorHealth",],Corrected==1),aes(y=point,ymax=CIHigh, ymin=CILow), 
                  lwd = 2, fatten=1,position = position_dodge(width = 1/2), shape = 21, fill = "WHITE") +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.6, 0.5)) +  xlab("Formula (y~x)") + ylab("Point estimate (95% CI)") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_colour_manual(values=group.colors, 
                                                                                                  name="Data & Method",breaks=c("UKB","UK Census","Lasso"),
                                                                                                  labels=c("UKB unweighted","UK Census (UKB-eligible subsample)", "UKB weighted"),
                                                                                guide = guide_legend(override.aes = list(linetype = c(0,0,0), shape = c(15,15,21),lwd=2)))  + 
theme_bw() + theme(legend.position="bottom")
dev.off()


png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBWeightLassoOwnsHouseUniversity.png"), width=2880, height=1720, res=288)
ggplot(data=output[output$formula=="OwnsHouse ~ University",], aes(x=formula,colour = data)) + geom_pointrange(data=subset(output[output$formula=="OwnsHouse ~ University",],Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                                                                      lwd = 6, fatten=000.1, shape="-",show.legend=FALSE)+
  geom_pointrange(data=subset(output[output$formula=="OwnsHouse ~ University",],Corrected==1),aes(y=point,ymax=CIHigh, ymin=CILow), 
                  lwd = 2, fatten=1,position = position_dodge(width = 1/2), shape = 21, fill = "WHITE") +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.1, 0.3)) +  xlab("Formula (y~x)") + ylab("Point estimate (95% CI)") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_colour_manual(values=group.colors, 
                                                                                name="Data & Method",breaks=c("UKB","UK Census","Lasso"),
                                                                                labels=c("UKB unweighted","UK Census (UKB-eligible subsample)", "UKB weighted"),
                                                                                guide = guide_legend(override.aes = list(linetype = c(0,0,0), shape = c(15,15,21),lwd=2)))  + 
  theme_bw() + theme(legend.position="bottom")
dev.off()


png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBWeightLassoReportsPoorHealthBornBefore1950.png"), width=1440, height=720, res=144)
ggplot(data=output[output$formula=="ReportsPoorHealth ~ BornBefore1950",], aes(x=formula,colour = data)) + geom_pointrange(data=subset(output[output$formula=="ReportsPoorHealth ~ BornBefore1950",],Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                                                                      lwd = 6, fatten=000.1, shape="-",show.legend=FALSE)+
  geom_pointrange(data=subset(output[output$formula=="ReportsPoorHealth ~ BornBefore1950",],Corrected==1),aes(y=point,ymax=CIHigh, ymin=CILow), 
                  lwd = 2, fatten=1,position = position_dodge(width = 1/2), shape = 21, fill = "WHITE") +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.05, 0.1)) +  xlab("Formula (y~x)") + ylab("Point estimate (95% CI)") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_colour_manual(values=group.colors, 
                                                                                name="Data & Method",breaks=c("UKB","UK Census","Lasso"),
                                                                                labels=c("UKB unweighted","UK Census (UKB-eligible subsample)", "UKB weighted"),
                                                                                guide = guide_legend(override.aes = list(linetype = c(0,0,0), shape = c(15,15,21),lwd=2)))  + 
  theme_bw() + theme(legend.position="bottom")
dev.off()


#plot with Lasso:
png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBWeightLasso.png"), width=2880, height=1800, res=288)
ggplot(data=output, aes(x=formula,colour = data)) + geom_pointrange(data=subset(output,Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                    lwd = 6, fatten=000.1, shape="-",show.legend=FALSE)+
  geom_pointrange(data=subset(output,Corrected==1),aes(y=point,ymax=CIHigh, ymin=CILow), 
                  lwd = 2, fatten=1,position = position_dodge(width = 1/2), shape = 21, fill = "WHITE") +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.53, 0.6)) +  xlab("") + ylab("Point estimate") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_colour_manual(values=group.colors, 
                                                                                                  name="Data",breaks=c("UKB","UK Census","Lasso"),
                                                                                                  labels=c("UKB unweighted","UK Census (UKB-eligible subsample)", "UKB weighted"),
                                                                                guide = guide_legend(override.aes = list(linetype = c(0,0,0), shape = c(15,15,21),lwd=2)))  + 
  geom_text(data=subset(output,data=="UKB"),aes(x=formula, y=0.52, label = paste("P",P)),hjust=0,color="black",
            na.rm = TRUE) + theme_bw()  + theme(legend.position="bottom")
dev.off()

#Estimate bias per association, and the average amount of reduction. 
Coutput<-output[output$data=="UK Census",]
Boutput<-output[output$data=="UKB",]
Loutput<-output[output$data=="Lasso",]

Bias<-abs(Coutput$point-Boutput$point)
BiasWeight<-abs(Coutput$point-Loutput$point)

BiasReduction<-(mean(Bias)-mean(BiasWeight))/mean(Bias)

print(paste("The reduction of selection bias across all models with binary regressors is",BiasReduction*100,"%"))

#Leave outcome/input out weights: 
outcomes<-apply(formula,2,strsplit,split="~")
outcomes<-sapply(outcomes,"[[",1)[1,]
outcomes<-matrix(outcomes)
colnames(outcomes)<-"outcomes"

inputs<-apply(formula,2,strsplit,split="~")
inputs<-sapply(inputs,"[[",1)[2,]
inputs<-matrix(inputs)
colnames(inputs)<-"inputs"

outcomes<-cbind(outcomes,inputs)
#assign outcomes to the variables from which they were originally derived:
orgvars<-tibble(outcomes=c("Employed","OwnsHouse","University","ReportsPoorHealth","Female","HasCar","BornBefore1950"),
                orgvar=c("Empstat","Tenure","Education","HealthSelfReport","sex","carsnoc","birthyear"))

outcomes<-merge(outcomes,orgvars)
names(orgvars)<-c("inputs","orgvarInput")
outcomes<-merge(outcomes,orgvars)

saveRDS(outcomes,"../TEMP/OutcomesLeaveOneOut.rds")
saveRDS(inputs,"../TEMP/InputsLeaveOneOut.rds")
saveRDS(UKB,"../TEMP/DataForLeaveOneOut.rds")

#output$data<-factor(output$data,levels=c("Placebo","Lasso","Weighted UKB, no Educ.",
#                                         "Weighted UKB, no Health, no Educ.",
#                                         "Weighted UKB, sex + birthyear + region",
#                                         "UKB","UK Census"))


bivars=c('Employed','OwnsHouse','University','ReportsPoorHealth','HasCar')
bivars<-as.data.frame(combn(bivars,2))
formula<- bivars %>% summarize_all(paste, collapse="~")
formula<-paste(formula,"+Birthyear+Female")

outputCont<-formula %>% 
  map(as.formula) %>% 
  map(UKBUKCensus,UKCensus,UKB)

names(outputCont)<-formula
outputCont<-as.data.frame(do.call(rbind,outputCont))
outputCont[,-c(1:2)]<-lapply(outputCont[,-c(1:2)],as.numeric)

outputCont$Corrected<-0

png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBControl.png"), width=2880, height=1440, res=288)
ggplot(data=outputCont, aes(x=formula,colour = data)) + geom_pointrange(data=subset(outputCont,Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                    lwd = 6, fatten=000.1, shape="-")+
  scale_colour_manual(values=group.colors, 
                      name="Data & Method",breaks=c("UKB","UK Census"),
                      labels=c("UKB unweighted","UK Census (UKB-eligible subsample)"),
                      guide = guide_legend(override.aes = list(linetype = c(0,0), shape = c(21,21), lwd=1))) +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.6, 0.5)) +  xlab("Formula (y~x)") + ylab("Point estimate (95% CI)") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  theme_bw() + theme(legend.position="bottom")
dev.off()


#Add lasso weights
WoutputLassoCont<-formula %>% 
  map(as.formula) %>% 
  map(UKBWLS,UKB,w=UKB$LassoWeight,lab="Lasso",odds=FALSE)

names(WoutputLassoCont)<-formula
WoutputLassoCont<-as.data.frame(do.call(rbind,WoutputLassoCont))
WoutputLassoCont[,-c(1:2)]<-lapply(WoutputLassoCont[,-c(1:2)],as.numeric)
WoutputLassoCont$Corrected<-1
WoutputLassoCont$P<-NA

print("here")

outputCont<-rbind(outputCont,WoutputLassoCont)

outputCont$data<-factor(outputCont$data,levels=c("Lasso","Weighted UKB, sex + birthyear + region",
                                         "Weighted UKB, no Health, no Educ.",
                                         "Weighted UKB, no Educ.",
                                         "UKB","UK Census"))
outputCont$formula<-do.call("cbind",strsplit(outputCont[,"formula"],split=" \\+"))[1,]
#plot with Lasso:
png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBWeightLassoControl.png"), width=2880, height=1800, res=288)
ggplot(data=outputCont, aes(x=formula,colour = data)) + geom_pointrange(data=subset(outputCont,Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                    lwd = 6, fatten=000.1, shape="-",show.legend=FALSE)+
  geom_pointrange(data=subset(outputCont,Corrected==1),aes(y=point,ymax=CIHigh, ymin=CILow), 
                  lwd = 2, fatten=1,position = position_dodge(width = 1/2), shape = 21, fill = "WHITE") +
  #geom_pointrange(data=subset(output[-grep("Female|BornBefore1950",output$formula),],Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow, alpha=0.1), lwd = 6, fatten=000.1, shape="-",show.legend=FALSE)+
  #geom_pointrange(data=subset(output[-grep("Female|BornBefore1950",output$formula),],Corrected==1),aes(y=point,ymax=CIHigh, ymin=CILow, alpha=0.1,,stroke=0.3), 
  #                lwd = 2, fatten=1,position = position_dodge(width = 1/2), shape = 21, fill = "WHITE")+
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.5, 0.55)) +  xlab("Formula (y~x)") + ylab("Point estimate (95% CI)") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_colour_manual(values=group.colors, 
                                                                                                  name="Data & Method",breaks=c("UKB","UK Census","Lasso"),
                                                                                                  labels=c("UKB unweighted","UK Census (UKB-eligible subsample)", "UKB weighted"),
                                                                                guide = guide_legend(override.aes = list(linetype = c(0,0,0), shape = c(15,15,21),lwd=2)))  + theme_bw() + theme(legend.position="bottom")
dev.off()


#Estimate bias per association, and the average amount of reduction. 
CoutputCont<-outputCont[outputCont$data=="UK Census",]
BoutputCont<-outputCont[outputCont$data=="UKB",]
LoutputCont<-outputCont[outputCont$data=="Lasso",]

BiasCont<-abs(CoutputCont$point-BoutputCont$point)
BiasWeightCont<-abs(CoutputCont$point-LoutputCont$point)

BiasContReduction<-(mean(BiasCont)-mean(BiasWeightCont))/mean(BiasCont)

print(paste("The reduction of selection bias across all models with binary regressors is, and linearily controlling for birthyear and sex, is",BiasContReduction*100,"%"))

#combare bias before and after introduction of the control variables:
#CoutputCont$YX<-do.call("cbind",strsplit(CoutputCont[,"formula"],split=" \\+"))[1,]
#BoutputCont$YX<-do.call("cbind",strsplit(BoutputCont[,"formula"],split=" \\+"))[1,]
CoutputUncont<-Coutput[Coutput$formula %in% CoutputCont$formula,]
BoutputUncont<-Boutput[Boutput$formula %in% BoutputCont$formula,]
BiasUncont<-abs(CoutputUncont$point-BoutputUncont$point)

BiasControlReduction<-(mean(BiasCont)-mean(BiasUncont))/mean(BiasCont)
print(paste("When birhtyear and sex are controleld for bias is reduced by",BiasControlReduction*100,"%"))

formFullControl<-as.formula("ReportsPoorHealth ~ BornBefore1950 + Female + YearsEducation + as.factor(Tenure) + carsnoc + SingleHousehold + as.factor(Empstat) +as.factor(Ethnicity) + as.factor(region)")

outputFullControl<-UKBUKCensus(formFullControl,UKCensus,UKB,pMethod="zTest")

summary(lm(formFullControl,data=UKB))
summary(lm(formFullControl,data=UKCensus))

WoutputFullControl<-UKBWLS(formFullControl,UKB,w=UKB$LassoWeight,lab="Lasso",odds=FALSE)
#names(outputFullControl)<-"ReportsPoorHealth ~ BornBefore1950"
#names(WoutputFullControl)<-"ReportsPoorHealth ~ BornBefore1950"

outputFullControl<-as.data.frame(outputFullControl)
outputFullControl[,-c(1:2)]<-lapply(outputFullControl[,-c(1:2)],as.numeric)
outputFullControl$Corrected<-0

WoutputFullControl<-t(WoutputFullControl)
WoutputFullControl<-as.data.frame(WoutputFullControl)
#names(WoutputFullControl)<-formFullControl
WoutputFullControl[,-c(1:2)]<-lapply(WoutputFullControl[,-c(1:2)],as.numeric)

WoutputFullControl$P<-NA
WoutputFullControl$Corrected<-1

print(outputFullControl)
print(WoutputFullControl)


str(outputFullControl)
str(WoutputFullControl)

outputFullControl<-rbind(outputFullControl,WoutputFullControl)
outputFullControl$formula<-"ReportsPoorHealth ~ BornBefore1950 + Controls"

print(outputFullControl)
head(outputOrig)

outputOrig$SENonRobust<-NULL
outputOrig$SE<-NULL
outputOrig$varEpsilon<-NULL

outputFullControl<-rbind(outputFullControl,outputOrig[outputOrig$formula=="ReportsPoorHealth ~ BornBefore1950",-NCOL(outputOrig)])

print(outputFullControl)

outputFullControl<-outputFullControl[order(outputFullControl$formula),]

outputFullControl$formula<-factor(outputFullControl$formula,levels=c("ReportsPoorHealth ~ BornBefore1950 + Controls","ReportsPoorHealth ~ BornBefore1950"))


print(outputFullControl)

png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBWeightLassoFullControlReportsPoorHealthBornBefore1950.png"), width=1440, height=540, res=144)
ggplot(data=outputFullControl, aes(x=formula,colour = data)) + geom_pointrange(data=subset(outputFullControl,Corrected==0),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                                                                           lwd = 6, fatten=000.1, shape="-",show.legend=FALSE)+
  geom_pointrange(data=subset(outputFullControl,Corrected==1),aes(y=point,ymax=CIHigh, ymin=CILow), 
                  lwd = 2, fatten=1,position = position_dodge(width = 1/2), shape = 21, fill = "WHITE") +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.07, 0.07)) +  xlab("Formula (y~x)") + ylab("Point estimate (95% CI)") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_colour_manual(values=group.colors, 
                                                                                name="Data & Method",breaks=c("UKB","UK Census","Lasso"),
                                                                                labels=c("UKB unweighted","UK Census (UKB-eligible subsample)", "UKB weighted"),
                                                                                guide = guide_legend(override.aes = list(linetype = c(0,0,0), shape = c(15,15,21),lwd=2)))  + 
  theme_bw() + theme(legend.position="bottom")
dev.off()


#Next: birthyear, other cts. outcomes
rm(output)

names(UKB)[names(UKB)=='carsnoc']<-"NumberOfCars"
names(UKCensus)[names(UKCensus)=='carsnoc']<-"NumberOfCars"

contvars=c('HealthSelfReport','Birthyear','NumberOfCars','YearsEducation','Female')

varstand<-function(input){
  out<-(input-mean(input,na.rm=TRUE))/sd(input,na.rm=TRUE)
  return(out)
}

#standardize all variables in contvars:
for (var in contvars){
  UKB[,var]<-varstand(UKB[,var])
  UKCensus[,var]<-varstand(UKCensus[,var])
}


contvars<-as.data.frame(combn(contvars,2))
formula<- contvars %>% summarize_all(paste, collapse="~")
#formula<-c("Female~Birthyear","Education~Birthyear","University~Birthyear","HealthSelfReport~Birthyear",
#           "ReportsPoorHealth~Birthyear","OwnsHouse~Birthyear","Employed~Birthyear")
output<-formula %>% 
  map(as.formula) %>% 
  map(UKBUKCensus,UKCensus,UKB,pMethod="zTest",SEs=TRUE)

names(output)<-formula
output<-as.data.frame(do.call(rbind,output))
output[,-c(1:2)]<-lapply(output[,-c(1:2)],as.numeric)

output$Corrected<-0



output$pSig[as.numeric(output$P)<0.05/(NROW(output)/2)]<-"*"
output$pSig[as.numeric(output$P)>=0.05/(NROW(output)/2)]<-""
output$P<-PClean(output$P)

output

png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBBirthyear.png"), width=2880, height=1440, res=288)
ggplot(data=output, aes(x=formula,colour = data)) + 
  geom_pointrange(data=subset(output,Corrected==0&pSig=="*"),aes(y=point,ymax=CIHigh, ymin=CILow), 
                                                                    lwd = 6, fatten=000.1, shape="-")+
  geom_pointrange(data=subset(output,Corrected==0&pSig==""),aes(y=point,ymax=CIHigh, ymin=CILow, alpha="opacity"), 
                  lwd = 6, fatten=000.1, shape="-", show.legend=FALSE)+
  geom_text(data=subset(output,data=="UKB"),aes(x=formula, y=0.3, label = paste("P",P)),hjust=0,color="black",
            na.rm = TRUE) +
  scale_colour_manual(values=group.colors, 
                      name="Data",breaks=c("UKB","UK Census"),
                      labels=c("UKB unweighted","UK Census (UKB-eligible subsample)"),
                      guide = guide_legend(override.aes = list(linetype = c(0,0), shape = c(15,15), lwd=2))) +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.1, 0.4)) +  xlab("") + ylab("Point estimate") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  theme_bw() + theme(legend.position="bottom")
dev.off()

#Add lasso weights
WoutputLasso<-formula %>% 
  map(as.formula) %>% 
  map(UKBWLS,UKB,w=UKB$LassoWeight,lab="Lasso",odds=FALSE,SEs=TRUE)

names(WoutputLasso)<-formula
WoutputLasso<-as.data.frame(do.call(rbind,WoutputLasso))
WoutputLasso[,-c(1:2)]<-lapply(WoutputLasso[,-c(1:2)],as.numeric)
WoutputLasso$Corrected<-1
WoutputLasso$P<-NA
WoutputLasso$pSig<-""

output<-rbind(output,WoutputLasso)

output$data<-factor(output$data,levels=c("Lasso","Weighted UKB, sex + birthyear + region",
                                         "Weighted UKB, no Health, no Educ.",
                                         "Weighted UKB, no Educ.",
                                         "UKB","UK Census"))

#plot with Lasso:
png(paste0("../OUTPUT/FIGURES/PhenoUKCUKBWeightLassoBirthyear.png"), width=2880, height=1440, res=288)
ggplot(data=output, aes(x=formula,colour = data)) + 
  geom_pointrange(data=subset(output,Corrected==0&pSig=="*"),aes(y=point,ymax=CIHigh, ymin=CILow), 
                  lwd = 6, fatten=000.1, shape="-")+
  geom_pointrange(data=subset(output,Corrected==0&pSig==""),aes(y=point,ymax=CIHigh, ymin=CILow, alpha="opacity"), 
                  lwd = 6, fatten=000.1, shape="-", show.legend=FALSE)+
  geom_pointrange(data=subset(output,Corrected==1),aes(y=point,ymax=CIHigh, ymin=CILow), 
                  lwd = 2, fatten=1,position = position_dodge(width = 1/2), shape = 21, fill = "WHITE") +
  geom_text(data=subset(output,data=="UKB"),aes(x=formula, y=0.3, label = paste("P",P)),hjust=0,color="black",
            na.rm = TRUE) +
  coord_flip()  +
  scale_y_continuous(limits =  c(-0.1, 0.35)) +  xlab("") + ylab("Point estimate") + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + scale_colour_manual(values=group.colors, 
                                                            name="Data",breaks=c("UKB","UK Census","Lasso"),
                                                            labels=c("UKB unweighted","UK Census (UKB-eligible subsample)","UKB weighted"),
                                                            guide = guide_legend(override.aes = list(linetype = c(0,0,0), 
                                                                                                     shape = c(15,15,21),lwd=2)))  +
  theme_bw() + theme(legend.position="bottom")
dev.off()


#Calculate effective sample sizes
outputForN<-output
outputForN$SE_dif<-(outputForN$point-outputForN$CILow)/qnorm(0.975) #figure out std. error
#outputForN$YVar<-do.call("cbind",strsplit(outputForN[,"formula"],split=" ~"))[1,]
#outputForN$VarY<-1
#apply(UKB[,outputForN$YVar],2,var,na.rm=TRUE) 

outputForN$XVar<-do.call("cbind",strsplit(outputForN[,"formula"],split="~ "))[2,]
outputForN$VarX<-1
#apply(UKB[,outputForN$XVar],2,var,na.rm=TRUE) 
outputForN$N_eff<-outputForN$varEpsilon/((outputForN$SE^2)*outputForN$VarX)
outputForN$N_effAlt<-outputForN$varEpsilon/((outputForN$SENonRobust^2)*outputForN$VarX)
outputForN$N_effAlt2<-(outputForN$point^2)/(outputForN$SENonRobust^2)
print(outputForN)

outputForN[21:NROW(outputForN),]$N_eff
mean(outputForN[21:NROW(outputForN),]$N_eff)

outputForN[21:NROW(outputForN),]$N_effAlt
mean(outputForN[21:NROW(outputForN),]$N_effAlt)


#outputForN$N_effAlt<-(outputForN$point^2)/(outputForN$SE^2)

#Estimate bias per association, and the average amount of reduction. 
Coutput<-output[output$data=="UK Census",]
Boutput<-output[output$data=="UKB",]
Loutput<-output[output$data=="Lasso",]

Bias<-abs(Coutput$point[Coutput$pSig=="*"]-Boutput$point[Coutput$pSig=="*"])
BiasWeight<-abs(Coutput$point[Coutput$pSig=="*"]-Loutput$point[Coutput$pSig=="*"])
  
BiasReduction<-(mean(Bias)-mean(BiasWeight))/mean(Bias)

print(paste("The reduction of selection bias across all models with continuous regressors is",BiasReduction*100,"%"))



