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
library(testit)
library(data.table)
library(estimatr)

options(bitmapType="cairo")

WeightedMean<-function(x, w, na.rm=TRUE,CI=FALSE){
  if (length(x)!=length(w)){
    stop("x and weights are not of same length")
  }
  if (na.rm==TRUE){ 
    w <- w[!is.na(x)]
    x <- x[!is.na(x)]
  }
  m<-sum(x*w)/sum(w)
  if (CI==TRUE){
  se=sqrt(sum(w*(x-m)^2)/sum(w))
  CILow<-m-(se/sqrt(NROW(x)))*1.96
  CIHigh<-m+(se/sqrt(NROW(x)))*1.96
  return(cbind(m,CILow,CIHigh))
  }else{return(m)}
  }

FullData<-read.table("../TEMP/UKBandCensusPriorToTesting.csv",header=TRUE,sep=",")
FullData$YearsEducation<-NULL
WinsorLassoWeights<-read.table("../OUTPUT/UKBWeightsKFolded.csv",header=TRUE,sep=",")
WinsorLassoWeights$Test<-NULL

UKB<-FullData[FullData$ukb_selected==1,]
UKB <- join(UKB,WinsorLassoWeights, by="f.eid",match="first")
NROW(UKB)
UKB<-UKB[!is.na(UKB$LassoWeight),]
NROW(UKB)

UKB<-mutate(UKB,Urban=case_when(UrbanRural==1|UrbanRural==5|UrbanRural==11|UrbanRural==12~1,
                                UrbanRural==2|UrbanRural==3|UrbanRural==4|UrbanRural==6|UrbanRural==7|
                                  UrbanRural==8|UrbanRural==13|UrbanRural==14|UrbanRural==15|UrbanRural==16|
                                  UrbanRural==17|UrbanRural==18~0))

UKBTownsend<-read.table("../TEMP/UKBTownsend.csv",header=TRUE,sep=",")

UKB <- join(UKB,UKBTownsend, by="f.eid",match="first")
Pheno <- readRDS("../TEMP/GWASControls.Rda")
UKB <- join(UKB,Pheno, by="f.eid",match="first")

UKB$Breastfed[UKB$Breastfed<0]<-NA
UKB$NoInHH[UKB$NoInHH<0]<-NA
UKB$Tea[UKB$Tea==-10]<-0
UKB$Tea[UKB$Tea<0]<-NA
UKB$EverAddicted[UKB$EverAddicted<0]<-NA
UKB$EverAddictedAlcohol[UKB$EverAddictedAlcohol<0]<-NA
UKB$EverCannabis[UKB$EverCannabis<0]<-NA
UKB$MaxFreqCannabis[UKB$MaxFreqCannabis<0]<-NA
UKB$ChestPain[UKB$ChestPain<0]<-NA
UKB$CookedVeg[UKB$CookedVeg==-10]<-0
UKB$CookedVeg[UKB$CookedVeg<0]<-NA
UKB$CheeseIntake[UKB$CheeseIntake<0]<-NA
UKB$PartOfMultipleBirth[UKB$PartOfMultipleBirth<0]<-NA
UKB$Handedness[UKB$Handedness<0]<-NA
UKB$LeftHanded[UKB$Handedness>=0&!is.na(UKB$Handedness)]<-0
UKB$LeftHanded[UKB$Handedness==2]<-1
UKB$Ambidextrous[UKB$Handedness>=0&!is.na(UKB$Handedness)]<-0
UKB$Ambidextrous[UKB$Handedness==3]<-1
UKB$AdoptedAsChild[UKB$AdoptedAsChild<0]<-NA
UKB$MaternalSmoking[UKB$MaternalSmoking<0]<-NA
UKB$TimeEmployedCurrentJob[UKB$TimeEmployedCurrentJob==-10]<-0
UKB$TimeEmployedCurrentJob[UKB$TimeEmployedCurrentJob<0]<-NA

UKB$LengthOfWorkingWeek[UKB$LengthOfWorkingWeek<0]<-NA
UKB$HeavyManualWork[UKB$HeavyManualWork<0]<-NA
UKB$Happiness[UKB$Happiness<0]<-NA
UKB$Happiness<- -UKB$Happiness + 7
UKB$HHIncome[UKB$HHIncome<0]<-NA

UKB <- mutate(UKB,HHIncome=case_when(HHIncome==1~18000,
                                               HHIncome==2~24500,
                                               HHIncome==3~42000,
                                               HHIncome==4~76000,
                                               HHIncome==5~150000))


for (var in c('EverAddicted','VitaminD','Calcium','Cholesterol','WhiteBloodCellCount','RedBloodCellCount',
'BreadConsumed','EverCannabis','MaxFreqCannabis','ChestPain','CookedVeg','CheeseIntake',
'PartOfMultipleBirth','BirthWeight','LeftHanded','Ambidextrous','AdoptedAsChild','MaternalSmoking','AgeCompletedEduc',
'TimeEmployedCurrentJob','LengthOfWorkingWeek','HeavyManualWork','HandGripStrengthLeft',
'HandGripStrengthRight','Happiness','HHIncome','Died')){
  print(var)
  print(summary(UKB[,var]))
}

UKB$AlcoholMoreThan5<-as.numeric(UKB$AlcoholFreq==1)

BMIReg<-lm_robust(VitaminD~BMI,data=UKB)
BMIRegW<-lm_robust(VitaminD~BMI,data=UKB,weights=UKB$LassoWeight)

CholReg<-lm_robust(Cholesterol~BMI,data=UKB)
CholRegW<-lm_robust(Cholesterol~BMI,data=UKB,weights=UKB$LassoWeight)

CholVitReg<-lm_robust(Cholesterol~VitaminD,data=UKB)
CholVitRegW<-lm_robust(Cholesterol~VitaminD,data=UKB,weights=UKB$LassoWeight)

SBPReg<-lm_robust(SBP~BMI,data=UKB)
SBPRegW<-lm_robust(SBP~BMI,data=UKB,weights=UKB$LassoWeight)

SBPReg<-lm_robust(SBP~AgeAtRecruitment,data=UKB)
SBPRegW<-lm_robust(SBP~AgeAtRecruitment,data=UKB,weights=UKB$LassoWeight)

SBPReg<-lm_robust(SBP~AlcoholFreq,data=UKB)
SBPRegW<-lm_robust(SBP~AlcoholFreq,data=UKB,weights=UKB$LassoWeight)

SBPReg<-lm_robust(Died~AlcoholFreq,data=UKB)
SBPRegW<-lm_robust(Died~AlcoholFreq,data=UKB,weights=UKB$LassoWeight)

SBPReg<-lm_robust(Died~EverSmoked+birthyearcenter+sex+Education,data=UKB)
SBPRegW<-lm_robust(Died~EverSmoked+birthyearcenter+sex+Education,data=UKB,weights=UKB$LassoWeight)
SBPReg<-lm_robust(Died~CigaretNum+birthyearcenter+sex+Education,data=UKB)
SBPRegW<-lm_robust(Died~CigaretNum+birthyearcenter+sex+Education,data=UKB,weights=UKB$LassoWeight)
SBPReg<-lm_robust(Died~AlcoholMoreThan5+birthyearcenter+sex+Education,data=UKB)
SBPRegW<-lm_robust(Died~AlcoholMoreThan5+birthyearcenter+sex+Education,data=UKB,weights=UKB$LassoWeight)
SBPReg<-lm_robust(Died~YearsEducation+birthyearcenter+sex,data=UKB)
SBPRegW<-lm_robust(Died~YearsEducation+birthyearcenter+sex,data=UKB,weights=UKB$LassoWeight)
SBPReg<-lm_robust(Died~BMI+birthyearcenter+sex,data=UKB)
SBPRegW<-lm_robust(Died~BMI+birthyearcenter+sex,data=UKB,weights=UKB$LassoWeight)


categories<-c("Demographic","Demographic","Socioeconomic","Socioeconomic",
              "Anthropometric","Anthropometric","Anthropometric","Anthropometric","Anthropometric",
              "Health","Mental Health","Health","Health","Health","Early lifetime",
              "Health behavior","Health behavior","Food/Beverage consumption","Health behavior",
              "Health behavior","Health","Health","Health","Health","Health","Food/Beverage consumption",
              "Health behavior","Health behavior","Health","Food/Beverage consumption","Food/Beverage consumption",
              "Early lifetime","Early lifetime","Other","Other","Early lifetime","Early lifetime","Socioeconomic",
              "Socioeconomic","Socioeconomic","Socioeconomic","Health","Health","Other","Socioeconomic","Demographic")

vars<-c("AgeAtRecruitment","Urban","TownsendCont","NoInHH","Height",
        "BMI","WC","WHR","HIP","Disability","Depression",
        "Asthma","DBP","SBP", "Breastfed","EverSmoked","AlcoholFreq","Tea",
        "CigaretNum",'EverAddicted','VitaminD','Calcium','Cholesterol','WhiteBloodCellCount','RedBloodCellCount',
        'BreadConsumed','EverCannabis','MaxFreqCannabis','ChestPain','CookedVeg','CheeseIntake',
        'PartOfMultipleBirth','BirthWeight','LeftHanded','Ambidextrous','AdoptedAsChild','MaternalSmoking','AgeCompletedEduc',
        'TimeEmployedCurrentJob','LengthOfWorkingWeek','HeavyManualWork','HandGripStrengthLeft',
        'HandGripStrengthRight','Happiness','HHIncome','Died')

varlabels<-c("Age at Recruitment","Urban","Townsend Cont.","No. of people in household","Height","BMI","Waist Circumference","Waist Hip Ratio",
             "Hip Circumference","Disability","Depression","Asthma","DBP","SBP","Breastfed","Ever Smoked","Alcohol Freq.","Tea",
        "Number of Cigarettes","Ever Addicated","Vitamin D","Calcium","Cholesterol","White Blood Cell Count",
        "Red Blood Cell Count","Bread Consumed","Ever Smoked Cannabis","Max Freq. Cannabis use","Chest Pain","Cooked Veg. Consumption",
        "Cheese Consumption","Multiple Birth","Birth Weight","Left Handed","Ambidextrous","Adopted as a child","Maternal smoking",
        "Age completed full-time educ.","Time employed current job","Length working week","Heavy manual work","Hand grip strength (left)",
        "Hand grip strength (right)","Happiness","Household income","Died after Census Day")

g<-c()
for (v in vars){
  x<-t.test(UKB[,v])$conf.int[1:2]
  y<-t.test(UKB[,v])$estimate[1]
  m<-c(x,y)
  l<-WeightedMean(UKB[,v],UKB$LassoWeight,na.rm=TRUE,CI=TRUE)
  t<-c(m,l)
  g<-rbind(g,t)
  }


g<-cbind(varlabels,g)
g<-cbind(vars,g)
g<-cbind(categories,g)
g<-as.data.frame(g)
colnames(g)<-c("Category","Var","Varlabels","CILow","CIHigh","Mean","WMean","WCILow","WCIHigh")
g<-g[order(g$Category),]
g$Change<-abs((as.numeric(g$WMean)-as.numeric(g$Mean))/as.numeric(g$Mean))

cat<-unique(g$Category)
catindex<-which(!duplicated(g$Category))-1

g$Mean<-as.numeric(g$Mean)
g$CILow<-as.numeric(g$CILow)
g$CIHigh<-as.numeric(g$CIHigh)
g$WMean<-as.numeric(g$WMean)
g$WCILow<-as.numeric(g$WCILow)
g$WCIHigh<-as.numeric(g$WCIHigh)

g$Mean<-round(g$Mean,digits=3)
g$WMean<-round(g$WMean,digits=3)
g$CILow<-round(g$CILow,digits=3)
g$CIHigh<-round(g$CIHigh,digits=3)
g$WCILow<-round(g$WCILow,digits=3)
g$WCIHigh<-round(g$WCIHigh,digits=3)
g$Change<-round(g$Change,digits=3)

g$Mean<-paste0("\\textbf{",g$Mean,"} [",g$CILow,";",g$CIHigh,"]")
g$WeightedMean<-paste0("\\textbf{",g$WMean,"} [",g$WCILow,"; ",g$WCIHigh,"]")
g$Change<-paste0(g$Change*100,"\\%")

g<-g[c("Varlabels","Mean","WeightedMean","Change")]
# \\% change after weighting each mean calculated as the absolute value of the weighted mean, minus the unweighted mean, divided by the unweighted mean.
SumTab<-xtable(g,align = "ll|l|l|l", caption="\\textbf{Mean of various variables in the UKB before and after weighting using IPWs:} 95\\% confidence intervals around each mean included.")
addtorow<-list()
addtorow$pos<-as.list(catindex)
cat<-paste0("\\hline \\textit{",cat,"} \\\\ \\hline ")
addtorow$command<-as.vector(cat)

colnames(SumTab) = c("\\textbf{Variable}", "\\textbf{Mean [95\\% CI]}", "\\textbf{Weighted Mean [95\\% CI]}", "\\textbf{\\% Change}")
print(SumTab,include.rownames=FALSE,add.to.row=addtorow,sanitize.colnames.function = paste0,
      sanitize.text.function= paste0,type="latex",file="../OUTPUT/TABLES/WeightedMeans.tex",floating=FALSE,size="scriptsize",tabular.environment="longtable")
