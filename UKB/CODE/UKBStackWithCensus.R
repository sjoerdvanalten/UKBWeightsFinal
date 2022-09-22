
#library(plyr)
library(dplyr)
library(tidyverse)
library(stargazer)
library(Hmisc)
library(reporttools)
library(pROC)
library(xtable)
options(bitmapType="cairo")

##Load in region-matched data
rm(list=ls())
UKBPheno <- read.table("../TEMP/UKBWithRegion.csv",header=TRUE,sep=",")
UKBPheno$popweight <- 1

#Get list of regions with <=25 UKB participants. Throw these individuals out of the UKBData, 
#and throw individuals living in these regions out of the Census data.
RegionCount <- group_by(UKBPheno,region)
RegionCount <- summarise(RegionCount,count=n())

RegionKeep<-c(RegionCount$region[RegionCount$count>70])
RegionKeepSum<-sum(RegionCount$count[RegionCount$count>70]) #check how many individuals would be dropped from this condition

table(UKBPheno$region)
print(table(UKBPheno$region[UKBPheno$UKB_eligible==FALSE]))
N1<-NROW(UKBPheno)

UKBPheno<-UKBPheno[UKBPheno$region %in% RegionKeep,]
N2<-NROW(UKBPheno)

saveRDS(RegionKeep,"../TEMP/RegionKeep.rds")

print(paste("To ensure that our UKB selection weights estimated with region as an explanatory variable for selection would not produce 
any extreme values, we also dropped UKB participants residing in Census local authority districts from which less than 50 UKB participants 
were sampled. This dropped another",N1-N2,"UKB observations."))

summary(UKBPheno$UKB_eligible)
print(paste("we drop", NROW(UKBPheno[is.na(UKBPheno$region),]), "because they have missing information on region of residence."))
print(table(UKBPheno$region[UKBPheno$UKB_eligible==FALSE]))
UKBPheno <- UKBPheno[UKBPheno$UKB_eligible==1,]
UKBPheno$UKB_eligible <- NULL

UKCensus<-read.table("../INPUT/CensusDataForProbit.csv",header=TRUE,sep=",")
sum(UKCensus$popweight)
NROW(UKCensus)
UKCensus<-UKCensus[UKCensus$region %in% RegionKeep,]
sum(UKCensus$popweight)
NROW(UKCensus)
table(UKCensus$region)

head(UKBPheno)

#save list of UKB genetic sample IDs that are included to subset from the genetic data:
UKCensus$"f.eid" <- 0
UKCensus$"UrbanRural"<-0
head(UKCensus)
UKCensus <- UKCensus[, colnames(UKBPheno)]
head(UKBPheno)
head(UKCensus) 
summary(UKBPheno$region)
summary(UKCensus$region)
head(UKCensus)
head(UKBPheno)
UKBPheno$region <- as.factor(UKBPheno$region)
UKCensus$region <- as.factor(UKCensus$region)
UKBPheno$carsnoc<- as.factor(UKBPheno$carsnoc)
UKCensus$carsnoc <- as.factor(UKCensus$carsnoc)
UKBPheno$Tenure<- as.factor(UKBPheno$Tenure)
UKCensus$Tenure <- as.factor(UKCensus$Tenure)

print(head(UKBPheno))
print(head(UKCensus))

FullData<-rbind(UKBPheno,UKCensus)

print(head(FullData))

FullData$region <- as.factor(FullData$region)
FullData$birthyearcenter<- as.factor(FullData$birthyearcenter)
FullData$Education <- as.factor(FullData$Education)

FullData$UKBSel <- factor(FullData$ukb_selected,c(0,1),c("Census","UKB"))
FullData$sex <- factor(FullData$sex,c(1,2),c("Male","Female"))

FullData$HasADegree<-as.numeric(as.numeric(FullData$Education)>1)
summary(FullData$HasADegree[FullData$ukb_selected==1])
summary(FullData$HasADegree[FullData$ukb_selected==0])

summary(FullData$Education)
#tableNominalAdjusted is a customized version of tableNominal() from the
#+reporttools package, which adds the argument showgrouptotal, with default value TRUE.


source("functions/tableNominalAdjusted.R")
tableNominalAdjusted(vars=FullData[,c("sex","birthyear","carsnoc","Tenure")],group=FullData$UKBSel,longtable=FALSE,
                     cumsum=FALSE,nams=c("Sex","Birth year cohort","No. of vehicles","Tenure"), print.pval = "fisher", floater=FALSE,
                     showgrouptotal=FALSE,file="../OUTPUT/UKBSum.tex")
FullData$sex <- as.integer(FullData$sex) - 1
rm(UKCensus,UKBPheno)
FullData<-FullData[!is.na(FullData$region),]

head(FullData)

print(ls())
gc()

#split data into 5 test folds):

set.seed(123)   # set seed to ensure you always have same random numbers generated


FullData$Test<-sample.int(5,size=NROW(FullData),replace=TRUE)

table(FullData$Test[FullData$ukb_selected==1])
table(FullData$Test[FullData$ukb_selected==0])

write.csv(FullData, "../TEMP/UKBandCensusPriorToTesting.csv",row.names=FALSE)
saveRDS(FullData, file="../TEMP/UKBandCensusPriorToTesting.Rda")

TestAssign<-FullData[c("f.eid","Test")]
write.csv(TestAssign, "../TEMP/TestAssign.csv",row.names=FALSE)

m1<-0
m2<-0

varlist<-c("sex","Education","region","birthyearcenter","HealthSelfReport","Tenure","Empstat","carsnoc","SingleHousehold","Ethnicity")

MisData<-tibble(var=varlist,
                lab=c("Sex","Education","Region","Year of Birth","Health (Self-reported)",
                      "Tenure of dwelling","Employment status","Number of cars","One-person household",
                      "Ethnicity")
                ,MissingCensus="",MissingUKB="")

ind<-1
#Assess missing data
for (var in varlist){
  print(var)
  MisData[ind,"MissingCensus"]<-paste0(round(NROW(which(is.na(FullData[FullData$ukb_selected==0,var])))/NROW(FullData[FullData$ukb_selected==0,var])*100,2),"\\%")
  MisData[ind,"MissingUKB"]<-paste0(round(NROW(which(is.na(FullData[FullData$ukb_selected==1,var])))/NROW(FullData[FullData$ukb_selected==1,var])*100,2),"\\%")
  ind<-ind+1
}


print(NROW(which(rowSums(is.na(FullData[FullData$ukb_selected==0,varlist])) > 0, ))/NROW(FullData[FullData$ukb_selected==0,]))
print(NROW(which(rowSums(is.na(FullData[FullData$ukb_selected==1,varlist])) > 0, ))/NROW(FullData[FullData$ukb_selected==1,]))

Tab<-xtable(MisData[c(2,3,4)],align = "cc|c|c")
colnames(Tab) = c("\\textbf{Variable}", "\\textbf{UK Census}", "\\textbf{UKB}")
print(Tab,include.rownames=FALSE,sanitize.colnames.function = paste0,
      sanitize.text.function= paste0,type="latex",file="../OUTPUT/TABLES/MissingData.tex",floating=FALSE)

