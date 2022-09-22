##Date: 06/10/2020

##Author: Sjoerd van Alten

##Goal: Restrict the 2011 5% UK Census data towards a representative dataset of the
#+9.2 million individuals aged 40-69 who lived within 25 miles (40 km)
#+ of one of the 22 assessment centers in England, Wales and scotland

##Note: Scotland Census data should still be appended to this dataset

#remove all variables from previous history:
rm(list=ls())
#removes all plots momentarily load in RStudio:


library(dplyr)
library(xtable)

EngCensusData <- read.table("../DATA/RAW/recodev12.csv",header=TRUE,sep=",")
ScotCensusData <- read.table("../DATA/RAW/ScotlandCensus/csv/safeguarded_grouped_la.csv",header=TRUE,sep=",")

#Create IDs:
EngCensusData$ID<-paste0("ENG",seq(1:NROW(EngCensusData)))
ScotCensusData$ID<-paste0("SCO",seq(1:NROW(ScotCensusData)))
  
NROW(EngCensusData)
NROW(ScotCensusData)

EngCensusData$la_merge <- EngCensusData$la_group
EngCensusData$la_group <- as.factor(EngCensusData$la_group)

ScotCensusData$la_merge <- ScotCensusData$COUNCIL_AREA_GROUP
ScotCensusData$la_group <- as.factor(ScotCensusData$COUNCIL_AREA_GROUP)

colnames(EngCensusData)

#subset to individuals aged 40-69:
EngCensusData <- EngCensusData[EngCensusData$ageh>=9 & EngCensusData$ageh<=15,]

NROW(ScotCensusData)
ScotCensusData <- ScotCensusData[ScotCensusData$AGE>=9 & ScotCensusData$AGE<=15,]
NROW(ScotCensusData)

#Only individuals who were a usual resident (restriction not possible for Scotland, but we only loose very few ppl by doing this anyways.)
NewCensusData <- EngCensusData[EngCensusData$popbasesec==1,]
NewScotCensusData<-ScotCensusData
#There are 265 LA groups, restrict to those who were eligible for
#+ ukb-sampling

EligibleData <- read.table("../DATA/TEMP/UKBEligibleGeographic.csv",header=TRUE,sep=",")

EligibleData$LARegion <- EligibleData$LA.Group.Number1 
#EligibleData$Local.Authority.Name <- as.numeric(EligibleData$Local.Authority.Name)

table(NewCensusData$la_merge)
table(EligibleData$LA.Group.Number1)

EligibleData<-EligibleData[!is.na(EligibleData$popweight), ]
EngEligibleData<-EligibleData[EligibleData$Country=="EnglandOrWales",]
ScotEligibleData<-EligibleData[EligibleData$Country=="Scotland",]
NewCensusData$Country<-"EnglandOrWales"
NewScotCensusData$Country<-"Scotland"

NROW(NewCensusData)
NewCensusData<- merge(NewCensusData,EngEligibleData,by.x="la_merge",by.y="LA.Group.Number1")
NROW(NewCensusData)
NewCensusData<- NewCensusData[NewCensusData$popweight>0,]
sum(NewCensusData$popweight)

NROW(NewScotCensusData)
NewScotCensusData<- merge(NewScotCensusData,ScotEligibleData,by.x="la_merge",by.y="LA.Group.Number1")
NROW(NewScotCensusData)
NewScotCensusData<- NewScotCensusData[NewScotCensusData$popweight>0,]

sum(NewScotCensusData$popweight)

NewCensusData<-mutate(NewCensusData,Ethnicity=case_when(ethnicityew<=3~1,
                                                                  ethnicityew>=4& ethnicityew<=5~2,
                                                                  ethnicityew>=6&ethnicityew<=10~3,
                                                                  ethnicityew>=11&ethnicityew<=12~4,
                                                                  ethnicityew>=13~5))

NewScotCensusData<-mutate(NewScotCensusData,Ethnicity=case_when(ETHNIC<=6~1,
                                                       ETHNIC==7~2,
                                                       ETHNIC>=8&ETHNIC<=10~3,
                                                       ETHNIC>=11&ETHNIC<=12~4,
                                                       ETHNIC>=13~5))

table(NewCensusData$Ethnicity)


NewCensusData <- mutate(NewCensusData,Education=case_when(hlqupuk11==10 ~ 1,
                                                          hlqupuk11==11 ~ 2, hlqupuk11==12 ~ 2, 
                                                          hlqupuk11==13 ~ 5, hlqupuk11==14 ~ 3, hlqupuk11==15 ~ 5
                                                          , hlqupuk11==16 ~ 4))
NewCensusData <- mutate(NewCensusData,YearsEducation=case_when(hlqupuk11==10 ~ 7,
                                                          hlqupuk11==11 ~ 10, hlqupuk11==12 ~ 10, 
                                                          hlqupuk11==13 ~ 12, hlqupuk11==14 ~ 13, hlqupuk11==15 ~ 20
                                                          , hlqupuk11==16 ~ 15))

#Note: Scotland misses a category, check what the education distribution looks like for Scotland Assessed UKB participants.
NewScotCensusData <- mutate(NewScotCensusData,Education=case_when(HLQPS11==20 ~ 1,
                                                            HLQPS11==21 ~ 2,
                                                            HLQPS11==22 ~ 2,
                                                            HLQPS11==23 ~ 3,
                                                            HLQPS11==24 ~ 5))
NewScotCensusData <- mutate(NewScotCensusData,YearsEducation=case_when(HLQPS11==20 ~ 7,
                                                            HLQPS11==21 ~ 10,
                                                            HLQPS11==22 ~ 13,
                                                            HLQPS11==23 ~ 19,
                                                            HLQPS11==24 ~ 20))

summary(NewCensusData$YearsEducation)
summary(NewScotCensusData$YearsEducation)
#Keep only selected variables
NewCensusData <- NewCensusData[c("ID","sex","ageh","Education","LARegion","popweight","cobg","disability","ecopuk11","health","tenure",
                                 "empstat","carsnoc","MinWtAdjust","MaxWtAdjust","country","occg","YearsEducation","meighuk11","Ethnicity","ethnicityew")]
NewScotCensusData <- NewScotCensusData[c("ID","SEX","AGE","Education","LARegion","popweight","COB","DISABILITY","ECOPUK11","HEALTH","LANDLORD","TENHUK11","EMPSTAT",
                                   "CARSNO","MinWtAdjust","MaxWtAdjust","Country.x","OCCUPATION","YearsEducation","MEIGHS11","Ethnicity","ETHNIC")]

names(NewCensusData)[names(NewCensusData) == 'LARegion'] <- 'region'
names(NewCensusData)[names(NewCensusData) == 'tenure'] <- 'Tenure'
names(NewCensusData)[names(NewCensusData) == 'health'] <- 'HealthSelfReport'
names(NewCensusData)[names(NewCensusData) == 'occg'] <- 'Occupation'


names(NewScotCensusData)[names(NewScotCensusData) == 'LARegion'] <- 'region'
names(NewScotCensusData)[names(NewScotCensusData) == 'HEALTH'] <- 'HealthSelfReport'
names(NewScotCensusData)[names(NewScotCensusData) == 'OCCUPATION'] <- 'Occupation'

NewCensusData$SingleHousehold<-NA
NewCensusData$SingleHousehold[NewCensusData$meighuk11==1]<-1
NewCensusData$SingleHousehold[NewCensusData$meighuk11>1]<-0

NewScotCensusData$SingleHousehold<-NA
NewScotCensusData$SingleHousehold[NewScotCensusData$MEIGHS11==1]<-1
NewScotCensusData$SingleHousehold[NewScotCensusData$MEIGHS11>1]<-0


#generate birth of year based on ageh:
NewCensusData <- mutate(NewCensusData,birthyear=case_when(ageh==9 ~ "1966-1970",
                                                          ageh==10 ~ "1961-1965", ageh==11 ~ "1956-1960", 
                                                          ageh==12 ~ "1951-1955", ageh==13 ~ "1946-1950", ageh==14 ~ "1941-1945"
                                                          , ageh==15 ~ "1936-1940"))
NewCensusData <- mutate(NewCensusData,birthyearcenter=case_when(ageh==9 ~ 1968,
                                                                ageh==10 ~ 1963, ageh==11 ~ 1958, 
                                                                ageh==12 ~ 1953, ageh==13 ~ 1948, ageh==14 ~ 1943
                                                                , ageh==15 ~ 1938))

NewScotCensusData <- mutate(NewScotCensusData,birthyear=case_when(AGE==9 ~ "1966-1970",
                                                          AGE==10 ~ "1961-1965", AGE==11 ~ "1956-1960", 
                                                          AGE==12 ~ "1951-1955", AGE==13 ~ "1946-1950", AGE==14 ~ "1941-1945"
                                                          , AGE==15 ~ "1936-1940"))
NewScotCensusData <- mutate(NewScotCensusData,birthyearcenter=case_when(AGE==9 ~ 1968,
                                                                AGE==10 ~ 1963, AGE==11 ~ 1958, 
                                                                AGE==12 ~ 1953, AGE==13 ~ 1948, AGE==14 ~ 1943
                                                                , AGE==15 ~ 1938))
NewCensusData$Occupation<-round(NewCensusData$Occupation/10)
NewCensusData$Occupation[is.na(NewCensusData$Occupation)]<--9
#adjust weight based on birth year cutoffs, to make the birth year distribution for each region in the UK Census closer to the
#+UKB eligible population (this differs per region due to how the UKB was sampled.)
sum(NewCensusData$popweight)
NewCensusData$popweight[NewCensusData$birthyearcenter==1938&NewCensusData$popweight>0.2]<-
  NewCensusData$popweight[NewCensusData$birthyearcenter==1938&NewCensusData$popweight>0.2]*NewCensusData$MinWtAdjust[NewCensusData$birthyearcenter==1938&NewCensusData$popweight>0.2]
NewCensusData$popweight[NewCensusData$birthyearcenter==1968&NewCensusData$popweight>0.2]<-
  NewCensusData$popweight[NewCensusData$birthyearcenter==1968&NewCensusData$popweight>0.2]*NewCensusData$MaxWtAdjust[NewCensusData$birthyearcenter==1968&NewCensusData$popweight>0.2]
sum(NewCensusData$popweight)

sum(NewScotCensusData$popweight)
NewScotCensusData$popweight[NewScotCensusData$birthyearcenter==1938&NewScotCensusData$popweight>0.2]<-
  NewScotCensusData$popweight[NewScotCensusData$birthyearcenter==1938&NewScotCensusData$popweight>0.2]*NewScotCensusData$MinWtAdjust[NewScotCensusData$birthyearcenter==1938&NewScotCensusData$popweight>0.2]
NewScotCensusData$popweight[NewScotCensusData$birthyearcenter==1968&NewScotCensusData$popweight>0.2]<-
  NewScotCensusData$popweight[NewScotCensusData$birthyearcenter==1968&NewScotCensusData$popweight>0.2]*NewScotCensusData$MaxWtAdjust[NewScotCensusData$birthyearcenter==1968&NewScotCensusData$popweight>0.2]
sum(NewScotCensusData$popweight)

NewCensusData <- mutate(NewCensusData,BirthCountry=case_when(cobg==1~1,cobg==2~2,cobg==3~3,cobg==4~4,cobg==6~5,cobg==7~6,cobg==8~7,
                                                             cobg==9~8,cobg==10~9,cobg==11~10,cobg==12~11,cobg==13~12))
NewScotCensusData <- mutate(NewScotCensusData,BirthCountry=case_when(COB==1~1,COB==2~2,COB==3~3,COB==4~4,COB==6~5,COB==7~6,COB==8~7,
                                                             COB==9~8,COB==10~9,COB==11~10,COB==12~11,COB==13~12))


NewCensusData <- mutate(NewCensusData,disability=case_when(disability==1~1, disability==2~1,disability==3~0))
NewScotCensusData <- mutate(NewScotCensusData,disability=case_when(DISABILITY==1~1, DISABILITY==2~1,DISABILITY==3~0))

NewCensusData <- mutate(NewCensusData,HealthSelfReport=case_when(HealthSelfReport==5~1,HealthSelfReport==4~1,
                                                                 HealthSelfReport==3~2,HealthSelfReport==2~3,HealthSelfReport==1~3))
NewScotCensusData <- mutate(NewScotCensusData,HealthSelfReport=case_when(HealthSelfReport==5~1,HealthSelfReport==4~1,
                                                                 HealthSelfReport==3~2,HealthSelfReport==2~3,HealthSelfReport==1~3))


NewCensusData <- mutate(NewCensusData,Empstat=case_when(ecopuk11<=6~1,ecopuk11==7~5,ecopuk11==8~1,ecopuk11==9~7,
                                                        ecopuk11==11~7,ecopuk11==10~2,ecopuk11==14~2,ecopuk11==12~3,
                                                        ecopuk11==13~4))

NewScotCensusData <- mutate(NewScotCensusData,Empstat=case_when(ECOPUK11<=6~1,ECOPUK11==7~5,ECOPUK11==8~1,ECOPUK11==9~2,
                                                          ECOPUK11==11~3,ECOPUK11==10~7,ECOPUK11==12~4,
                                                          ECOPUK11==13~2))

NewScotCensusData<-mutate(NewScotCensusData,Tenure=case_when(TENHUK11==1~1,TENHUK11==2~2,TENHUK11==3~4,TENHUK11==4~4))

#The disability question may understate disabilities as measured in UKB. Also assign disability when individuals report ``bad health''
NewCensusData$disability[NewCensusData$HealthSelfReport==1] <- 1
NewScotCensusData$disability[NewScotCensusData$HealthSelfReport==1] <- 1

#NewCensusData$HoursMoreThan30[NewCensusData$HoursMoreThan30==-9]<-NA
#NewCensusData$HoursMoreThan30 <- as.numeric(NewCensusData$hours >= 3)  
NewCensusData$ReportsPoorHealth <- as.numeric(NewCensusData$HealthSelfReport == 1)
NewScotCensusData$ReportsPoorHealth <- as.numeric(NewScotCensusData$HealthSelfReport == 1)


table(NewCensusData$HealthSelfReport)
table(NewScotCensusData$HealthSelfReport)

table(NewCensusData$disability)/NROW(NewCensusData)*100
table(NewScotCensusData$disability)/NROW(NewScotCensusData)*100

#table(NewCensusData$everwork)

table(NewCensusData$HealthSelfReport)/NROW(NewCensusData)*100
table(NewScotCensusData$HealthSelfReport)/NROW(NewScotCensusData)*100
#table(NewCensusData$HoursMoreThan30)

table(NewCensusData$cobg)/NROW(NewCensusData)*100
table(NewScotCensusData$COB)/NROW(NewScotCensusData)*100

table(NewCensusData$Empstat)/NROW(NewCensusData)*100
table(NewScotCensusData$Empstat)/NROW(NewScotCensusData)*100

table(NewCensusData$Education)/NROW(NewCensusData)*100
table(NewScotCensusData$Education)/NROW(NewScotCensusData)*100

table(NewCensusData$Tenure)/NROW(NewCensusData)*100
table(NewScotCensusData$Tenure)/NROW(NewScotCensusData)*100

table(NewCensusData$carsnoc)/NROW(NewCensusData)*100
table(NewScotCensusData$CARSNO)/NROW(NewScotCensusData)*100
NewScotCensusData$CARSNO[NewScotCensusData$CARSNO==-9]<-NA

table(NewCensusData$birthyear)/NROW(NewCensusData)*100
table(NewScotCensusData$birthyear)/NROW(NewScotCensusData)*100

table(NewCensusData$hours)

table(NewCensusData$ReportsPoorHealth)

summary(as.factor(NewCensusData$Education))

NewCensusData <- mutate(NewCensusData,birthyear=factor(birthyear))
NewCensusData$ukb_selected <- 0
NewScotCensusData$ukb_selected <- 0
summary(as.factor(NewCensusData$Education))

ScotEligibleList<-NewScotCensusData[c("ID","popweight")]
ScotEligibleList$InUKBEligible<-1
EngEligibleList<-NewCensusData[c("ID","popweight")]
EngEligibleList$InUKBEligible<-1

names(NewScotCensusData)[names(NewScotCensusData)=='COUNCIL_AREA_GROUP']<-'region'
names(NewScotCensusData)[names(NewScotCensusData)=='Country.x']<-'Country'
NewCensusData <- NewCensusData[c("sex","Education","region","popweight","birthyear","birthyearcenter","ukb_selected",
                                 "HealthSelfReport","ReportsPoorHealth","Tenure","Empstat","BirthCountry","carsnoc","country","YearsEducation","SingleHousehold","Ethnicity","ethnicityew")]
NewScotCensusData <- NewScotCensusData[c("SEX","Education","region","popweight","birthyear","birthyearcenter","ukb_selected",
                                 "HealthSelfReport","ReportsPoorHealth","Tenure","Empstat","BirthCountry","CARSNO","Country","YearsEducation","SingleHousehold","Ethnicity","ETHNIC")]

names(NewScotCensusData)[names(NewScotCensusData) == 'CARSNO'] <- 'carsnoc'
names(NewScotCensusData)[names(NewScotCensusData) == 'SEX'] <- 'sex'
names(NewScotCensusData)[names(NewScotCensusData) == 'Country'] <- 'country'
NewScotCensusData$DataIndex<-2000
NewCensusData$DataIndex<-1000

EngDataAll<-NewCensusData[names(NewCensusData)!="ethnicityew"]
ScotDataAll<-NewScotCensusData[names(NewScotCensusData)!="ETHNIC"]

CensusDataAll<-rbind(EngDataAll,ScotDataAll)

CensusDataAll$Education <- cut(CensusDataAll$YearsEducation, 
                               breaks=c(0,8.5,11,17.5,20),
                               labels=c("1","2","3","4"))
table(CensusDataAll$Education)/sum(table(CensusDataAll$Education))

CensusDataAll$region<-as.factor(CensusDataAll$DataIndex+CensusDataAll$region)
table(CensusDataAll$region)

NROW(which(is.na(CensusDataAll)))

NROW(which(is.na(CensusDataAll)))/NROW(CensusDataAll)*100

write.csv(CensusDataAll, "../DATA/CLEAN/CensusDataForProbit.csv",row.names=FALSE)
EngCensusData<-merge(EngCensusData,EngEligibleList,all.x=TRUE,all.y=FALSE)

EngCensusData$InUKBEligible[is.na(EngCensusData$InUKBEligible)]<-0
EngCensusData$popweight[is.na(EngCensusData$popweight)]<-0
summary(EngCensusData$InUKBEligible)
EngCensusData$InUKBEligible[EngCensusData$InUKBEligible==1]<-
    EngCensusData$InUKBEligible[EngCensusData$InUKBEligible==1]*(EngCensusData$popweight[EngCensusData$InUKBEligible==1]/20)
summary(EngCensusData$InUKBEligible)
EngCensusData$InUKBEligibleDraw<-rbinom(NROW(EngCensusData),1,EngCensusData$InUKBEligible)

EngCensusData$InUKBEligible<-EngCensusData$InUKBEligibleDraw
summary(EngCensusData$InUKBEligible)

ScotCensusData<-merge(ScotCensusData,ScotEligibleList,all.x=TRUE,all.y=FALSE)
ScotCensusData$InUKBEligible[is.na(ScotCensusData$InUKBEligible)]<-0
ScotCensusData$popweight[is.na(ScotCensusData$popweight)]<-0
summary(ScotCensusData$InUKBEligible)
ScotCensusData$InUKBEligible[ScotCensusData$InUKBEligible==1]<-
  ScotCensusData$InUKBEligible[ScotCensusData$InUKBEligible==1]*(ScotCensusData$popweight[ScotCensusData$InUKBEligible==1]/20)
summary(ScotCensusData$InUKBEligible)
ScotCensusData$InUKBEligibleDraw<-rbinom(NROW(ScotCensusData),1,ScotCensusData$InUKBEligible)


EngCensusData <- mutate(EngCensusData,AGE=case_when(ageh==9 ~ 42,
                                                                ageh==10 ~ 47, ageh==11 ~ 52, 
                                                                ageh==12 ~ 57, ageh==13 ~ 62, ageh==14 ~ 67
                                                                , ageh==15 ~ 72))

ScotCensusData <- mutate(ScotCensusData,AGE=case_when(AGE==9 ~ 42,
                                                    AGE==10 ~ 47, AGE==11 ~ 52, 
                                                    AGE==12 ~ 57, AGE==13 ~ 62, AGE==14 ~ 67
                                                    , AGE==15 ~ 72))

EngCensusData$BornOutsideUK<-as.numeric(EngCensusData$cobg>=5)
EngCensusData$BornOutsideUK[EngCensusData$cobg==-9]<-NA
ScotCensusData$BornOutsideUK<-as.numeric(ScotCensusData$COB>=5)
ScotCensusData$BornOutsideUK[ScotCensusData$COB==-9]<-NA

EngCensusData$DISABILITY<-as.numeric(EngCensusData$disability<=2)
ScotCensusData$DISABILITY<-as.numeric(ScotCensusData$DISABILITY<=2)

EngCensusData$Employed<-as.numeric(EngCensusData$ecopuk11<=6|EngCensusData$ecopuk11==8)
EngCensusData$Retired<-as.numeric(EngCensusData$ecopuk11==10)
EngCensusData$Unemployed<-as.numeric(EngCensusData$ecopuk11==7|EngCensusData$ecopuk11==9)

ScotCensusData$Employed<-as.numeric(ScotCensusData$ECOPUK11<=6|ScotCensusData$ECOPUK11==8)
ScotCensusData$Retired<-as.numeric(ScotCensusData$ECOPUK11==10)
ScotCensusData$Unemployed<-as.numeric(ScotCensusData$ECOPUK11==7|ScotCensusData$ECOPUK11==9)


EngCensusData$White<-as.numeric(EngCensusData$ethnicityew<=3)
ScotCensusData$White<-as.numeric(ScotCensusData$ETHNIC<=6)

EngCensusData$Everwork<-as.numeric(EngCensusData$everwork==1)
ScotCensusData$Everwork<-as.numeric(ScotCensusData$EVERWORK==1)

EngCensusData$Health<--EngCensusData$health+6
ScotCensusData$Health<--ScotCensusData$HEALTH+6

EngCensusData$LangPRF<--EngCensusData$langprf+5
ScotCensusData$LangPRF<--ScotCensusData$LANGPRF+5

EngCensusData$LivingInCouple<-as.numeric(EngCensusData$larpuk11<=3)
ScotCensusData$LivingInCouple<-as.numeric(ScotCensusData$LARPUK11<=3)

EngCensusData$NoReligion<-as.numeric(EngCensusData$religionew==1)
ScotCensusData$NoReligion<-as.numeric(ScotCensusData$RELPS11==1)

EngCensusData$Christian<-as.numeric(EngCensusData$religionew==2)
ScotCensusData$Christian<-as.numeric(ScotCensusData$RELPS11==2)

EngCensusData$OtherReligion<-as.numeric(EngCensusData$religionew>=3)
ScotCensusData$OtherReligion<-as.numeric(ScotCensusData$RELPS11>=3)

EngCensusData$OwnsHouse<-as.numeric(EngCensusData$tenure<=1)
ScotCensusData$OwnsHouse<-as.numeric(ScotCensusData$TENHUK11<=2)

EngCensusData$PublicTransport<-as.numeric(EngCensusData$transport>=2&EngCensusData$transport<=4)
EngCensusData$CarOrMotorcycle<-as.numeric(EngCensusData$transport>=5&EngCensusData$transport<=8)
EngCensusData$FlatOrAppartment<-as.numeric(EngCensusData$typaccom==4)

ScotCensusData$PublicTransport<-as.numeric(ScotCensusData$TRANSPORTPS11>=2&ScotCensusData$TRANSPORTPS11<=4)
ScotCensusData$CarOrMotorcycle<-as.numeric(ScotCensusData$TRANSPORTPS11>=5&ScotCensusData$TRANSPORTPS11<=8)
ScotCensusData$FlatOrAppartment<-as.numeric(ScotCensusData$TYPACCOM==4)


EngCensusData <- mutate(EngCensusData,PersonsPerRoom=case_when(pproomhuk11==1~0.25,
                                                               pproomhuk11==2~0.75,
                                                               pproomhuk11==3~1.25,
                                                               pproomhuk11==4~2))

ScotCensusData <- mutate(ScotCensusData,PersonsPerRoom=case_when(PPROOMHUK11==1~0.25,
                                                                 PPROOMHUK11==2~0.75,
                                                                 PPROOMHUK11==3~1.25,
                                                                 PPROOMHUK11==4~2))

EngCensusData <- mutate(EngCensusData,YearsEducation=case_when(hlqupuk11==10 ~ 7,
                                                               hlqupuk11==11 ~ 10, hlqupuk11==12 ~ 10, 
                                                               hlqupuk11==13 ~ 12, hlqupuk11==14 ~ 13, hlqupuk11==15 ~ 20
                                                               , hlqupuk11==16 ~ 15))

ScotCensusData <- mutate(ScotCensusData,YearsEducation=case_when(HLQPS11==20 ~ 7,
                                                                       HLQPS11==21 ~ 10,
                                                                       HLQPS11==22 ~ 13,
                                                                       HLQPS11==23 ~ 19,
                                                                       HLQPS11==24 ~ 20))
ScotCensusData$sex<-ScotCensusData$SEX
ScotCensusData$sizhuk11<-ScotCensusData$SIZHUK11
ScotCensusData$depedhuk11<-ScotCensusData$DEPEDHUK11
ScotCensusData$depemhuk11<-ScotCensusData$DEPEMHUK11
ScotCensusData$dephdhuk11<-ScotCensusData$DEPHDHUK11
ScotCensusData$dephshuk11<-ScotCensusData$DEPHSHUK11
ScotCensusData$deprived<-ScotCensusData$DEPRIVED
ScotCensusData$housecarer<-ScotCensusData$CRSHUK11
ScotCensusData$illhuk11g<-ScotCensusData$ILLHUK11
ScotCensusData$carsnoc<-ScotCensusData$CARSNO

ScotCensusData$sizhuk11[ScotCensusData$sizhuk11==-9]<-NA
ScotCensusData$depedhuk11[ScotCensusData$depedhuk11==-9]<-NA
ScotCensusData$depemhuk11[ScotCensusData$depemhuk11==-9]<-NA
ScotCensusData$dephdhuk11[ScotCensusData$dephdhuk11==-9]<-NA
ScotCensusData$dephshuk11[ScotCensusData$dephshuk11==-9]<-NA
ScotCensusData$deprived[ScotCensusData$deprived==-9]<-NA
ScotCensusData$housecarer[ScotCensusData$housecarer==-9]<-NA
ScotCensusData$illhuk11g[ScotCensusData$illhuk11g==-9]<-NA
ScotCensusData$carsnoc[ScotCensusData$carsnoc==-9]<-NA

categories<-c("Demographic","Demographic","Demographic","Demographic","Demographic","Demographic",
              "Socioeconomic status","Socioeconomic status","Socioeconomic status","Socioeconomic status","Socioeconomic status","Socioeconomic status","Socioeconomic status","Socioeconomic status",
              "Health","Health","Health","Health",
              "Employment","Employment","Employment","Employment",
              "Urbanicity","Urbanicity","Urbanicity","Urbanicity",
              "Religion","Religion","Religion")
vars<-c("AGE","LivingInCouple","BornOutsideUK","White","sex","sizhuk11",
        "depedhuk11","depemhuk11","dephdhuk11","dephshuk11","deprived","YearsEducation","OwnsHouse","carsnoc",
        "Health","DISABILITY","housecarer","illhuk11g",
        "Employed","Retired","Unemployed","Everwork",
        "PersonsPerRoom","PublicTransport","CarOrMotorcycle","FlatOrAppartment",
        "NoReligion","Christian","OtherReligion")
varlabels<-c("Age","Living in a couple","Born outside UK","White","Sex","Household size",
             "Deprived in education dimension","Deprived in employment dimension","Deprived in health and disability dimension","Deprived in housing dimension","Deprivation indicator (total)","Years of education","Owns house","No. of cars",
             "Self-reported health","Disability","Number of housecarers in household","No. in household with illness/disability",
             "Employed","Retired","Unemployed","Ever worked",
             "Persons per room","Goes to work by public transport","Goes to work by car/motorcycle","Lives in flat or appartment",
             "Has no religion","Christian","Other religion")

EngCensusData<-EngCensusData[c("InUKBEligible",vars)]

ScotCensusData<-ScotCensusData[c("InUKBEligible",vars)]

sumdata<-rbind(EngCensusData,ScotCensusData)

g<-c()
for (v in vars){
  x<-t.test(sumdata[,v])$conf.int[1:2]
  y<-t.test(sumdata[,v])$estimate[1]
  m<-c(x,y)
  x2<-t.test(sumdata[sumdata$InUKBEligible==1,v])$conf.int[1:2]
  y2<-t.test(sumdata[sumdata$InUKBEligible==1,v])$estimate[1]
  m2<-c(x2,y2)
  t<-c(m,m2)
  g<-rbind(g,t)
}

g<-cbind(varlabels,g)
g<-cbind(vars,g)
g<-cbind(categories,g)
g<-as.data.frame(g)
colnames(g)<-c("Category","Var","Varlabels","CILow","CIHigh","Mean","CILow2","CIHigh2","Mean2")
#g<-g[order(g$Category),]
total<-c("","Observations","Observations","","",NROW(sumdata),"","",NROW(sumdata[sumdata$InUKBEligible==1,]))
g<-rbind(g,total)

cat<-unique(g$Category)
catindex<-which(!duplicated(g$Category))-1

g$Mean<-as.numeric(g$Mean)
g$CILow<-as.numeric(g$CILow)
g$CIHigh<-as.numeric(g$CIHigh)
g$Mean2<-as.numeric(g$Mean2)
g$CILow2<-as.numeric(g$CILow2)
g$CIHigh2<-as.numeric(g$CIHigh2)

g$Mean<-round(g$Mean,digits=3)
g$Mean2<-round(g$Mean2,digits=3)
g$CILow<-round(g$CILow,digits=3)
g$CIHigh<-round(g$CIHigh,digits=3)
g$CILow2<-round(g$CILow2,digits=3)
g$CIHigh2<-round(g$CIHigh2,digits=3)

g$Mean[g$Var!="Observations"]<-paste0("\\textbf{",g$Mean[g$Var!="Observations"],"} [",g$CILow[g$Var!="Observations"],";",g$CIHigh[g$Var!="Observations"],"]")
g$Mean2[g$Var!="Observations"]<-paste0("\\textbf{",g$Mean2[g$Var!="Observations"],"} [",g$CILow2[g$Var!="Observations"],"; ",g$CIHigh2[g$Var!="Observations"],"]")

g<-g[c("Varlabels","Mean","Mean2")]
# \\% change after weighting each mean calculated as the absolute value of the weighted mean, minus the unweighted mean, divided by the unweighted mean.
SumTab<-xtable(g,align = "ll|l|l", caption="\\textbf{Comparison of the mean of various variables in the full population of Great Britain (40-74 year olds) 
               and the UKB-eligible population (40-74) year olds as estimated in the UK Census data.} 95\\% confidence intervals around each mean included. For Census respondents
               in certain regions, it is uncertain whether they received a UKB invitation (that is, whether they were part of the UKB-eligible population) or not. This is because the regions
               that these respondents lived in were only partially sampled by the UKB. We assign such respondents probabilistically to the UKB-eligible population or not. These probabilities are based on
               the proportion of the region's population that fell within a sampling radius of one of the 22 UKB assessment centres.")
addtorow<-list()
addtorow$pos<-as.list(catindex)
cat<-paste0("\\hline \\textit{",cat,"} \\\\ \\hline ")
addtorow$command<-as.vector(cat)

colnames(SumTab) = c("\\textbf{Variable}", "\\textbf{Mean in full population [95\\% CI]}", "\\textbf{Mean in UKB-eligible population [95\\% CI]}")
print(SumTab,include.rownames=FALSE,add.to.row=addtorow,sanitize.colnames.function = paste0,
      sanitize.text.function= paste0,type="latex",file="../OUTPUT/TABLES/MeanGB_UKBEligible.tex",floating=FALSE,size="scriptsize",tabular.environment="longtable")

summary(EngCensusData$AGE)
summary(EngCensusData$AGE[EngCensusData$InUKBEligible==1])

summary(EngCensusData$carsnoc)
summary(EngCensusData$carsnoc[EngCensusData$InUKBEligible==1])

summary(EngCensusData$BornOutsideUK)
summary(EngCensusData$BornOutsideUK[EngCensusData$InUKBEligible==1])

summary(EngCensusData$depedhuk11)
summary(EngCensusData$depedhuk11[EngCensusData$InUKBEligible==1])

summary(EngCensusData$depemhuk11)
summary(EngCensusData$depemhuk11[EngCensusData$InUKBEligible==1])

summary(EngCensusData$dephdhuk11)
summary(EngCensusData$dephdhuk11[EngCensusData$InUKBEligible==1])

summary(EngCensusData$dephshuk11)
summary(EngCensusData$dephshuk11[EngCensusData$InUKBEligible==1])

summary(EngCensusData$deprived)
summary(EngCensusData$deprived[EngCensusData$InUKBEligible==1])

summary(EngCensusData$DISABILITY)
summary(EngCensusData$DISABILITY[EngCensusData$InUKBEligible==1])

summary(EngCensusData$Employed)
summary(EngCensusData$Employed[EngCensusData$InUKBEligible==1])

summary(EngCensusData$Retired)
summary(EngCensusData$Retired[EngCensusData$InUKBEligible==1])

summary(EngCensusData$Unemployed)
summary(EngCensusData$Unemployed[EngCensusData$InUKBEligible==1])

summary(EngCensusData$White)
summary(EngCensusData$White[EngCensusData$InUKBEligible==1])

summary(EngCensusData$Everwork)
summary(EngCensusData$Everwork[EngCensusData$InUKBEligible==1])

summary(EngCensusData$Health)
summary(EngCensusData$Health[EngCensusData$InUKBEligible==1])

summary(EngCensusData$YearsEducation)
summary(EngCensusData$YearsEducation[EngCensusData$InUKBEligible==1])

summary(EngCensusData$housecarer)
summary(EngCensusData$housecarer[EngCensusData$InUKBEligible==1])

summary(EngCensusData$illhuk11g)
summary(EngCensusData$illhuk11g[EngCensusData$InUKBEligible==1])

#INDGPUK11 Industries
#ISCOG 2 digit occ. codes
#nssec

summary(EngCensusData$LivingInCouple)
summary(EngCensusData$LivingInCouple[EngCensusData$InUKBEligible==1])

summary(EngCensusData$PersonsPerRoom)
summary(EngCensusData$PersonsPerRoom[EngCensusData$InUKBEligible==1])

summary(EngCensusData$NoReligion)
summary(EngCensusData$NoReligion[EngCensusData$InUKBEligible==1])

summary(EngCensusData$Christian)
summary(EngCensusData$Christian[EngCensusData$InUKBEligible==1])

summary(EngCensusData$OtherReligion)
summary(EngCensusData$OtherReligion[EngCensusData$InUKBEligible==1])

summary(EngCensusData$sex)
summary(EngCensusData$sex[EngCensusData$InUKBEligible==1])

summary(EngCensusData$sizhuk)
summary(EngCensusData$sizhuk[EngCensusData$InUKBEligible==1])

summary(EngCensusData$OwnsHouse)
summary(EngCensusData$OwnsHouse[EngCensusData$InUKBEligible==1])

summary(EngCensusData$PublicTransport)
summary(EngCensusData$PublicTransport[EngCensusData$InUKBEligible==1])

summary(EngCensusData$CarOrMotorcycle)
summary(EngCensusData$CarOrMotorcycle[EngCensusData$InUKBEligible==1])

summary(EngCensusData$FlatOrAppartment)
summary(EngCensusData$FlatOrAppartment[EngCensusData$InUKBEligible==1])

