rm(list=ls())

library(plyr)
library(dplyr)
library(stargazer)
library(Hmisc)
library(reporttools)
library(data.table)
options(bitmapType="cairo")

UKBPheno <- fread("../INPUT/basket2005284.ukb41217_subset_Vtest.R.tab",header=TRUE,sep="\t")
UKBPheno<-as.data.frame(UKBPheno)

#colnames(UKBPheno)
summary(UKBPheno$f.53.0.0) #Median assessment date of january 2009.

names(UKBPheno)[names(UKBPheno) == 'f.31.0.0'] <- 'sex'
names(UKBPheno)[names(UKBPheno) == 'f.34.0.0'] <- 'birthyear_detail'
names(UKBPheno)[names(UKBPheno) == 'f.20074.0.0'] <- 'EastCoordinate'
names(UKBPheno)[names(UKBPheno) == 'f.20075.0.0'] <- 'NorthCoordinate'
names(UKBPheno)[names(UKBPheno) == 'f.129.0.0'] <- 'NorthCoordinateAtBirth'
names(UKBPheno)[names(UKBPheno) == 'f.130.0.0'] <- 'EastCoordinateAtBirth'
names(UKBPheno)[names(UKBPheno) == 'f.6138.0.0'] <- 'Degree0'
names(UKBPheno)[names(UKBPheno) == 'f.6138.0.1'] <- 'Degree1'
names(UKBPheno)[names(UKBPheno) == 'f.6138.0.2'] <- 'Degree2'
names(UKBPheno)[names(UKBPheno) == 'f.6138.0.3'] <- 'Degree3'
names(UKBPheno)[names(UKBPheno) == 'f.6138.0.4'] <- 'Degree4'
names(UKBPheno)[names(UKBPheno) == 'f.6138.0.5'] <- 'Degree5'
names(UKBPheno)[names(UKBPheno) == 'f.2188.0.0'] <- 'Disability'
names(UKBPheno)[names(UKBPheno) == 'f.2178.0.0'] <- 'HealthSelfReport'
names(UKBPheno)[names(UKBPheno) == 'f.20118.0.0'] <- 'UrbanRural'
names(UKBPheno)[names(UKBPheno) == 'f.1647.0.0'] <- 'COBUK'
names(UKBPheno)[names(UKBPheno) == 'f.20115.0.0'] <- 'COBNOUK'
names(UKBPheno)[names(UKBPheno) == 'f.6142.0.0'] <- 'Empstat' 
names(UKBPheno)[names(UKBPheno) == 'f.21022.0.0'] <- 'AgeAtRecruitment' 
names(UKBPheno)[names(UKBPheno) == 'f.680.0.0'] <- 'Tenure'
names(UKBPheno)[names(UKBPheno) == 'f.54.0.0'] <- 'AssessmentCentre'
names(UKBPheno)[names(UKBPheno) == 'f.728.0.0'] <- 'carsnoc'
names(UKBPheno)[names(UKBPheno) == 'f.189.0.0'] <- 'TownsendCont'
names(UKBPheno)[names(UKBPheno) == 'f.40000.0.0'] <- 'DeathDate'
names(UKBPheno)[names(UKBPheno) == 'f.1488.0.0'] <- 'Tea'
names(UKBPheno)[names(UKBPheno) == 'f.1677.0.0'] <- 'Breastfed'
names(UKBPheno)[names(UKBPheno) == 'f.709.0.0'] <- 'NoInHH'
names(UKBPheno)[names(UKBPheno) == 'f.20401.0.0'] <- 'EverAddicted'
names(UKBPheno)[names(UKBPheno) == 'f.20406.0.0'] <- 'EverAddictedAlcohol'
names(UKBPheno)[names(UKBPheno) == 'f.30890.0.0'] <- 'VitaminD'
names(UKBPheno)[names(UKBPheno) == 'f.30680.0.0'] <- 'Calcium'
names(UKBPheno)[names(UKBPheno) == 'f.30690.0.0'] <- 'Cholesterol'
names(UKBPheno)[names(UKBPheno) == 'f.30000.0.0'] <- 'WhiteBloodCellCount'
names(UKBPheno)[names(UKBPheno) == 'f.30010.0.0'] <- 'RedBloodCellCount'
names(UKBPheno)[names(UKBPheno) == 'f.100940.0.0'] <- 'BreadConsumed'
names(UKBPheno)[names(UKBPheno) == 'f.20453.0.0'] <- 'EverCannabis'
names(UKBPheno)[names(UKBPheno) == 'f.20454.0.0'] <- 'MaxFreqCannabis'
names(UKBPheno)[names(UKBPheno) == 'f.2335.0.0'] <- 'ChestPain'
names(UKBPheno)[names(UKBPheno) == 'f.1289.0.0'] <- 'CookedVeg'
names(UKBPheno)[names(UKBPheno) == 'f.1408.0.0'] <- 'CheeseIntake'
names(UKBPheno)[names(UKBPheno) == 'f.1777.0.0'] <- 'PartOfMultipleBirth'
names(UKBPheno)[names(UKBPheno) == 'f.20022.0.0'] <- 'BirthWeight'
names(UKBPheno)[names(UKBPheno) == 'f.1707.0.0'] <- 'Handedness'
names(UKBPheno)[names(UKBPheno) == 'f.1767.0.0'] <- 'AdoptedAsChild'
names(UKBPheno)[names(UKBPheno) == 'f.1787.0.0'] <- 'MaternalSmoking'
names(UKBPheno)[names(UKBPheno) == 'f.845.0.0'] <- 'AgeCompletedEduc'
names(UKBPheno)[names(UKBPheno) == 'f.757.0.0'] <- 'TimeEmployedCurrentJob'
names(UKBPheno)[names(UKBPheno) == 'f.767.0.0'] <- 'LengthOfWorkingWeek'
names(UKBPheno)[names(UKBPheno) == 'f.816.0.0'] <- 'HeavyManualWork'
names(UKBPheno)[names(UKBPheno) == 'f.46.0.0'] <- 'HandGripStrengthLeft'
names(UKBPheno)[names(UKBPheno) == 'f.47.0.0'] <- 'HandGripStrengthRight'
names(UKBPheno)[names(UKBPheno) == 'f.20458.0.0'] <- 'Happiness'
names(UKBPheno)[names(UKBPheno) == 'f.738.0.0'] <- 'HHIncome'
names(UKBPheno)[names(UKBPheno) == 'f.21000.0.0'] <- 'Ethnicity'
names(UKBPheno)[names(UKBPheno) == 'f.20150.0.0'] <- 'FEV1'
names(UKBPheno)[names(UKBPheno) == 'f.20258.0.0'] <- 'FEV1/FVC'
names(UKBPheno)[names(UKBPheno) == 'f.30600.0.0'] <- 'Albumin'
names(UKBPheno)[names(UKBPheno) == 'f.2443.0.0'] <- 'Diabetes'

UKBPheno$Ethnicity[UKBPheno$Ethnicity>=1000&!is.na(UKBPheno$Ethnicity)]<-
  round(UKBPheno$Ethnicity[UKBPheno$Ethnicity>=1000&!is.na(UKBPheno$Ethnicity)]/1000)
UKBPheno$Ethnicity[UKBPheno$Ethnicity==5]<-3 #Combine Asian and Chinese
UKBPheno$Ethnicity[UKBPheno$Ethnicity==6]<-5 
UKBPheno$Ethnicity[UKBPheno$Ethnicity<=0]<-NA

N1<-NROW(UKBPheno)
print(paste("Initial UKB sample size",N1))
UKBPheno<-UKBPheno[!is.na(UKBPheno$sex),]
UKBPheno<-UKBPheno[!is.na(UKBPheno$EastCoordinate),]
UKBPheno<-UKBPheno[!is.na(UKBPheno$NorthCoordinate),]
UKBPheno<-UKBPheno[!is.na(UKBPheno$Ethnicity),]
N2<-NROW(UKBPheno)

print(paste("Number of UKB respondents left after dropping those with missing info on Sex, place of residence",N2))

NROW(UKBPheno)
UKBPheno$DiedBeforeCDay<-0
UKBPheno$DiedBeforeCDay[UKBPheno$DeathDate<="2011-03-26"]<-1

table(UKBPheno$DiedBeforeCDay)

UKBPheno<-UKBPheno[UKBPheno$DiedBeforeCDay==0,]

UKBPheno$DiedBeforeCDay<-NULL

print(paste("Number of UKB respondents left after dropping those who died before Census day", NROW(UKBPheno)))

UKBPheno$Died<-0
UKBPheno$Died[!is.na(UKBPheno$DeathDate)]<-1

UKBPheno$AgeCompletedEduc[UKBPheno$AgeCompletedEduc<0]<-NA

for (var in c("Degree0","Degree1","Degree2","Degree3","Degree4","Degree5")){
UKBPheno$Degree_fact<-as.factor(UKBPheno[,var])
UKBPheno %>% 
  group_by(Degree_fact) %>%
  summarise(age_finished=mean(AgeCompletedEduc,na.rm=TRUE),n()) %>%
  print(n=50)

for (level in c(-7,-3,1,2,3,4,5,6)){
png(paste0("../OUTPUT/FIGURES/EducAge_finishedHisto",level,var,".png"))
hist(UKBPheno$AgeCompletedEduc[UKBPheno[,var]==level],breaks=seq(5,35,1),main="",xlab="Age completed full time education")
dev.off()
}
}

YearsEducFun<-function(EduVar,EduAgeVar){
  tmp<-case_when(EduVar==-7~7,EduVar==3~10,
                 EduVar==4~10,EduVar==5~-10,
                 EduVar==2~13,EduVar==1~20,
                 EduVar==6~-11)
  print(head(tmp))
  tmp[EduVar==5&!is.na(EduVar)]<-EduAgeVar[EduVar==5&!is.na(EduVar)]-5
  tmp[EduVar==5&!is.na(EduVar)&tmp>=19]<-19
  tmp[EduVar==6&!is.na(EduVar)]<-EduAgeVar[EduVar==6&!is.na(EduVar)]-5
  tmp[EduVar==6&!is.na(EduVar)&tmp>=15]<-15
  tmp[tmp<=7]<-7
  return(tmp)
}

UKBPheno$YearsEducation0<-YearsEducFun(UKBPheno$Degree0,UKBPheno$"AgeCompletedEduc")
UKBPheno$YearsEducation1<-YearsEducFun(UKBPheno$Degree1,UKBPheno$"AgeCompletedEduc")
UKBPheno$YearsEducation2<-YearsEducFun(UKBPheno$Degree2,UKBPheno$"AgeCompletedEduc")
UKBPheno$YearsEducation3<-YearsEducFun(UKBPheno$Degree3,UKBPheno$"AgeCompletedEduc")
UKBPheno$YearsEducation4<-YearsEducFun(UKBPheno$Degree4,UKBPheno$"AgeCompletedEduc")
UKBPheno$YearsEducation5<-YearsEducFun(UKBPheno$Degree5,UKBPheno$"AgeCompletedEduc")

UKBPheno$YearsEducation <- apply(UKBPheno[,c("YearsEducation0","YearsEducation1","YearsEducation2","YearsEducation3","YearsEducation4","YearsEducation5")], 1, function(x) max(x,na.rm=TRUE))
UKBPheno$YearsEducation[UKBPheno$YearsEducation==-Inf]<-NA

summary(UKBPheno$YearsEducation)


UKBPheno$Education <- cut(UKBPheno$YearsEducation, 
                               breaks=c(0,8.5,11,17.5,20),
                               labels=c("1","2","3","4"))

summary(as.factor(UKBPheno$Education))
table(UKBPheno$Education)/sum(table(UKBPheno$Education))

UKBPheno$Education1<-NULL
UKBPheno$Education2<-NULL
UKBPheno$Education3<-NULL
UKBPheno$Education4<-NULL
UKBPheno$Education5<-NULL

UKBPheno$YearsEducation1<-NULL
UKBPheno$YearsEducation2<-NULL
UKBPheno$YearsEducation3<-NULL
UKBPheno$YearsEducation4<-NULL
UKBPheno$YearsEducation5<-NULL

UKBPheno$HasADegree<-as.numeric(UKBPheno$Education>1)

UKBPheno <- mutate(UKBPheno,Townsend=case_when(TownsendCont<(-2) ~ 1,
                                               TownsendCont>=-2&TownsendCont<2 ~2,TownsendCont>=2 ~3))


write.csv(UKBPheno[,c("f.eid","Townsend","TownsendCont","AgeAtRecruitment",
                      "Disability","EastCoordinate","NorthCoordinate","Tea","Breastfed","NoInHH",'EverAddicted',
                      'EverAddictedAlcohol','VitaminD','Calcium','Cholesterol','WhiteBloodCellCount','RedBloodCellCount',
                      'BreadConsumed','EverCannabis','MaxFreqCannabis','ChestPain','CookedVeg','CheeseIntake',
                      'PartOfMultipleBirth','BirthWeight','Handedness','AdoptedAsChild','MaternalSmoking','AgeCompletedEduc',
                      'TimeEmployedCurrentJob','LengthOfWorkingWeek','HeavyManualWork','HandGripStrengthLeft',
                      'HandGripStrengthRight','Happiness','HHIncome','Died')],
                      "../TEMP/UKBTownsend.csv",row.names=FALSE)

N1<-NROW(UKBPheno$birthyear_detail)
print(N1)
UKBPheno<-UKBPheno[UKBPheno$birthyear_detail>2,]
NROW(UKBPheno$birthyear_detail)

#Import the implied birthyear distribution per assessment centre. Match with UKBPheno, and exclude participants who were not aged between
#+40 and 69 at the start of their assessment centre's sampling period.
CentreBirthYearInfo<-read.table("../INPUT/UKBCentreBirthYearInfo.csv",header=TRUE,sep=",")
names(CentreBirthYearInfo)[names(CentreBirthYearInfo) == 'clinicID'] <- 'AssessmentCentre'
UKBPheno<-merge(UKBPheno,CentreBirthYearInfo,by="AssessmentCentre",all.x=TRUE)

N1<-NROW(UKBPheno)
UKBPheno<-UKBPheno[UKBPheno$birthyear_detail>=UKBPheno$BirthyearMin,]
NROW(UKBPheno)
UKBPheno<-UKBPheno[UKBPheno$birthyear_detail<=UKBPheno$BirthyearMax,]
N2<- NROW(UKBPheno)
summary(UKBPheno$birthyear_detail)
BYDrop<-N1-N2

print(paste("We are dropping",BYDrop,"due to missing birthyear/birthyear restrictions, there are",NROW(UKBPheno),"observations left"))

NROW(UKBPheno)

UKBPheno <- mutate(UKBPheno,birthyear=case_when(birthyear_detail > 1965 & birthyear_detail < 1971 ~ "1966-1970",
                                                birthyear_detail > 1960 & birthyear_detail < 1966 ~ "1961-1965", 
                                                birthyear_detail > 1955 & birthyear_detail < 1961 ~ "1956-1960", 
                                                birthyear_detail > 1950 & birthyear_detail < 1956 ~ "1951-1955", 
                                                birthyear_detail > 1945 & birthyear_detail < 1951 ~ "1946-1950", 
                                                birthyear_detail > 1940 & birthyear_detail < 1946 ~ "1941-1945", 
                                                birthyear_detail > 1935 & birthyear_detail < 1941 ~ "1936-1940"))
UKBPheno <- mutate(UKBPheno,birthyearcenter=case_when(birthyear_detail > 1965 & birthyear_detail < 1971 ~ 1968,
                                                      birthyear_detail > 1960 & birthyear_detail < 1966 ~ 1963, 
                                                      birthyear_detail > 1955 & birthyear_detail < 1961 ~ 1958, 
                                                      birthyear_detail > 1950 & birthyear_detail < 1956 ~ 1953, 
                                                      birthyear_detail > 1945 & birthyear_detail < 1951 ~ 1948, 
                                                      birthyear_detail > 1940 & birthyear_detail < 1946 ~ 1943,
                                                      birthyear_detail > 1935 & birthyear_detail < 1941 ~ 1938))

UKBPheno$SingleHousehold<-NA
UKBPheno$SingleHousehold[UKBPheno$NoInHH==1]<-1
UKBPheno$SingleHousehold[UKBPheno$NoInHH>1]<-0

UKBPheno$Disability[UKBPheno$Disability==-1|UKBPheno$Disability==-3]<-NA
UKBPheno$carsnoc[UKBPheno$carsnoc==-1|UKBPheno$carsnoc==-3]<-NA
UKBPheno$HealthSelfReport[UKBPheno$HealthSelfReport==-1|UKBPheno$HealthSelfReport==-3]<-NA
UKBPheno <- mutate(UKBPheno,HealthSelfReport=case_when(HealthSelfReport==4~1,
                                                       HealthSelfReport==3~2,
                                                       HealthSelfReport==2~3,
                                                       HealthSelfReport==1~3))

UKBPheno$carsnoc <- UKBPheno$carsnoc-1

UKBPheno$ReportsPoorHealth <- as.numeric(UKBPheno$HealthSelfReport == 1)

#Code a BirthCountry variable: 1 - England, 2 - Scotland, 3 - NI, 4 - Wales, 5 - Ireland, 6 EU countries (as of march 2001), 
##7 EU accession april 2001 to March 2011), ##8 Rest of Europe, ##9 Africa, ##10 Middle East and Asia, ##11 Americas/Caribbean, ##12 Antartica/Oceania/Other
UKBPheno <- mutate(UKBPheno,BirthCountry=case_when(COBUK==1~1,COBUK==2~4,COBUK==3~2,COBUK==4~3,COBUK==5~5))
UKBPheno$BirthCountry[UKBPheno$COBNOUK==304|
                        UKBPheno$COBNOUK==307|
                        UKBPheno$COBNOUK==314|
                        UKBPheno$COBNOUK==317|
                        UKBPheno$COBNOUK==318|
                        UKBPheno$COBNOUK==320|
                        UKBPheno$COBNOUK==310|
                        UKBPheno$COBNOUK==322|
                        UKBPheno$COBNOUK==300|
                        UKBPheno$COBNOUK==346|
                        UKBPheno$COBNOUK==330|
                        UKBPheno$COBNOUK==336|
                        UKBPheno$COBNOUK==337|
                        UKBPheno$COBNOUK==332|
                        UKBPheno$COBNOUK==340|
                        UKBPheno$COBNOUK==349|
                        UKBPheno$COBNOUK==350|
                        UKBPheno$COBNOUK==321|
                        UKBPheno$COBNOUK==325|
                        UKBPheno$COBNOUK==355] <- 6

UKBPheno$BirthCountry[UKBPheno$COBNOUK==309|
                        UKBPheno$COBNOUK==301|
                        UKBPheno$COBNOUK==312|
                        UKBPheno$COBNOUK==313|
                        UKBPheno$COBNOUK==315|
                        UKBPheno$COBNOUK==323|
                        UKBPheno$COBNOUK==328|
                        UKBPheno$COBNOUK==329|
                        UKBPheno$COBNOUK==333|
                        UKBPheno$COBNOUK==339|
                        UKBPheno$COBNOUK==342|
                        UKBPheno$COBNOUK==347|
                        UKBPheno$COBNOUK==348] <- 7

UKBPheno$BirthCountry[UKBPheno$COBNOUK==302|
                        UKBPheno$COBNOUK==303|
                        UKBPheno$COBNOUK==305|
                        UKBPheno$COBNOUK==306|
                        UKBPheno$COBNOUK==308|
                        UKBPheno$COBNOUK==311|
                        UKBPheno$COBNOUK==319|
                        UKBPheno$COBNOUK==324|
                        UKBPheno$COBNOUK==327|
                        UKBPheno$COBNOUK==341|
                        UKBPheno$COBNOUK==334|
                        UKBPheno$COBNOUK==335|
                        UKBPheno$COBNOUK==331|
                        UKBPheno$COBNOUK==338|
                        UKBPheno$COBNOUK==343|
                        UKBPheno$COBNOUK==344|
                        UKBPheno$COBNOUK==345|
                        UKBPheno$COBNOUK==351|
                        UKBPheno$COBNOUK==352|
                        UKBPheno$COBNOUK==353|
                        UKBPheno$COBNOUK==316] <- 8

UKBPheno$BirthCountry[UKBPheno$COBNOUK>100&UKBPheno$COBNOUK<200]<-9
UKBPheno$BirthCountry[UKBPheno$COBNOUK>200&UKBPheno$COBNOUK<300]<-10
UKBPheno$BirthCountry[UKBPheno$COBNOUK>400&UKBPheno$COBNOUK<500|
                        UKBPheno$COBNOUK>600&UKBPheno$COBNOUK<700]<-11
UKBPheno$BirthCountry[UKBPheno$COBNOUK>500&UKBPheno$COBNOUK<600]<-12

summary(UKBPheno$sex)
UKBPheno$sex[UKBPheno$sex==0] <- 2

UKBPheno$ukb_selected <- 1

UKBPheno <- mutate(UKBPheno,Tenure=case_when(Tenure==1~1,Tenure==2~2,Tenure==3~4,Tenure==4~4,Tenure==5~3,Tenure==6~5))
UKBPheno$Empstat[UKBPheno$Empstat==6] <- 2
UKBPheno$Empstat[UKBPheno$Empstat==-7|UKBPheno$Empstat==-3] <- NA

UKBPheno <- UKBPheno[c("f.eid","sex","birthyear","birthyearcenter","ukb_selected"
                       ,"EastCoordinate","NorthCoordinate","Education","HasADegree","UrbanRural","HealthSelfReport","ReportsPoorHealth",
                       "BirthCountry","Tenure","Empstat","carsnoc","AssessmentCentre","YearsEducation","SingleHousehold","Ethnicity")]

head(UKBPheno)

##save data to merge coordinates with Census LA areas in seperate file.
write.csv(UKBPheno, "../TEMP/UKBGeographicEastNorth.csv",row.names=FALSE)

