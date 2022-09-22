args <- commandArgs(trailingOnly = TRUE)

#rm(list=ls())

library(plyr)
library(dplyr)
library(stargazer)
library(Hmisc)
library(reporttools)
library(data.table)
options(bitmapType="cairo")

UKBPath<-as.character(args[1])

UKB <- fread(UKBPath,header=TRUE,sep="\t")
UKB<-as.data.frame(UKB)

#This data frame stores the year of birth cutoffs that are implied by the sampling scheme of each assessment centre.
CentreBirthYearInfo<-read.table("FILES/UKBCentreBirthYearInfo.csv",header=TRUE,sep=",")
names(CentreBirthYearInfo)[names(CentreBirthYearInfo) == 'clinicID'] <- 'AssessmentCentre'

#Make sure that these are the variables that are loaded in the UKB dataframe:
UKBCols<-c("f.eid","f.31.0.0","f.34.0.0","f.54.0.0","f.20074.0.0","f.20075.0.0","f.709.0.0","f.6138.0.0","f.6138.0.1","f.6138.0.2","f.6138.0.3","f.6138.0.4","f.6138.0.5",
           "f.2178.0.0","f.6142.0.0","f.680.0.0","f.728.0.0","f.21000.0.0","f.40000.0.0","f.845.0.0")

UKBLabels<-c("f.eid",'sex','birthyear_detail','AssessmentCentre','EastCoordinate','NorthCoordinate','NoInHH','Degree0','Degree1','Degree2',
             'Degree3','Degree4','Degree5','HealthSelfReport','Empstat','Tenure','carsnoc','Ethnicity','DeathDate','AgeCompletedEduc')
UKB<-UKB[UKBCols]
names(UKB)<-UKBLabels

#Recode ethnicity to reflect the same values as in the UKB-UK Census model. Combine Asian and Chinese: 
UKB$Ethnicity[UKB$Ethnicity>=1000&!is.na(UKB$Ethnicity)]<-
  round(UKB$Ethnicity[UKB$Ethnicity>=1000&!is.na(UKB$Ethnicity)]/1000)
UKB$Ethnicity[UKB$Ethnicity==5]<-3 #Combine Asian and Chinese
UKB$Ethnicity[UKB$Ethnicity==6]<-5 
UKB$Ethnicity[UKB$Ethnicity<=0]<-NA

summary(UKB$Ethnicity)

#Drop those with relevant missing variables
UKB<-UKB[!is.na(UKB$sex),]
UKB<-UKB[!is.na(UKB$EastCoordinate),]
UKB<-UKB[!is.na(UKB$NorthCoordinate),]
UKB<-UKB[!is.na(UKB$Ethnicity),]

#Drop those born before Census Day
NROW(UKB)
UKB$DiedBeforeCDay<-0
UKB$DiedBeforeCDay[UKB$DeathDate<="2011-03-26"]<-1

#+Code years education as typical time it takes to obtain the highest reported degree
#+ (The coding is similar to the EA4 GWAS, but defines years of education of getting a professional degree
#+ as age of completed education - 5, with a maximum of 15 years)
UKB$AgeCompletedEduc[UKB$AgeCompletedEduc<0]<-NA


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


UKB$YearsEducation0<-YearsEducFun(UKB$Degree0,UKB$"AgeCompletedEduc")
UKB$YearsEducation1<-YearsEducFun(UKB$Degree1,UKB$"AgeCompletedEduc")
UKB$YearsEducation2<-YearsEducFun(UKB$Degree2,UKB$"AgeCompletedEduc")
UKB$YearsEducation3<-YearsEducFun(UKB$Degree3,UKB$"AgeCompletedEduc")
UKB$YearsEducation4<-YearsEducFun(UKB$Degree4,UKB$"AgeCompletedEduc")
UKB$YearsEducation5<-YearsEducFun(UKB$Degree5,UKB$"AgeCompletedEduc")

UKB$YearsEducation <- apply(UKB[,c("YearsEducation0","YearsEducation1","YearsEducation2","YearsEducation3","YearsEducation4","YearsEducation5")], 1, function(x) max(x,na.rm=TRUE))
UKB$YearsEducation[UKB$YearsEducation==-Inf]<-NA

summary(UKB$YearsEducation)

#Discretize education to ensure comparability with the UKB-UK Census selection model:
UKB$Education <- cut(UKB$YearsEducation, 
                          breaks=c(0,8.5,11,17.5,20),
                          labels=c("1","2","3","4"))

UKB$Education1<-NULL
UKB$Education2<-NULL
UKB$Education3<-NULL
UKB$Education4<-NULL
UKB$Education5<-NULL

UKB$YearsEducation1<-NULL
UKB$YearsEducation2<-NULL
UKB$YearsEducation3<-NULL
UKB$YearsEducation4<-NULL
UKB$YearsEducation5<-NULL

UKB<-UKB[UKB$birthyear_detail>2,]
summary(UKB$birthyear_detail)

UKB<-merge(UKB,CentreBirthYearInfo,by="AssessmentCentre",all.x=TRUE)

N1<-NROW(UKB)
UKB<-UKB[UKB$birthyear_detail>=UKB$BirthyearMin,]
UKB<-UKB[UKB$birthyear_detail<=UKB$BirthyearMax,]
N2<- NROW(UKB)
summary(UKB$birthyear_detail)
BYDrop<-N1-N2

print(paste("We are dropping",BYDrop,"due to missing birthyear/birthyear restrictions, there are",NROW(UKB),"observations left"))


UKB <- mutate(UKB,birthyear=case_when(birthyear_detail > 1965 & birthyear_detail < 1971 ~ "1966-1970",
                                                birthyear_detail > 1960 & birthyear_detail < 1966 ~ "1961-1965", 
                                                birthyear_detail > 1955 & birthyear_detail < 1961 ~ "1956-1960", 
                                                birthyear_detail > 1950 & birthyear_detail < 1956 ~ "1951-1955", 
                                                birthyear_detail > 1945 & birthyear_detail < 1951 ~ "1946-1950", 
                                                birthyear_detail > 1940 & birthyear_detail < 1946 ~ "1941-1945", 
                                                birthyear_detail > 1935 & birthyear_detail < 1941 ~ "1936-1940"))
UKB <- mutate(UKB,birthyearcenter=case_when(birthyear_detail > 1965 & birthyear_detail < 1971 ~ 1968,
                                                      birthyear_detail > 1960 & birthyear_detail < 1966 ~ 1963, 
                                                      birthyear_detail > 1955 & birthyear_detail < 1961 ~ 1958, 
                                                      birthyear_detail > 1950 & birthyear_detail < 1956 ~ 1953, 
                                                      birthyear_detail > 1945 & birthyear_detail < 1951 ~ 1948, 
                                                      birthyear_detail > 1940 & birthyear_detail < 1946 ~ 1943,
                                                      birthyear_detail > 1935 & birthyear_detail < 1941 ~ 1938))
UKB$SingleHousehold<-NA
UKB$SingleHousehold[UKB$NoInHH==1]<-1
UKB$SingleHousehold[UKB$NoInHH>1]<-0

UKB$carsnoc[UKB$carsnoc==-1|UKB$carsnoc==-3]<-NA
UKB$HealthSelfReport[UKB$HealthSelfReport==-1|UKB$HealthSelfReport==-3]<-NA
UKB <- mutate(UKB,HealthSelfReport=case_when(HealthSelfReport==4~1,
                                                       HealthSelfReport==3~2,
                                                       HealthSelfReport==2~3,
                                                       HealthSelfReport==1~3))

UKB$carsnoc <- UKB$carsnoc-1
UKB$sex[UKB$sex==0] <- 2


UKB <- mutate(UKB,Tenure=case_when(Tenure==1~1,Tenure==2~2,Tenure==3~4,Tenure==4~4,Tenure==5~3,Tenure==6~5))
UKB$Empstat[UKB$Empstat==6] <- 2
UKB$Empstat[UKB$Empstat==-7|UKB$Empstat==-3] <- NA

summary(UKB)


UKB <- UKB[c("f.eid","sex","birthyear","birthyearcenter","EastCoordinate","NorthCoordinate","Education","HealthSelfReport",
             "Tenure","Empstat","carsnoc","AssessmentCentre","SingleHousehold","Ethnicity")]

write.csv(UKB, "TEMP/UKBTemp.csv",row.names=FALSE)

