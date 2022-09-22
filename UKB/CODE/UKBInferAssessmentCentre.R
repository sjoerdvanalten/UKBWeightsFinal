#Goal:
#+Infer UK Census regions (GLA) form UKB place of residence coordinates
#
rm(list=ls())

#In case of ggplot2 error, type: 
options(bitmapType="cairo")
library(plyr)
library(dplyr)
library(sf)
library(ggplot2)
library(ggrepel)
library(purrr)
library(viridis)
library(ggsci)
library(RColorBrewer)

UKB<-read.csv("TEMP/UKBTemp.csv",header=TRUE)

UKB <- st_as_sf(UKB,coords = c("EastCoordinate", "NorthCoordinate") , crs=27700)

UKGeography <- read_sf("FILES/UKBGeometryToCensusAreas.shp")
UKGeography$Index[UKGeography$Country=="EnglandOrWales"]<-1000
UKGeography$Index[UKGeography$Country=="Scotland"]<-2000
UKGeography$LARegion<-UKGeography$Index+UKGeography$LA_G_N1

#The following table shows how far individuals are allowed to live from their nearest assessment
#+centre to be included in the UKB sampling scheme
AssessmentDistanceCutoff<-read.table("FILES/UKBAssesmentDistanceCutoff.csv",header=TRUE,sep=",")

names(UKGeography)[names(UKGeography) == 'popwght'] <- 'popweight' #use the popweight variable to infer whether regions fall within the UKB sampling areas

UKB_sf <- st_read("FILES/UKB_Assessment_geography.geojson") #This file shows the sampling region around each UKB assessment centre. 
#+UKB respondents that live outside the sampling region will be dropped.

#For each UKB participant, calculate distance to nearest assessment centre:
UKBDistances<-st_distance(UKB,UKB_sf)

#CentreNames in order by CentreID
CentreNames<-c("Stockport","Manchester","Oxford","Cardiff","Glasgow","Edinburgh","Stoke",
               "Reading","Bury","Newcastle","Leeds","Bristol","Barts","Nottingham","Sheffield",
               "Liverpool","Middlesbrough","Hounslow","Croydon","Birmingham","Swansea","Wrexham")

AssessmentCentreCodes<-UKB %>% st_set_geometry(NULL) %>%
  group_by(AssessmentCentre) %>%
  summarise() 
AssessmentCentreCodes$NameAssessment<-CentreNames

#Drop respondents that lived further than 40km from any assessment centre.
UKB$DistanceNearestAssessment<-apply(UKBDistances,1,min)
UKB$DistanceNearestAssessment<- UKB$DistanceNearestAssessment/1000

UKB<-inner_join(UKB,AssessmentCentreCodes)
UKB$NearestAssessment<-apply(UKBDistances,1,which.min)
UKB$NearestAssessment<-UKB_sf$name[UKB$NearestAssessment]

head(UKB)
UKB$AssessmentIndex<-lapply(UKB$NameAssessment,function(x) which(x==UKB_sf$name))

head(UKB)
UKB$DistanceAssessment<-as.numeric(cbind(lapply(1:NROW(UKB), function(i) UKBDistances[i,UKB$AssessmentIndex[[i]]])))
UKB$DistanceAssessment<- UKB$DistanceAssessment/1000

#N1<-NROW(UKB)
#UKB<- UKB[UKB$DistanceNearestAssessment<=40,]
#N2<-NROW(UKB)
#print(paste("Virtually all UKB participants resided within 40 km of an assessment centre at the day of assessment.", 
#            "We drop", N1-N2 ,"UKB participants for whom this was not the case from the dataset."))
            

#Define the variable sampling distance, which equals the distance to an individual's assessment centre, or if this distance is larger
#+ than 40, the distance to the nearest assessment centre.
UKB$SamplingDistance<-UKB$DistanceAssessment
#UKB$SamplingDistance[UKB$DistanceAssessment>=40]<-UKB$DistanceNearestAssessment[UKB$DistanceAssessment>=40]


#Assign 29 individuals who went to Barts to Hounslow as Hounslow is closer for them.
NROW(UKB[UKB$NearestAssessment=="Hounslow"&UKB$NameAssessment=="Barts",])
table(UKB$NameAssessment)
UKB$NameAssessment[UKB$NearestAssessment=="Hounslow"&UKB$NameAssessment=="Barts"]<-"Hounslow"
NROW(UKB[UKB$NearestAssessment=="Hounslow"&UKB$NameAssessment=="Barts",])
table(UKB$NameAssessment)

NROW(UKB[UKB$NearestAssessment=="Barts"&UKB$NameAssessment=="Hounslow",])

NROW(UKB[UKB$NearestAssessment=="Barts"&UKB$NameAssessment=="Croydon",])
NROW(UKB[UKB$NearestAssessment=="Croydon"&UKB$NameAssessment=="Barts",])

UKB$NameAssessment[UKB$NearestAssessment=="Croydon"&UKB$NameAssessment=="Barts"]<-"Croydon"

AssessmentDistanceCutoffNearest<-AssessmentDistanceCutoff
AssessmentDistanceCutoffNearest<-rename(AssessmentDistanceCutoffNearest,NameAssessment=name)
AssessmentDistanceCutoffReal<-AssessmentDistanceCutoff %>% select(name,DistanceCutoff)
AssessmentDistanceCutoffReal<-rename(AssessmentDistanceCutoffReal,NearestAssessment=name,DistanceCutoff2=DistanceCutoff)

UKB <- inner_join(UKB,AssessmentDistanceCutoffNearest)
UKB <- inner_join(UKB,AssessmentDistanceCutoffReal)

UKB$SamplingRule<-1
UKB$SamplingRule[UKB$SamplingDistance>=UKB$DistanceCutoff]<-2


head(UKB)

AssessmentDistanceCutoff$DistanceCutoff<-AssessmentDistanceCutoff$DistanceCutoff*1000
UKB_sf <- merge(UKB_sf,AssessmentDistanceCutoff)


head(UKB)

##Drop individuals who lived too far from the assessment centre:
N1<-NROW(UKB)
UKB <- UKB[UKB$DistanceAssessment<=UKB$DistanceCutoff|UKB$DistanceNearestAssessment<=UKB$DistanceCutoff2,]
N2<-NROW(UKB)
NumDrop<-N1-N2
NumDrop

head(UKB)
#match east north coordinates to the census regions, a necessary variable for weight prediction; 
UKGeography<-st_transform(UKGeography,27700)
UKB$region <- NA 
UKGeography = UKGeography[!duplicated(UKGeography$LARegion),]

for (i in 1:NROW(UKGeography)){
  print(i)
  withinlist<-st_intersects(UKB,UKGeography[i,])
  withinlistlogical= lengths(withinlist) > 0
  UKB$region[withinlistlogical==TRUE]<-UKGeography$LARegion[i]
}


mismatch<-UKB[is.na(UKB$region),] #Inspect non-matched coordinates (these are all in the water due to UKB's reporting of coordinates with an error of 1km)
matched<-UKB[!is.na(UKB$region),]
#Use the webiste gridreferencefinder.com to visualize the points on the UK map:
#for (i in 1:NROW(mismatch)){
for (i in 1:NROW(mismatch)){
  print(i)
  MatchIndex<-st_nearest_feature(mismatch[i,],matched)
  print(MatchIndex)
  mismatch[i,"region"]<-matched[MatchIndex,"region"]
}


summary(UKB$region)
UKB[is.na(UKB$region),]$region<-mismatch$region
summary(UKB$region)
UKB$region <- as.factor(UKB$region)

#Unstick geometry
UKB<-st_set_geometry(UKB,NULL)
UKGeography<-st_set_geometry(UKGeography,NULL)
NROW(UKB)


UKB_new <- merge(UKB,UKGeography,by.x="region",by.y="LARegion",all.x=TRUE,all.y=FALSE)
NROW(UKB_new)

UKB <- UKB_new
rm(UKB_new)
head(UKB)
UKB$UKB_lgb <- 0 
UKB$UKB_lgb[UKB$popweight>0] <- 1
summary(UKB$UKB_lgb)
UKB$UKB_lgb <- as.logical(UKB$UKB_lgb)
summary(UKB$region)

names(UKB)[names(UKB) == 'UKB_lgb'] <- 'UKB_eligible'
print(head(UKB))

UKB <- UKB[c("f.eid","region","sex","birthyear","birthyearcenter","UKB_eligible","Education","HealthSelfReport",
                       "Tenure","Empstat","carsnoc","SingleHousehold","Ethnicity")]

#remove regions that should not be included

write.csv(UKB, "TEMP/UKBWithRegionTemp.csv",row.names=FALSE)


