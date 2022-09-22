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

UKBPheno<-read.csv("../TEMP/UKBGeographicEastNorth.csv",header=TRUE)

head(UKBPheno)

UKBPheno <- st_as_sf(UKBPheno,coords = c("EastCoordinate", "NorthCoordinate") , crs=27700)

UKGeography <- read_sf("../INPUT/UKBGeometryToCensusAreas.shp")

UKGeography$Index[UKGeography$Country=="EnglandOrWales"]<-1000
UKGeography$Index[UKGeography$Country=="Scotland"]<-2000
UKGeography$LARegion<-UKGeography$Index+UKGeography$LA_G_N1

names(UKGeography)[names(UKGeography) == 'popwght'] <- 'popweight'

head(UKGeography)

#draw circles around UKB assessment centers:
UKB_sf <- st_read("../INPUT/UKB_Assessment_geography.geojson")

#For each UKB participant, calculate distance to nearest assessment centre:
UKBDistances<-st_distance(UKBPheno,UKB_sf)

#CentreNames in order by CentreID
CentreNames<-c("Stockport","Manchester","Oxford","Cardiff","Glasgow","Edinburgh","Stoke",
               "Reading","Bury","Newcastle","Leeds","Bristol","Barts","Nottingham","Sheffield",
               "Liverpool","Middlesbrough","Hounslow","Croydon","Birmingham","Swansea","Wrexham")

head(UKBPheno)

AssessmentCentreCodes<-UKBPheno %>% st_set_geometry(NULL) %>%
  group_by(AssessmentCentre) %>%
  summarise() 
AssessmentCentreCodes$NameAssessment<-CentreNames

UKBPheno$DistanceNearestAssessment<-apply(UKBDistances,1,min)
UKBPheno$DistanceNearestAssessment<- UKBPheno$DistanceNearestAssessment/1000

head(UKBPheno)
UKBPheno<-inner_join(UKBPheno,AssessmentCentreCodes)
head(UKBPheno)


UKBPheno$NearestAssessment<-apply(UKBDistances,1,which.min)
UKBPheno$NearestAssessment<-UKB_sf$name[UKBPheno$NearestAssessment]


UKBPheno$AssessmentIndex<-lapply(UKBPheno$NameAssessment,function(x) which(x==UKB_sf$name))
UKBPheno$DistanceAssessment<-as.numeric(cbind(lapply(1:NROW(UKBPheno), function(i) UKBDistances[i,UKBPheno$AssessmentIndex[[i]]]))) ####?
UKBPheno$DistanceAssessment<- UKBPheno$DistanceAssessment/1000

NROW(UKBPheno[UKBPheno$DistanceAssessment!=UKBPheno$DistanceNearestAssessment,])

head(UKBPheno)
head(UKBPheno$DistanceNearestAssessment)
summary(UKBPheno$DistanceNearestAssessment)
summary(UKBPheno$DistanceAssessment)

#Drop 73 participants who lived further than 40 km from their nearest assessment centres.
N1<-NROW(UKBPheno)
UKBPheno<- UKBPheno[UKBPheno$DistanceNearestAssessment<=40,]
N2<-NROW(UKBPheno)
print(paste("Virtually all UKB participants resided within 40 km of an assessment centre at the day of assessment.", 
"We drop", N1-N2 ,"UKB participants for whom this was not the case from the dataset."))

#Define the variable sampling distance, which equals the distance to an individual's assessment centre, or if this distance is larger
#+ than 40, the distance to the nearest assessment centre.
UKBPheno$SamplingDistance<-UKBPheno$DistanceAssessment
UKBPheno$SamplingDistance[UKBPheno$DistanceAssessment>=40]<-UKBPheno$DistanceNearestAssessment[UKBPheno$DistanceAssessment>=40]

#Assign 29 individuals who went to Barts to Hounslow as Hounslow is closer for them.

NROW(UKBPheno[UKBPheno$NearestAssessment=="Hounslow"&UKBPheno$NameAssessment=="Barts",])
table(UKBPheno$NameAssessment)
UKBPheno$NameAssessment[UKBPheno$NearestAssessment=="Hounslow"&UKBPheno$NameAssessment=="Barts"]<-"Hounslow"
NROW(UKBPheno[UKBPheno$NearestAssessment=="Hounslow"&UKBPheno$NameAssessment=="Barts",])
table(UKBPheno$NameAssessment)

NROW(UKBPheno[UKBPheno$NearestAssessment=="Barts"&UKBPheno$NameAssessment=="Hounslow",])

NROW(UKBPheno[UKBPheno$NearestAssessment=="Barts"&UKBPheno$NameAssessment=="Croydon",])
NROW(UKBPheno[UKBPheno$NearestAssessment=="Croydon"&UKBPheno$NameAssessment=="Barts",])

UKBPheno$NameAssessment[UKBPheno$NearestAssessment=="Croydon"&UKBPheno$NameAssessment=="Barts"]<-"Croydon"

AssessmentDistanceCutoff<-UKBPheno %>% st_set_geometry(NULL) %>%
  group_by(AssessmentCentre) %>%
  summarise(distance_max=max(SamplingDistance,na.rm=TRUE),
            distance_999pct=quantile(SamplingDistance,.999,na.rm=TRUE),
            distance_998pct=quantile(SamplingDistance,.998,na.rm=TRUE),
            distance_997pct=quantile(SamplingDistance,.997,na.rm=TRUE),
            distance_995pct=quantile(SamplingDistance,.995,na.rm=TRUE),
            distance_95pct=quantile(SamplingDistance,.95,na.rm=TRUE),
            distance_min=min(SamplingDistance,na.rm=TRUE),
            n()) 

AssessmentDistanceCutoff$name<-CentreNames

print(AssessmentDistanceCutoff,n=50,width=Inf)
#Define a cutoff rule for each assessment centre, equal to "distance_max" if "distance_max" and the 99.9th percentile differ by more than 2km, set it equal to the
#99.7th percentile, plus 2km

AssessmentDistanceCutoff$DistanceCutoff<-AssessmentDistanceCutoff$distance_max
AssessmentDistanceCutoff$PctChange<-AssessmentDistanceCutoff$distance_max-AssessmentDistanceCutoff$distance_999pct

AssessmentDistanceCutoff$DistanceCutoff[AssessmentDistanceCutoff$PctChange>=2]<-AssessmentDistanceCutoff$distance_997pct[AssessmentDistanceCutoff$PctChange>=2]+2

AssessmentDistanceCutoff<-AssessmentDistanceCutoff %>% select(AssessmentCentre,name,DistanceCutoff)
AssessmentDistanceCutoffNearest<-AssessmentDistanceCutoff %>% select(name,DistanceCutoff)
AssessmentDistanceCutoffNearest<-rename(AssessmentDistanceCutoffNearest,NameAssessment=name)
AssessmentDistanceCutoffReal<-AssessmentDistanceCutoff %>% select(name,DistanceCutoff)
AssessmentDistanceCutoffReal<-rename(AssessmentDistanceCutoffReal,NearestAssessment=name,DistanceCutoff2=DistanceCutoff)


UKBPheno <- inner_join(UKBPheno,AssessmentDistanceCutoffNearest)
UKBPheno <- inner_join(UKBPheno,AssessmentDistanceCutoffReal)
NROW(UKBPheno[UKBPheno$DistanceCutoff!=UKBPheno$DistanceCutoff2,])

UKBPheno$SamplingRule<-1
UKBPheno$SamplingRule[UKBPheno$SamplingDistance>=UKBPheno$DistanceCutoff]<-2

png("../OUTPUT/FIGURES/SamplingDistanceHisto.png")
hist(UKBPheno$SamplingDistance)
dev.off()

AssessmentDistanceCutoff<-AssessmentDistanceCutoff %>% select(name,DistanceCutoff)

write.csv(AssessmentDistanceCutoff, "../OUTPUT/UKBAssesmentDistanceCutoff.csv",row.names=FALSE)

AssessmentDistanceCutoff$DistanceCutoff<-AssessmentDistanceCutoff$DistanceCutoff*1000
UKB_sf <- merge(UKB_sf,AssessmentDistanceCutoff)

UKB_circles <- st_buffer(UKB_sf, dist = UKB_sf$DistanceCutoff)

UKB_sf$coordX <- map_dbl(UKB_sf$geometry, 1)
UKB_sf$coordY <- map_dbl(UKB_sf$geometry, 2)

nudgex<-c(300000,-400000,-400000,-200000,-200000,300000,400000,-400000,0,
          400000,-300000,-200000,400000,0,600000,700000,0,400000,
          300000,-300000,-200000,-300000)
nudgey<-c(0,0,0,0,0,0,0,0,-100000,
          0,0,0,0,150000,0,0,-200000,0,
          0,0,0,0)

UKBPheno$NameAssessment<-as.factor(UKBPheno$NameAssessment)

UKB_sf$name[UKB_sf$name=="Croydon"] <- "Croydon (London)"
UKB_sf$name[UKB_sf$name=="Hounslow"] <- "Hounslow (London)"
UKB_sf$name[UKB_sf$name=="Barts"] <- "Central London"

png("../OUTPUT/FIGURES/UKBGeographicSampling.png",width = 1080, height = 1080)
ggplot() +   geom_sf(data = UKGeography, lwd=0) + geom_sf(data=UKBPheno, alpha=0.01)+ 
  geom_text_repel(aes(x=coordX,y=coordY,label = name), data = UKB_sf, 
                  nudge_x = nudgex,nudge_y = nudgey, colour = "black", size = 5)  + guides(color = FALSE) + theme_classic() + theme(legend.position = "bottom",
                                                                                                                                    legend.title = element_text(size = 25),
                                                                                                                                    legend.text = element_text(size = 20), 
                                                                                                                                    legend.key.size = unit(1.5,'cm'),
                                                                                                                                    legend.justification = "right",
                                                                                                                                    axis.title.x=element_blank(),
                                                                                                                                    axis.text.x=element_blank(),
                                                                                                                                    axis.ticks.x=element_blank(),
                                                                                                                                    axis.title.y=element_blank(),
                                                                                                                                    axis.text.y=element_blank(),
                                                                                                                                    axis.ticks.y=element_blank())
dev.off()


groupcolors<-colorRampPalette(c(rainbow(8)))(22)
png("../OUTPUT/FIGURES/UKBGeoPlot.png",width = 1080, height = 1080)
ggplot() + 
  geom_sf(data = UKGeography, aes(fill = popweight)) +
  scale_fill_gradient(name = "Census Adjustment Factor",low="white",high="black") + geom_sf(data=UKBPheno, aes(color=NameAssessment), alpha=0.01)+ 
  #scale_color_viridis(discrete = TRUE, option = "C")+
  scale_color_manual(values=groupcolors)+
  geom_sf(data = UKB_circles, alpha=0.1) + geom_sf(data = UKB_sf)+ 
  geom_text_repel(aes(x=coordX,y=coordY,label = name), data = UKB_sf, 
                  nudge_x = nudgex,nudge_y = nudgey, colour = "black", size = 5)  + guides(color = FALSE) + theme_classic() + theme(legend.position = "bottom",
                                                                                                                                    legend.title = element_text(size = 25),
                                                                                                                                    legend.text = element_text(size = 20), 
                                                                                                                                    legend.key.size = unit(1.5,'cm'),
                                                                                                                                    legend.justification = "right",
                                                                                                                                    axis.title.x=element_blank(),
                                                                                                                                    axis.text.x=element_blank(),
                                                                                                                                    axis.ticks.x=element_blank(),
                                                                                                                                    axis.title.y=element_blank(),
                                                                                                                                    axis.text.y=element_blank(),
                                                                                                                                    axis.ticks.y=element_blank())
dev.off()

##Drop individuals who lived too far from the assessment centre:
N1<-NROW(UKBPheno)
table(UKBPheno$NameAssessment[UKBPheno$DistanceAssessment>UKBPheno$DistanceCutoff&UKBPheno$DistanceNearestAssessment>UKBPheno$DistanceCutoff2])
UKBPheno <- UKBPheno[UKBPheno$DistanceAssessment<=UKBPheno$DistanceCutoff|UKBPheno$DistanceNearestAssessment<=UKBPheno$DistanceCutoff2,]
N2<-NROW(UKBPheno)
NumDrop<-N1-N2
NumDrop

##Make the figure again, just to see what it looks like after excluding the above participants.
png("../OUTPUT/FIGURES/UKBGeoPlotAfterExclusion.png",width = 1080, height = 1080)
ggplot() + 
  geom_sf(data = UKGeography, aes(fill = popweight)) +
  scale_fill_gradient(name = "Census Weight",low="white",high="black") + geom_sf(data=UKBPheno, aes(color=NameAssessment), alpha=0.5)+ 
  #scale_color_viridis(discrete = TRUE, option = "C")+
  scale_color_manual(values=groupcolors)+
  geom_sf(data = UKB_circles, alpha=0.1) + geom_sf(data = UKB_sf)+ 
  geom_text_repel(aes(x=coordX,y=coordY,label = name), data = UKB_sf, 
                  nudge_x = nudgex,nudge_y = nudgey, colour = "black", size = 5)  + guides(color = FALSE) + theme_classic() + theme(legend.position = "bottom",
                                                                                                                                    legend.title = element_text(size = 50),
                                                                                                                                    legend.text = element_text(size = 45), 
                                                                                                                                    legend.key.size = unit(1.5,'cm'),
                                                                                                                                    axis.title.x=element_blank(),
                                                                                                                                    axis.text.x=element_blank(),
                                                                                                                                    axis.ticks.x=element_blank(),
                                                                                                                                    axis.title.y=element_blank(),
                                                                                                                                    axis.text.y=element_blank(),
                                                                                                                                    axis.ticks.y=element_blank())
dev.off()

#match east north coordinates to the census regions; 
#UKBPheno$region <- apply(UKBPheno, 1, function(row) {  
#  UKGeography[which(st_intersects(UKBPheno, UKGeography, sparse = FALSE)), ]$Lcl_A_N 
#})
UKGeography<-st_transform(UKGeography,27700)
UKBPheno$region <- NA 
UKGeography = UKGeography[!duplicated(UKGeography$LARegion),]

for (i in 1:NROW(UKGeography)){
  print(i)
  withinlist<-st_intersects(UKBPheno,UKGeography[i,])
  withinlistlogical= lengths(withinlist) > 0
  UKBPheno$region[withinlistlogical==TRUE]<-UKGeography$LARegion[i]
}

mismatch<-UKBPheno[is.na(UKBPheno$region),] #Inspect non-matched coordinates (these are all in the water due to UKB's reporting of coordinates with an error of 1km)
matched<-UKBPheno[!is.na(UKBPheno$region),]
#Use the webiste gridreferencefinder.com to visualize the points on the UK map:
for (i in 1:NROW(mismatch)){
  print(i)
  MatchIndex<-st_nearest_feature(mismatch[i,],matched)
  print(MatchIndex)
  mismatch[i,"region"]<-matched[MatchIndex,"region"]
}

summary(UKBPheno$region)
UKBPheno[is.na(UKBPheno$region),]$region<-mismatch$region
summary(UKBPheno$region)
UKBPheno$region <- as.factor(UKBPheno$region)


#Unstick geometry
UKBPheno<-st_set_geometry(UKBPheno,NULL)
UKGeography<-st_set_geometry(UKGeography,NULL)
NROW(UKBPheno)

UKBPheno_new <- merge(UKBPheno,UKGeography,by.x="region",by.y="LARegion",all.x=TRUE,all.y=FALSE)
NROW(UKBPheno_new)

UKBPheno <- UKBPheno_new
rm(UKBPheno_new)
head(UKBPheno)
UKBPheno$UKB_lgb <- 0 
UKBPheno$UKB_lgb[UKBPheno$popweight>0] <- 1
summary(UKBPheno$UKB_lgb)
UKBPheno$UKB_lgb <- as.logical(UKBPheno$UKB_lgb)
summary(UKBPheno$region)

names(UKBPheno)[names(UKBPheno) == 'UKB_lgb'] <- 'UKB_eligible'
UKBPheno <- UKBPheno[c("f.eid","region","sex","birthyear","birthyearcenter","ukb_selected"
                       ,"popweight","UKB_eligible","Education","YearsEducation","UrbanRural","HealthSelfReport","ReportsPoorHealth",
                       "BirthCountry","Tenure","Empstat","carsnoc","SingleHousehold","Ethnicity")]

print(head(UKBPheno))

write.csv(UKBPheno, "../TEMP/UKBWithRegion.csv",row.names=FALSE)

print(paste("We dropped",NumDrop,"UKB observations because they lived outside their empirically estimated assessment center sampling radius"))
