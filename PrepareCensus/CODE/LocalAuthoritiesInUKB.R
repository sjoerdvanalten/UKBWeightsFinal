##Date: 06/10/2020

##Author: Sjoerd van Alten

##Goal: Create a datafile that contains information on which local authorities were eligible for UKB sampling
#Note: Where possible, groupings of local authorities are based on 1991 geographies and groupings


library(sf) #package to handle geographic dataa
library(ggplot2)
library(ggrepel)
library(purrr) #for map_dbl() function
library(data.table)
library(tidyverse)
library(testit)

options(bitmapType="cairo")

#Read in the LA definitions as given by the UK Census
CensusGroupData <-
  read.table(
    "../DATA/RAW/LACodesToLANames.csv",
    header = TRUE,
    sep = ",",
    quote = "",
    stringsAsFactors = FALSE
  )
CensusGroupData[CensusGroupData$Local.Authority.Name == "Isles of Scilly", 2] = "Cornwall"
CensusGroupData[CensusGroupData$Local.Authority.Name == "Ellesmere Port & Neston", 2] = "Ellesmere Port and Neston"
CensusGroupData[CensusGroupData$Local.Authority.Name == "Crewe & Nantwich", 2] = "Crewe and Nantwich"
CensusGroupData[CensusGroupData$Local.Authority.Name == "City of London", 2] = "Westminster"

ScotlandCensusGroupData <-
  read.table(
    "../DATA/RAW/ScotlandLACodesToLANames.csv",
    header = TRUE,
    sep = ",",
    quote = "",
    stringsAsFactors = FALSE
  )
ScotlandCensusGroupData[ScotlandCensusGroupData[, 2] == "Edinburgh", 2] = "Edinburgh, City of"
ScotlandCensusGroupData <- ScotlandCensusGroupData[c(5, 2, 3)]
colnames(ScotlandCensusGroupData) <- colnames(CensusGroupData)

#Merge 1991 LADs
LAGeoData1991 <- read_sf("../DATA/RAW/LADMap1991/england_dt_1991.shp")
WalesGeo1991 <- read_sf("../DATA/RAW/LADMap1991/wales_dt_1991.shp")
LAGeoData1991 <- rbind(LAGeoData1991, WalesGeo1991)
LAGeoData1991 <- LAGeoData1991[c("name", "geometry")]

MergeData1991 <-
  merge(CensusGroupData[(CensusGroupData$Geography == "1991 Local Authority District"), ],
        LAGeoData1991,
        by.x = "Local.Authority.Name",
        by.y = "name")


#Some Geometries in MergeData1991 are dispersed, merge them into one geometry:
Alter1991 <-
  data.frame(
    "Local.Authority.Name" = c("Alnwick", "Berwick-upon-Tweed", "Penwith"),
    "LA.Group.Number1" = c(rep(48, 3)),
    "Geography" = c(rep("1991 Local Authority District", 3))
  )

Alter1991$geometry <-
  c(
    st_union(MergeData1991$geometry[MergeData1991$"Local.Authority.Name" == "Alnwick"])[1]
    ,
    st_union(MergeData1991$geometry[MergeData1991$"Local.Authority.Name" ==
                                      "Berwick-upon-Tweed"])[1],
    st_union(MergeData1991$geometry[MergeData1991$"Local.Authority.Name" ==
                                      "Penwith"])[1]
  )

Alter1991 <- st_sf(Alter1991)

MergeData1991 <-
  MergeData1991[MergeData1991$"Local.Authority.Name" %in% c("Alnwick", "Berwick-upon-Tweed", "Penwith") == FALSE, ]
##Dataframes to sf, so spatial data can be read:
MergeData1991 <- st_sf(MergeData1991)
Alter1991 <- st_sf(Alter1991)

MergeData1991 <- rbind(MergeData1991, Alter1991)


#next, merge the 2011 LADs
LAGeoData2011 <- st_read("../DATA/RAW/CensusLAGeo.geojson")
LAGeoData2011 <- st_transform(LAGeoData2011, 27700)
colnames(LAGeoData2011)[colnames(LAGeoData2011) == "cmlad11nm"] <-
  'name'

#merging will be done by name, so make sure that all names of counties are the same in both
#+ tables. Start with getting rid of everything after the comma in LAGeoData (e.g. "Bristol,
#+ city of" etc.)
LAGeoData2011$name <- gsub("(.*),.*", "\\1", LAGeoData2011$name)
LAGeoData2011 <- LAGeoData2011[c("name", "geometry")]

MergeData2011 <-
  merge(CensusGroupData[(CensusGroupData$Geography == "2011 Local Authority"), ],
        LAGeoData2011,
        by.x = "Local.Authority.Name" ,
        by.y = "name")
##MergeData to sf, so spatial data can be read:
MergeData2011 <- st_sf(MergeData2011)

MergeData <- rbind(MergeData1991, MergeData2011)
#FalseMatch must be empty
FalseMatch <-
  CensusGroupData[CensusGroupData$Local.Authority.Name %in% MergeData$Local.Authority.Name == FALSE, ]

assert(NROW(FalseMatch)==0)

MergeData <- st_sf(MergeData)
MergeData <- st_transform(MergeData, 27700)
MergeData <-
  MergeData %>% group_by(LA.Group.Number1) %>% summarise(count = n()) #Collapse ScotlandGeo data by grouped council area
MergeData <- MergeData[c("LA.Group.Number1", "geometry")]
MergeData <- st_sf(MergeData)
MergeData <- st_transform(MergeData, 27700)

#Next, merge Scotland geo
ScotlandGeo2001 <-
  read_sf("../DATA/RAW/ScotlandCAMap2001/scotland_ca_2001.shp")
ScotlandGeo2001 <- st_transform(ScotlandGeo2001, 27700)
ScotlandGeo2001 <-
  ScotlandGeo2001 %>% group_by(name) %>% summarise(count = n()) #Collapse ScotlandGeo data by Council area
ScotlandGeo2001 <- ScotlandGeo2001[c("name", "geometry")]

MergeScotland <- merge(ScotlandCensusGroupData,
                       ScotlandGeo2001,
                       by.x = "Local.Authority.Name",
                       by.y = "name")
MergeScotland <- st_sf(MergeScotland)
MergeScotland <- st_transform(MergeScotland, 27700)
MergeScotland <-
  MergeScotland %>% group_by(LA.Group.Number1) %>% summarise(count = n()) #Collapse ScotlandGeo data by grouped council area
MergeScotland <- MergeScotland[c("LA.Group.Number1", "geometry")]
MergeScotland <- st_sf(MergeScotland)
MergeScotland <- st_transform(MergeScotland, 27700)

MergeScotland$Country <- "Scotland"
MergeData$Country <- "EnglandOrWales"

MergeData <- rbind(MergeData, MergeScotland)
MergeData <- st_sf(MergeData)

##define a new geometry that draws circles amongst the UKB assessment centers:
##crs=27700 converts UK coordinates to meter units.
MergeData <- st_transform(MergeData, 27700)

rm(Alter1991)
rm(LAGeoData1991)
rm(LAGeoData2011)
rm(MergeData1991)
rm(MergeData2011)
rm(MergeScotland)
rm(ScotlandGeo2001)
rm(WalesGeo1991)

UKB_sf <- st_read("../DATA/TEMP/UKB_Assessment_geography.geojson")

#Run the file UKBGeometry in Cartesius here on stacked UKB/UKCensus data to obtain empirically estimated AssessmentDistance cutoffs

AssessmentDistanceCutoff <-
  read.table("../DATA/RAW/UKBAssesmentDistanceCutoff.csv",
             header = TRUE,
             sep = ",")
AssessmentDistanceCutoff$name[AssessmentDistanceCutoff$name == "Middlesborough"] <-
  "Middlesbrough"

AssessmentDistanceCutoff$DistanceCutoff <-
  AssessmentDistanceCutoff$DistanceCutoff * 1000

UKB_sf <- merge(UKB_sf, AssessmentDistanceCutoff)

UKB_circles <- st_buffer(UKB_sf, dist = UKB_sf$DistanceCutoff)

#+st_intersects for a less conservative approach:
withinlist <- st_intersects(MergeData, UKB_circles)
withinlistlogical = lengths(withinlist) > 0
withinlistlength = lengths(withinlist)
MergeData$UKB_eligible <- withinlistlogical
MergeData$UKBCentres <- withinlist

#Get a list of ALL centres that intersect with a regions geometry.
MergeData$UKBCentre1[withinlistlogical == FALSE] <- NA
MergeData$UKBCentre1[withinlistlogical == TRUE] <-
  lapply(withinlist[withinlistlogical == TRUE], `[[`, 1)
MergeData$UKBCentre1 <- as.integer(MergeData$UKBCentre1)

MergeData$UKBCentre2[withinlistlength < 2] <- NA
MergeData$UKBCentre2[withinlistlength >= 2] <-
  lapply(withinlist[withinlistlength >= 2], `[[`, 2)
MergeData$UKBCentre2 <- as.integer(MergeData$UKBCentre2)

MergeData$UKBCentre3[withinlistlength < 3] <- NA
MergeData$UKBCentre3[withinlistlength >= 3] <-
  lapply(withinlist[withinlistlength >= 3], `[[`, 3)
MergeData$UKBCentre3 <- as.integer(MergeData$UKBCentre3)

MergeData$UKBCentre4[withinlistlength < 4] <- NA
MergeData$UKBCentre4[withinlistlength >= 4] <-
  lapply(withinlist[withinlistlength >= 4], `[[`, 4)
MergeData$UKBCentre4 <- as.integer(MergeData$UKBCentre4)

MergeData$UKBCentre5[withinlistlength < 5] <- NA
MergeData$UKBCentre5[withinlistlength >= 5] <-
  lapply(withinlist[withinlistlength >= 5], `[[`, 5)
MergeData$UKBCentre5 <- as.integer(MergeData$UKBCentre5)

#map the indices into names
UKBIndex <- st_set_geometry(UKB_circles, NULL)
MergeData$NameAssessmentCenter1[withinlistlength >= 1] <-
  UKBIndex[MergeData$UKBCentre1[withinlistlength >= 1], "name"]
MergeData$NameAssessmentCenter2[withinlistlength >= 2] <-
  UKBIndex[MergeData$UKBCentre2[withinlistlength >= 2], "name"]
MergeData$NameAssessmentCenter3[withinlistlength >= 3] <-
  UKBIndex[MergeData$UKBCentre3[withinlistlength >= 3], "name"]
MergeData$NameAssessmentCenter4[withinlistlength >= 4] <-
  UKBIndex[MergeData$UKBCentre4[withinlistlength >= 4], "name"]
MergeData$NameAssessmentCenter5[withinlistlength >= 5] <-
  UKBIndex[MergeData$UKBCentre5[withinlistlength >= 5], "name"]

summary(MergeData$UKB_eligible)

EligibleData <-
  MergeData[c(
    "LA.Group.Number1",
    "NameAssessmentCenter1",
    "NameAssessmentCenter2",
    "NameAssessmentCenter3",
    "NameAssessmentCenter4",
    "NameAssessmentCenter5",
    "Country"
  )]


UKB_sf$coordX <- map_dbl(UKB_sf$geometry, 1)
UKB_sf$coordY <- map_dbl(UKB_sf$geometry, 2)

nudgex <-
  c(
    -400000,
    -400000,
    -400000,
    -200000,
    -200000,
    200000,
    -400000,
    -400000,
    300000,
    400000,
    0,
    -200000,
    600000,
    0,
    -400000,
    700000,
    400000,
    -400000,
    300000,
    0,
    -200000,
    300000
  )
nudgey <- c(-100000,
            0,
            0,
            0,
            0,
            0,
            0,
            200000,
            0,
            0,
            -300000,
            0,
            0,
            100000,
            0,
            0,
            0,
            -500000,
            0,
            0,
            0,
            0)

png(
  "../OUTPUT/FIGURES/AssessmentCenterMapNoWeight.png",
  height = 1980,
  width = 1500,
  bg = "transparent"
)
ggplot() + geom_sf(data = EligibleData, fill = "white") + geom_sf(data = UKB_circles, fill = "black", alpha =
                                                                    0.25) +
  geom_sf(data = UKB_sf) + geom_text_repel(
    aes(x = coordX, y = coordY, label = name),
    data = UKB_sf,
    colour = "black",
    size = 10
  ) + theme(
    legend.title = element_text(size = 50),
    legend.text = element_text(size = 45),
    legend.key.size = unit(1.5, 'cm')
  ) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_rect(fill = "transparent"),
    # bg of the panel
    plot.background = element_blank(),
    # bg of the plot
    panel.grid.major = element_blank(),
    # get rid of major grid
    panel.grid.minor = element_blank(),
    # get rid of minor grid
    legend.background = element_rect(fill = "transparent"),
    # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent")
  )
dev.off()

st_write(EligibleData,
         "../DATA/TEMP/UKBGeometryToCensusAreasTemp.shp",
         append = FALSE)

st_write(UKB_circles,
         "../DATA/TEMP/UKB_circles.shp",
         append = FALSE)

st_write(UKB_sf,
         "../DATA/TEMP/UKB_sf.shp",
         append = FALSE)

source("UKPopMap.R")

rm(list=ls())


nudgex <-
  c(
    -400000,
    -400000,
    -400000,
    -200000,
    -200000,
    200000,
    -400000,
    -400000,
    300000,
    400000,
    0,
    -200000,
    600000,
    0,
    -400000,
    700000,
    400000,
    -400000,
    300000,
    0,
    -200000,
    300000
  )
nudgey <- c(-100000,
            0,
            0,
            0,
            0,
            0,
            0,
            200000,
            0,
            0,
            -300000,
            0,
            0,
            100000,
            0,
            0,
            0,
            -500000,
            0,
            0,
            0,
            0)


EligibleData<-st_read("../DATA/TEMP/UKBGeometryToCensusAreasTemp.shp")

UKB_circles<-st_read("../DATA/TEMP/UKB_circles.shp")
names(UKB_circles)<-c("name","DistanceCutoff","geometry")
##Ear-mark LADs that are within 40km of an assessment centre.
UKBPopEligible <-
  read.table("../DATA/TEMP/FractionPopEligible.csv",
             header = TRUE,
             sep = ",")

UKB_sf<-st_read("../DATA/TEMP/UKB_sf.shp")

#names(UKB_sf)<-c("name","DistanceCutoff","geometry","coordX","coordY")

colnames(EligibleData)[colnames(EligibleData)=="LA_G_N1"]<-"LA.Group.Number1"

EligibleData <-
  merge(
    EligibleData,
    UKBPopEligible,
    by.x = c("LA.Group.Number1", "Country"),
    by.y = c("CensusRegion", "Country"),
    all.x = TRUE
  )


EligibleData$FractionPopEligible[is.na(EligibleData$FractionPopEligible) &
                                   EligibleData$UKB_eligible == FALSE] <- 0
EligibleData$popweight <- EligibleData$FractionPopEligible * 20

png("../OUTPUT/FIGURES/AssessmentCenterMap.png",
    height = 1980,
    width = 1500)
ggplot() + geom_sf(data = EligibleData, aes(fill = popweight)) +
  scale_fill_gradient(low = "white", high = "green") + geom_sf(data = UKB_circles, fill = "black", alpha =
                                                                 0.25) +
  geom_sf(data = UKB_sf) + geom_text_repel(
    aes(x = coordX, y = coordY, label = name),
    data = UKB_sf,
    nudge_x = nudgex,
    nudge_y = nudgey,
    colour = "black",
    size = 10
  ) + theme(
    legend.title = element_text(size = 50),
    legend.text = element_text(size = 45),
    legend.key.size = unit(1.5, 'cm')
  )
dev.off()

st_write(EligibleData,
         "../DATA/CLEAN/UKBGeometryToCensusAreas.shp",
         append = FALSE)

EligibleData <- st_set_geometry(EligibleData, NULL)
#drop geometry from EligibleData:


#Calculate whether Census weights should be adjusted based on birthyear (to match the sampling mechanisms of UKB)
CentreBirthyearInfo <-
  read.table("../DATA/TEMP/UKBCentreBirthYearInfo.csv",
             header = TRUE,
             sep = ",")

MergeYear <- select(CentreBirthyearInfo,-"clinicID")
colnames(MergeYear) <- c("NameAssessmentCenter1", "BYMin1", "BYMax1")
names(EligibleData)<-c("LA.Group.Number1","Country","NameAssessmentCenter1","NameAssessmentCenter2",
                       "NameAssessmentCenter3","NameAssessmentCenter4","NameAssessmentCenter5","FractionPopEligible","popweight")
EligibleData <- merge(EligibleData, MergeYear, all.x = TRUE)

MergeYear <- select(CentreBirthyearInfo,-"clinicID")
colnames(MergeYear) <- c("NameAssessmentCenter2", "BYMin2", "BYMax2")
EligibleData <- merge(EligibleData, MergeYear, all.x = TRUE)

MergeYear <- select(CentreBirthyearInfo,-"clinicID")
colnames(MergeYear) <- c("NameAssessmentCenter3", "BYMin3", "BYMax3")
EligibleData <- merge(EligibleData, MergeYear, all.x = TRUE)

MergeYear <- select(CentreBirthyearInfo,-"clinicID")
colnames(MergeYear) <- c("NameAssessmentCenter4", "BYMin4", "BYMax4")
EligibleData <- merge(EligibleData, MergeYear, all.x = TRUE)

MergeYear <- select(CentreBirthyearInfo,-"clinicID")
colnames(MergeYear) <- c("NameAssessmentCenter5", "BYMin5", "BYMax5")
EligibleData <- merge(EligibleData, MergeYear, all.x = TRUE)

EligibleData$BYMinTot <-
  apply(EligibleData[, c("BYMin1", "BYMin2", "BYMin3", "BYMin4", "BYMin5")], 1, min, na.rm =
          TRUE)
EligibleData$BYMaxTot <-
  apply(EligibleData[, c("BYMax1", "BYMax2", "BYMax3", "BYMax4", "BYMax5")], 1, max, na.rm =
          TRUE)

#The weight adjustments are created below. We assume a uniform distribution across birthyear here.
#+ This could be improved upon by taking region-specific data on birthyear distributions in the 2011 Census.
EligibleData <-
  mutate(
    EligibleData,
    MinWtAdjust = case_when(
      BYMinTot == 1936 ~ 1,
      BYMinTot == 1937 ~ 0.8,
      BYMinTot == 1938 ~ 0.6,
      BYMinTot == 1939 ~ 0.4,
      BYMinTot == 1940 ~ 0.2
    )
  )
EligibleData <-
  mutate(
    EligibleData,
    MaxWtAdjust = case_when(
      BYMaxTot == 1970 ~ 1,
      BYMaxTot == 1969 ~ 0.8,
      BYMaxTot == 1968 ~ 0.6,
      BYMaxTot == 1967 ~ 0.4,
      BYMaxTot == 1966 ~ 0.2
    )
  )
#drop geometry from EligibleData:
EligibleData <-
  EligibleData[c("LA.Group.Number1",
                 "popweight",
                 "MinWtAdjust",
                 "MaxWtAdjust",
                 "Country")]

write.csv(EligibleData, "../DATA/TEMP/UKBEligibleGeographic.csv")
