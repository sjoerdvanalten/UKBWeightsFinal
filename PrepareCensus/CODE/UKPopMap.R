#remove all variables from previous history:
rm(list = ls())


options(bitmapType="cairo")
library(readxl)
library(sf) #package to handle geographic dataa
library(ggplot2)

#removes all plots momentarily load in RStudio:
while (!is.null(dev.list()))
  dev.off()

UKGeo2011 <- read_sf("../DATA/RAW/LSOA2011/england_lsoa_2011.shp")
WalesGeo2011 <- read_sf("../DATA/RAW/LSOA2011/wales_lsoa_2011.shp")
ScotlandGeo2011 <-
  read_sf("../DATA/RAW/LSOA2011/scotland_oa_2011.shp") #More detailed oa's used for Scotland (possibly also available for Eng/Wales)
ScotGeoBind <- ScotlandGeo2011[, c("code", "name", "label", "geometry")]

UKGeo2011 <- rbind(UKGeo2011, WalesGeo2011)
UKGeo2011$Country <- "EnglandOrWales"
ScotGeoBind$Country <- "Scotland"

UKGeo2011 <- rbind(UKGeo2011, ScotGeoBind)

CensusGeo <- read_sf("../DATA/TEMP/UKBGeometryToCensusAreasTemp.shp")

UKB_sf <- st_read("../DATA/TEMP/UKB_Assessment_geography.geojson")

rm(WalesGeo2011)


PopData <-
  read_excel(
    "../DATA/RAW/mid-2011-lsoa-quinary-estimates.xls",
    sheet = "Mid-2011 Persons",
    range = "A4:W35105"
  )
PopData <- PopData[c("Area Codes",
                     "...3",
                     "40-44",
                     "45-49",
                     "50-54",
                     "55-59",
                     "60-64",
                     "65-69",
                     "70-74")]
PopData$TotalPop <- PopData$"40-44"+PopData$"45-49"+PopData$"50-54"+PopData$"55-59"+PopData$"60-64"+PopData$"65-69"+PopData$"70-74"
PopData <- PopData[!is.na(PopData$"...3"), ]
PopData <- PopData[c("Area Codes", "TotalPop")]
ScotPop <-
  ScotlandGeo2011[c("code", "popcount")] #Popcount for scottish areas is based on all ages.
ScotPop <- st_set_geometry(ScotPop, NULL)
ScotPop$Country <- "Scotland"
PopData$Country <- "EnglandOrWales"
colnames(ScotPop) <- colnames(PopData)
PopData <- rbind(PopData, ScotPop)

UKGeo2011 <-
  merge(
    UKGeo2011,
    PopData,
    by.x = c("code", "Country"),
    by.y = c("Area Codes", "Country")
  )
summary(UKGeo2011$TotalPop)
rm(PopData)
UKGeo2011$CensusRegion <- NA
for (i in 1:NROW(CensusGeo)) {
  print(i)
  withinlist <- st_intersects(UKGeo2011, CensusGeo[i, ])
  withinlistlogical = lengths(withinlist) > 0
  UKGeo2011$CensusRegion[withinlistlogical == TRUE] <-
    CensusGeo$LA_G_N1[i]
}

AssessmentDistanceCutoff <-
  read.table("../DATA/RAW/UKBAssesmentDistanceCutoff.csv",
             header = TRUE,
             sep = ",")

AssessmentDistanceCutoff$DistanceCutoff <-
  AssessmentDistanceCutoff$DistanceCutoff * 1000

UKB_sf <- merge(UKB_sf, AssessmentDistanceCutoff)

UKB_circles <- st_buffer(UKB_sf, dist = UKB_sf$DistanceCutoff)

##Ear-mark LSOAs that are within 40km of an assessment centre.
#+st_intersects for a less conservative approach:
withinlist <- st_intersects(UKGeo2011, UKB_circles)
withinlistlogical = lengths(withinlist) > 0
UKGeo2011$UKB_eligible <- withinlistlogical

l <- lapply(UKGeo2011$geometry, function(x) {
  lapply(UKB_circles$geometry,
         function(y)
           st_intersection(st_make_valid(x), st_make_valid(y)) %>% st_area() / st_area(x))
})

l <- lapply(UKGeo2011$geometry, function(x) {
  lapply(UKB_circles$geometry,
         function(y)
           st_area(st_intersection(st_make_valid(x), st_make_valid(y))))
})


q <-
  as.data.frame(matrix(
    data = NA,
    nrow = length(l),
    ncol = NROW(UKB_circles)
  ))
for (i in 1:length(l)) {
  q[i, ] <- as.numeric(l[[i]]) / st_area(UKGeo2011$geometry[i])
  print(q[i, ])
}
UKGeo2011$UKB_eligible_area <- rowSums(q)
UKGeo2011$UKB_eligible_area[UKGeo2011$UKB_eligible_area >= 1] <- 1

UKGeo2011$UKB_eligible[UKGeo2011$UKB_eligible_area >= 0.3] <- TRUE
UKGeo2011$UKB_eligible[UKGeo2011$UKB_eligible_area < 0.3] <- FALSE
summary(UKGeo2011$UKB_eligible)

png("../OUTPUT/FIGURES/PopByLSOA.png",
    height = 1980,
    width = 1500)
ggplot() + geom_sf(data = UKGeo2011[UKGeo2011$UKB_eligible == 1, ],
                   fill = "green") +
  geom_sf(data = UKGeo2011[UKGeo2011$UKB_eligible == 0, ],
          fill = "red") + geom_sf(data = UKB_circles, fill = "black", alpha =
                                    0.25)
dev.off()

UKRegionPop <- aggregate(
  UKGeo2011$TotalPop,
  by = list(
    CensusRegion = UKGeo2011$CensusRegion,
    Country = UKGeo2011$Country
  ),
  FUN = sum
)


UKEligiblePop <-
  aggregate(
    UKGeo2011$TotalPop[UKGeo2011$UKB_eligible == TRUE],
    by = list(
      CensusRegion = UKGeo2011$CensusRegion[UKGeo2011$UKB_eligible == TRUE],
      Country = UKGeo2011$Country[UKGeo2011$UKB_eligible ==
                                    TRUE]
    ),
    FUN = sum
  )

names(UKEligiblePop)[names(UKEligiblePop) == 'x'] <- 'EligiblePop'
names(UKRegionPop)[names(UKRegionPop) == 'x'] <- 'TotPop'
UKRegionPop <-
  merge(
    UKRegionPop,
    UKEligiblePop,
    by = c("CensusRegion", "Country"),
    all.x = TRUE
  )
UKRegionPop$EligiblePop[is.na(UKRegionPop$EligiblePop)] <- 0
UKRegionPop$FractionPopEligible <-
  UKRegionPop$EligiblePop / UKRegionPop$TotPop

UKRegionPop <-
  UKRegionPop[c("CensusRegion", "FractionPopEligible", "Country")]
write.csv(UKRegionPop, "../DATA/TEMP/FractionPopEligible.csv", row.names = FALSE)
#+
#geom_sf(data = UKGeo2011, aes(fill = TotalPop), color=NA) +
#  scale_fill_viridis_c(trans = "sqrt",direction=-1)