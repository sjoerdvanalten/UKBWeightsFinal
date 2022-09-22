##Date: 06/10/2020

##Author: Sjoerd van Alten

##Goal: Create the geometry of the 22 UKB assesment centers, which should result in a datafile
#+that can be used in the .R file LocalAuthoritiesInUKB.R

#Note: Where possible, groupings of local authorities are based on 1991 geographies and groupings
rm(list = ls())
#removes all plots momentarily load in RStudio:
while (!is.null(dev.list()))
  dev.off()

library(sf) #package to handle geographic dataa

#Note: assessment centre coordinates derived from https://biobank.ndph.ox.ac.uk/showcase/refer.cgi?id=11002
stockport_point = st_point(c(389200.00, 390300.00))
manchester_point = st_point(c(383700.00, 396500.00))
oxford_point = st_point(c(451100.00, 206000.00))
glasgow_point = st_point(c(259000.00, 666100.00))
cardiff_point = st_point(c(317500.00, 179300.00))
edinburgh_point = st_point(c(325400.00, 674000.00))
stoke_point = st_point(c(388500.00, 347500.00))
bury_point = st_point(c(380600.00, 410600.00))
newcastle_point = st_point(c(424600.00, 563900.00))
leeds_point = st_point(c(430000.00, 433600.00))
reading_point = st_point(c(471400.00, 173600.00))
bristol_point = st_point(c(359000.00, 173100.00))
nottingham_point = st_point(c(457200.00, 339300.00))
barts_point = st_point(c(532000.00, 182000.00))
liverpool_point = st_point(c(334700.00, 390600.00))
middlesbrough_point = st_point(c(449500.00, 520300.00))
hounslow_point = st_point(c(513000.00, 175400.00))
sheffield_point = st_point(c(435200.00, 387600.00))
croydon_point = st_point(c(532500.00, 165700.00))
birmingham_point = st_point(c(406900.00, 286900.00))
swansea_point = st_point(c(265200.00, 192300.00))
wrexham_point = st_point(c(333500.00, 350400.00))

ukb_geom = st_sfc(
  stockport_point,
  manchester_point,
  oxford_point,
  glasgow_point,
  cardiff_point,
  edinburgh_point,
  stoke_point,
  bury_point,
  newcastle_point,
  leeds_point,
  reading_point,
  bristol_point,
  nottingham_point,
  barts_point,
  liverpool_point,
  middlesbrough_point,
  hounslow_point,
  sheffield_point,
  croydon_point,
  birmingham_point,
  swansea_point,
  wrexham_point,
  crs = 27700
)           # sfc object
ukb_attrib = data.frame(
  # data.frame object
  name = c(
    "Stockport",
    "Manchester",
    "Oxford",
    "Glasgow",
    "Cardiff",
    "Edinburgh",
    "Stoke",
    "Bury",
    "Newcastle",
    "Leeds",
    "Reading",
    "Bristol",
    "Nottingham",
    "Barts",
    "Liverpool",
    "Middlesbrough",
    "Hounslow",
    "Sheffield",
    "Croydon",
    "Birmingham",
    "Swansea",
    "Wrexham"
  )
)
ukb_sf = st_sf(ukb_attrib, geometry = ukb_geom)    # sf object

file.remove("../DATA/TEMP/UKB_Assessment_geography.geojson")
st_write(ukb_sf, "../DATA/TEMP/UKB_Assessment_geography.geojson")