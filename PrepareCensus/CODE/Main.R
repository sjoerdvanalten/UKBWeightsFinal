#These files can be run on one's own computer ``without'' a need to access the UKB
##Store all UK Census files in the DATA/RAW folder 

options(bitmapType="cairo")

source("UKBGeometry.R") #final
source("UKBAssessmentCentresAndBirthyear.R")
source("LocalAuthoritiesInUKB.R")
source("UKCensusPrepare.R")

source("CreateFigure1.R") #+ Simulates scenarios for selection bias and creates figure 1 