##Date: 03/05/2021

##Author: Sjoerd van Alten

##Goal: Create a datafile of the 22 UKB assessment centres, along with the birthyear distribution of UKB participants that belongs to this
#+assessment center. The birthyear distribution can be calculated by taking the year at start of assessment at each centre (which ranges from
#+ 2006 to 2010), minus 70 (for the earliest included birthyears) and minus 40 (for the latest included birthyears). For information on each
#+ assessment centre, and when assessment started, see https://biobank.ndph.ox.ac.uk/showcase/exinfo.cgi?src=UKB_centres_map

#+ Note: there are a few assessment centres for which the birthyear distribution does not exactly follow the cutoff rule described above:
#+ The following centres sampled one year too many (Bristol (1969), Hounslow (1970), Birmingham (1970). Leeds sampled a year to few (1968)
#+ individuals not sampled. In all these case, we assume that these deviations from the overall UKB sampling rule were intended and not due
#+ to volunteer bias.

## information on the cutoffs of birthyear distribution, per assessment centre, will be used to create weights for the UK Census data to ensure
#+ that the UK Census data is representative of the UKB-eligible population.
library(tidyverse)

CentreBirthyearInfo <- tibble(
  clinicID = c(
    10003,
    11001,
    11002,
    11003,
    11004,
    11005,
    11006,
    11007,
    11008,
    11009,
    11010,
    11011,
    11012,
    11013,
    11014,
    11016,
    11017,
    11018,
    11020,
    11021,
    11022,
    11023
  ),
  UKBCentre = c(
    "Stockport",
    "Manchester",
    "Oxford",
    "Cardiff",
    "Glasgow",
    "Edinburgh",
    "Stoke",
    "Reading",
    "Bury",
    "Newcastle",
    "Leeds",
    "Bristol",
    "Barts",
    "Nottingham",
    "Sheffield",
    "Liverpool",
    "Middlesbrough",
    "Hounslow",
    "Croydon",
    "Birmingham",
    "Swansea",
    "Wrexham"
  ),
  BirthyearMin = c(
    1936,
    1937,
    1937,
    1937,
    1937,
    1937,
    1937,
    1938,
    1938,
    1938,
    1938,
    1938,
    1938,
    1938,
    1939,
    1939,
    1939,
    1939,
    1939,
    1939,
    1940,
    1940
  ),
  BirthyearMax = c(
    1966,
    1967,
    1967,
    1967,
    1967,
    1967,
    1967,
    1968,
    1968,
    1968,
    1967,
    1969,
    1968,
    1968,
    1969,
    1969,
    1969,
    1969,
    1969,
    1970,
    1970,
    1970
  )
)
write.csv(CentreBirthyearInfo,
          "../DATA/TEMP/UKBCentreBirthYearInfo.csv",
          row.names = FALSE)
