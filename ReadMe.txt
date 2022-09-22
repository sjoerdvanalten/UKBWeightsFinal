This folder contains all the codes necessary to reproduce the results ``The costs of non-reprsenative data: reweighting the UK Biobank 
corrects for pervasive selection bias due to volunteering'' by Sjoerd van Alten, Ben Domingue, Titus Galama and Andries Marees.

It is important to run each file from the directory in which it is stored.

Ensure that all data is available in the following folders:

PrepareCensus/DATA/RAW:

"recodev12.csv": This file is available from UK Data Service (under condition of safeguarded access), and contains the England & Wales 5% Local Authority Safeguarded Microdata
Place all files provied by UK Data Services for the 2011 Census Microdata Individual Safeguarded Sample (Local Authority): Scotland in a new subfolder called ScotlandCensus

-Create a subfolder called "LADMap1991": Place in this folder the geometry of the census 1991. These shapefiles can be downloaded at 
https://borders.ukdataservice.ac.uk/easy_download_data.html?data=England_dt_1991 and https://borders.ukdataservice.ac.uk/easy_download_data.html?data=Wales_dt_1991
The names of the files that one needs to download are england_dt_1991.dbf/prj/shp/shx and wales_dt_1991.dbf/prj/shp/shx 
- The file CensusLAGeo.geojson contains the geometry of the census 2011
local authority regions and can be found at
https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Census_Merged_Local_Authority_Districts_December_2011_Boundaries/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=geojson
-Create a subfolder called "LSOA2011" and downlload England and WALES LSOA geometries in it, as well as scotland LAS gometries. These are called england_lsoa_2011.dbf/prj/shp/shx, wales_lsoa_2011.dbf/prj/shp/shx  
and scotland_oa_2011/dbf/prj/shp/shx, and can be downloaded from the following sources:
https://borders.ukdataservice.ac.uk/easy_download_data.html?data=England_lsoa_2011
https://borders.ukdataservice.ac.uk/easy_download_data.html?data=Wales_lsoa_2011
https://borders.ukdataservice.ac.uk/easy_download_data.html?data=Scotland_oa_2011
-Create a subfolder called ScotlandCAMap2001 that contains the geometry of scottish council areas in 2001. These files are called scotland_ca_2001.dbf/prj/shp/shx and can be downloaded from 
https://borders.ukdataservice.ac.uk/easy_download_data.html?data=Scotland_ca_2001
- Download Mid-2011 population count by lower super output area from:
https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/lowersuperoutputareamidyearpopulationestimates
the relevant file is called mid-2011-lsoa-quinary-estimates.xls


To reproduce all results in the paper, use the following procedure:
 
First, run Main.sh in the PrepareCensus/CODE folder on a linux system. 
Next, open Main.txt in the UKB/CODE folder and run all bash files in order.

- The csv file LACodesToLaNames.csv shows how the England & Wales Microdata grouped
local authorities map into the actual local authorities. The information in this file
is obtained from the Appendix of the 2011 Census Microdata Individual Safeguarded 
Samples - User Guide