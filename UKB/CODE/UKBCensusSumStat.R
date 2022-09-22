#Goal: Summary statistics of Census 5% sample, UKB sample, and weighted UKB sample
library(tidyverse)
library(xtable)
library(reporttools)
library(questionr)
library(sf)
library(ggrepel)

options(bitmapType="cairo")
##Load in region-matched data
rm(list=ls())

FullData<-read.table("../TEMP/UKBandCensusPriorToTesting.csv",header=TRUE,sep=",")

FullData$Education<-as.factor(FullData$Education)

UKBWeights<-read.table("../OUTPUT/UKBWeightsKFolded.csv",header=TRUE,sep=",")

UKBPheno<-FullData[FullData$ukb_selected==1,1:18]

UKCensus<-FullData[FullData$ukb_selected==0,1:18]


#SibData <- read.table("../../UKBKin/UKB2_siblist.txt",header=TRUE)
#SibData$HasSib <- 1
#SibData <- SibData[c("ID","MZ","FS","HasSib")]
#names(SibData)[names(SibData) == 'ID'] <- 'f.eid'

#SibData<-merge(UKBPheno,SibData,by="f.eid")
UKBWeighted<-merge(UKBPheno,UKBWeights,by="f.eid")
UKBWeighted$popweight<-UKBWeighted$LassoWeight

RelativeTable <- function(vector){
  Total <- NROW(vector)
  AbsTab <- table(vector)
  print(Total)
  print(AbsTab)
  Tab <- (AbsTab/Total)*100
  Tab <- c(Tab,100)
  print(Tab)
  return(Tab)
}

varlist <-c("sex","birthyear","Education","HealthSelfReport","Empstat","carsnoc","Tenure","BirthCountry","Ethnicity","SingleHousehold")
for (i in varlist){
  print(i)
  x<-c(wtd.table(UKCensus[[i]][!is.na(UKCensus[[i]])],weights=UKCensus$popweight[!is.na(UKCensus[[i]])]),sum(UKCensus$popweight[!is.na(UKCensus[[i]])]))
  Relx<-100*(wtd.table(UKCensus[[i]][!is.na(UKCensus[[i]])],weights=UKCensus$popweight[!is.na(UKCensus[[i]])])/sum(UKCensus$popweight[!is.na(UKCensus[[i]])]))
  Relx<-c(Relx,100)
  assign(paste0("Census",i),x)
  assign(paste0("CensusRel",i),Relx)
  UKBx<-c(table(UKBPheno[[i]]),NROW(UKBPheno[[i]][!is.na(UKBPheno[[i]])]))
  UKBRelx<- RelativeTable(UKBPheno[[i]])
  assign(paste0("UKB",i),UKBx)
  assign(paste0("UKBRel",i),UKBRelx)
  #Sibx<-c(table(SibData[[i]]),NROW(SibData[[i]][!is.na(SibData[[i]])]))
  #SibRelx<- RelativeTable(SibData[[i]])
  #assign(paste0("Sib",i),Sibx)
  #assign(paste0("SibRel",i),SibRelx)
}

CensusSex<-Censussex
CensusRelSex<-CensusRelsex
CensusBirthyear<-Censusbirthyear
CensusRelBirthyear<-CensusRelbirthyear
CensusEduc<-CensusEducation
CensusRelEduc<-CensusRelEducation
UKBSex<-UKBsex
UKBRelSex<-UKBRelsex
UKBBirthyear<-UKBbirthyear
UKBRelBirthyear<-UKBRelbirthyear
UKBEduc<-UKBEducation
UKBRelEduc<-UKBRelEducation
#SibSex<-Sibsex
#SibRelSex<-SibRelsex
#SibBirthyear<-Sibbirthyear
#SibRelBirthyear<-SibRelbirthyear
#SibEduc<-SibEducation
#SibRelEduc<-SibRelEducation


CensusCol <- c(CensusSex,CensusBirthyear,CensusEduc,CensusEthnicity,CensusHealthSelfReport,CensusEmpstat,Censuscarsnoc,CensusTenure,CensusSingleHousehold)
CensusRelCol <- c(CensusRelSex,CensusRelBirthyear,CensusRelEduc,CensusRelEthnicity,CensusRelHealthSelfReport,CensusRelEmpstat,CensusRelcarsnoc,CensusRelTenure,CensusRelSingleHousehold)
UKBCol <- c(UKBSex,UKBBirthyear,UKBEduc,UKBEthnicity,UKBHealthSelfReport,UKBEmpstat,UKBcarsnoc,UKBTenure,UKBSingleHousehold)
UKBRelCol <- c(UKBRelSex,UKBRelBirthyear,UKBRelEduc,UKBRelEthnicity,UKBRelHealthSelfReport,UKBRelEmpstat,UKBRelcarsnoc,UKBRelTenure,UKBRelSingleHousehold)
#SibCol <- c(SibSex,SibBirthyear,SibEduc,SibEthnicity,SibHealthSelfReport,SibEmpstat,Sibcarsnoc,SibTenure,SibSingleHousehold)
#SibRelCol <- c(SibRelSex,SibRelBirthyear,SibEthnicity,SibRelEduc,SibRelHealthSelfReport,SibRelEmpstat,SibRelcarsnoc,SibRelTenure,SibRelSingleHousehold)

SexTest1<-chisq.test(cbind(CensusSex[-3], UKBSex[-3]))$p.value
BYTest1<-chisq.test(cbind(CensusBirthyear[-8], UKBBirthyear[-8]))$p.value
EducTest1<-chisq.test(cbind(CensusEduc[-6], UKBEduc[-6]))$p.value
HealthSelfReportTest1<-chisq.test(cbind(CensusHealthSelfReport[-5], UKBHealthSelfReport[-5]))$p.value
EthnicityTest1<-chisq.test(cbind(CensusEthnicity[-6], UKBEthnicity[-6]))$p.value
EmpstatTest1<-chisq.test(cbind(CensusEmpstat[-7], UKBEmpstat[-7]))$p.value
carsnocTest1<-chisq.test(cbind(Censuscarsnoc[-5], UKBcarsnoc[-5]))$p.value
TenureTest1<-chisq.test(cbind(CensusTenure[-6], UKBTenure[-6]))$p.value
BirthCountryTest1<-chisq.test(cbind(CensusBirthCountry[-13], UKBBirthCountry[-13]))$p.value
SingleHouseholdTest1<-chisq.test(cbind(CensusSingleHousehold[-3], UKBSingleHousehold[-3]))$p.value


#SexTest2<-chisq.test(cbind(CensusSex[-3], SibSex[-3]))$p.value
#BYTest2<-chisq.test(cbind(CensusBirthyear[-8], SibBirthyear[-8]))$p.value
#EducTest2<-chisq.test(cbind(CensusEduc[-6], SibEduc[-6]))$p.value
#HealthSelfReportTest2<-chisq.test(cbind(CensusHealthSelfReport[-5], SibHealthSelfReport[-5]))$p.value
#EthnicityTest2<-chisq.test(cbind(CensusEthnicity[-6], SibEthnicity[-6]))$p.value
#EmpstatTest2<-chisq.test(cbind(CensusEmpstat[-7], SibEmpstat[-7]))$p.value
#carsnocTest2<-chisq.test(cbind(Censuscarsnoc[-5], Sibcarsnoc[-5]))$p.value
#TenureTest2<-chisq.test(cbind(CensusTenure[-6], SibTenure[-6]))$p.value
#BirthCountryTest2<-chisq.test(cbind(CensusBirthCountry[-13], SibBirthCountry[-13]))$p.value
#SingleHouseholdTest2<-chisq.test(cbind(CensusSingleHousehold[-3], SibSingleHousehold[-3]))$p.value

#SexTest3<-chisq.test(cbind(UKBSex[-3], SibSex[-3]))$p.value
#BYTest3<-chisq.test(cbind(UKBBirthyear[-8], SibBirthyear[-8]))$p.value
#EducTest3<-chisq.test(cbind(UKBEduc[-6], SibEduc[-6]))$p.value
#HealthSelfReportTest3<-chisq.test(cbind(UKBHealthSelfReport[-5], SibHealthSelfReport[-5]))$p.value
#EthnicityTest3<-chisq.test(cbind(UKBEthnicity[-6], SibEthnicity[-6]))$p.value
#EmpstatTest3<-chisq.test(cbind(UKBEmpstat[-7], SibEmpstat[-7]))$p.value
#carsnocTest3<-chisq.test(cbind(UKBcarsnoc[-5], Sibcarsnoc[-5]))$p.value
#TenureTest3<-chisq.test(cbind(UKBTenure[-6], SibTenure[-6]))$p.value
#BirthCountryTest3<-chisq.test(cbind(UKBBirthCountry[-13], SibBirthCountry[-13]))$p.value


PFormat<- function(p){
  print(p)
  pF<-round(p,digits=8)
  if (pF < 0.00000001){
    pF<-"< 1e-8"
  } else {
    pF<-format(pF,scientific=FALSE)  
  }
  return(pF)
}

SumTab <- tibble(Variable = c("Sex", " ", " ","Birthyear"," "," "," "," "," "," "," ","Education","","","","","Ethnicity","","","","","",
                              "Health (self-reported)","","","","Employment status","","","","","","","No. of vehicles in household","","","","","","Tenure of dwelling","","","","","",
                              "One-person household","",""), 
                 Levels = c("Male", "Female", "Total","1936-1940","1941-1945","1946-1950","1951-1955","1956-1960","1961-1965","1966-1970","Total","None","Lower secondary","A-levels/vocational","University","Total",
                            "White","Mixed","Asian/Asian British","Black/Black British","Other","Total","Bad","Fair","Good/Very Good","Total","Paid employment","Retired","Stay-at-home","Incapacitated","Unemployed","Student","Total","0","1","2","3","4 or more","Total",
                            "Owns house (no mortgage)","Owns house w/ mortage","Shared ownership","Rent","Rent-free","Total",
                            "No","Yes","Total"), 
                 N_Census = CensusCol,
                 perc_Census = CensusRelCol,
                 N_UKB = UKBCol,
                 perc_UKB = UKBRelCol,
                 P_Chi_Census_UKB = c(PFormat(SexTest1),"","",PFormat(BYTest1),"","","","","","","",PFormat(EducTest1),"","","","",PFormat(EthnicityTest1),"","","","","",
                                      PFormat(HealthSelfReportTest1),"","","",PFormat(EmpstatTest1),"","","","","","",PFormat(carsnocTest1),"","","","","",
                                      PFormat(TenureTest1),"","","","","",PFormat(SingleHouseholdTest1),"",""))


#boldEdit <- function(x){
#paste0("\\textbf{", xtable::sanitize(x, type = "latex"), "}")
#paste0("\\textbf{",x,"}")
#  paste0(x)
#}
#To latex
SumTable <- xtable(SumTab, align = "rll|rr|rr|l", 
                   caption="\\textbf{Full summary statistics for the UKB-eligible population and the UKB}. 
                   The UKB-eligible population is created by restricting and weighting the 5\\% subsample of the 2011 UK Census. 
                   $P_{\\chi^2}$ in the 7th column reports the p-value of a $\\chi^2$-test for equal distributions for each variable as measured 
                   in the UKB-eligible population and UKB.",label="tab:UKBSumStatExtended")
colnames(SumTable) = c("\\textbf{Variable}", "\\textbf{Levels}", "\\textbf{N}", "\\textbf{\\%}", 
                       "\\textbf{N}", "\\textbf{\\%}","$P_{\\chi^2}$")
print(SumTable, include.rownames = FALSE, type = "latex", hline.after = c(2,3,10,11,15,16,21,22,25,26,32,33,38,39,44,45,47,48),
      sanitize.colnames.function = paste0,file="../OUTPUT/TABLES/SumStatsDifferences.tex",tabular.environment="longtable",floating=FALSE,format.args=list(big.mark = ",", decimal.mark = "."),
      add.to.row =list(pos=list(-1,0),command = c(paste("\\hline", 
                                                        "\\multicolumn{2}{c}{} & \\multicolumn{2}{c}{\\textbf{Census (UKB-eligible)}} & 
                                                           \\multicolumn{2}{c}{\\textbf{UKB}} & \\\\",
                                                        "\\hline"),"\\hline \\endhead ")))

#Make a different table, coding variables continuously:

#Make a map of UKB participation rates:
UKBRegCount<-count(x=UKBPheno,region)
UKCRegCount<-count(x=UKCensus,region,wt=popweight)
RegRate<-tibble(Region=UKBRegCount[,1],Participation=(UKBRegCount[,2]/UKCRegCount[,2])*100)
print(RegRate)

summary(RegRate$Participation) #Respondence in the UKB differs substantially by UKB region, ranging from 
UKGeography <- read_sf("../INPUT/UKBGeometryToCensusAreas.shp")

UKGeography$Index[UKGeography$Country=="EnglandOrWales"]<-1000
UKGeography$Index[UKGeography$Country=="Scotland"]<-2000
UKGeography$LARegion<-UKGeography$Index+UKGeography$LA_G_N1

NROW(UKGeography)
UKGeography<-merge(UKGeography,RegRate,by.x="LARegion",by.y="Region",all.x=TRUE)
NROW(UKGeography)

summary(UKGeography$Participation)

UKB_sf <- st_read("../INPUT/UKB_Assessment_geography.geojson")

UKB_sf$name[UKB_sf$name=="Croydon"] <- "Croydon (London)"
UKB_sf$name[UKB_sf$name=="Hounslow"] <- "Hounslow (London)"
UKB_sf$name[UKB_sf$name=="Barts"] <- "Central London"


nudgex<-c(300000,-400000,-400000,-200000,-200000,300000,400000,-400000,0,
          400000,-300000,-200000,400000,0,600000,700000,0,400000,
          300000,-300000,-200000,-300000)
nudgey<-c(0,0,0,0,0,0,0,0,-100000,
          0,0,0,0,150000,0,0,-200000,0,
          0,0,0,0)

UKB_sf$coordX <- map_dbl(UKB_sf$geometry, 1)
UKB_sf$coordY <- map_dbl(UKB_sf$geometry, 2)

png("../OUTPUT/FIGURES/ParticipationMap.png",height=1980,width=1500)
ggplot() + geom_sf(data = UKGeography, aes(fill = Participation)) + 
  scale_fill_gradient(name = "Participation Rate", low = "#56B1F7", high = "#132B43") + 
  geom_point(aes(x=coordX,y=coordY), data = UKB_sf, color = "red") +
  geom_text_repel(aes(x=coordX,y=coordY,label = name), data = UKB_sf, 
                  colour = "black", size = 6) +
  theme_classic() + theme(legend.position = "bottom",
                          legend.title = element_text(size = 25),
                          legend.text = element_text(size = 20), 
                          legend.key.size = unit(1.5,'cm'),
                          legend.justification = "right", axis.title.x=element_blank(), axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                          axis.text.y=element_blank(), axis.ticks.y=element_blank())
dev.off()

UKGeography$Participation[which(is.na(UKGeography$Participation))]<-0

UKBWeighted$ukb_selected<-2
AllData<-rbind(UKCensus,UKBPheno,UKBWeighted[,1:18])
AllData$UKBSel <- factor(AllData$ukb_selected,c(0,1,2),c("Census (UKB-eligible)","UKB","UKB Weighted"))
AllData$Female <- (as.integer(AllData$sex))
AllData$AtLeastOneCar <- (as.integer(AllData$carsnoc>=1))
AllData$ReportsPoorHealth <- (as.integer(AllData$HealthSelfReport==1))
AllData$PaidWork<-(as.integer(AllData$Empstat==1))
AllData$Retired<-(as.integer(AllData$Empstat==2))
AllData$Incapacitated<-(as.integer(AllData$Empstat==4))
AllData$Unemployed<-(as.integer(AllData$Empstat==5))
AllData$OwnsHouse<-(as.integer(AllData$Tenure<=2))
AllData$ALevels<-as.integer((as.integer(AllData$Education)>=3))
AllData$University<-as.integer((as.integer(AllData$Education)>=4))
AllData$White<-as.integer((as.integer(AllData$Ethnicity)==1))

source("functions/tableContinuous2.R")
IncVars<-c("birthyearcenter","HealthSelfReport","YearsEducation","carsnoc","Female","University",
           "ReportsPoorHealth","PaidWork","Retired","Incapacitated","Unemployed","OwnsHouse","White","SingleHousehold")
  TabVars<-AllData[, IncVars]
Names<-c("Year of Birth","Self-reported health","Years of education","No. of cars","Female","University or equiv.",
         "Reports \``poor health\"","Has paid work","Retired","Incapacitated","Unemployed","House owner","White ethnicity","One-person household")
colnames(TabVars)<-Names

fonts<-getFonts("bf")
header<-c(fonts$text("Var"),fonts$text("Dataset"),fonts$text("N"),
          fonts$text("Min"),fonts$text("Q1"),fonts$text("Median"),fonts$text("Mean"),
          fonts$text("Q3"),fonts$text("Max"),fonts$text("SD"))
tableContinuous2(vars = TabVars, weights=AllData$popweight, stats=c("n","min","mean","max","s"), group=AllData$UKBSel, longtable=FALSE,float=FALSE,prec=3,
                 col.tit=header,col.tit.font="bf",format.args=list(big.mark = ",", decimal.mark = "."),add.to.row =list(pos=list(0,15),
                                                                   command = c("\\hline \\textbf{Discrete/Continuous variables} \\\\",
                                                                               "\\hline \\textbf{Bivariate variables} \\\\")),
                 file="../OUTPUT/UKBSumCont.tex")

TabVars1<-AllData[AllData$ukb_selected<=1, IncVars]
colnames(TabVars1)<-Names
tableContinuous2(vars = TabVars1, weights=AllData$popweight[AllData$ukb_selected<=1], stats=c("n","min","mean","max"), group=AllData$UKBSel[AllData$ukb_selected<=1], longtable=FALSE, 
                 prec=3, col.tit=header,col.tit.font="bf", pval.bound=10^-9, file="../OUTPUT/UKBSumContCensusUKB.tex",print.pval = "anova")


TabVars2<-AllData[AllData$ukb_selected==0|AllData$ukb_selected==2, IncVars]
colnames(TabVars2)<-Names
tableContinuous2(vars = TabVars2, stats=c("n","min","mean","max"), weights=AllData$popweight[AllData$ukb_selected==0|AllData$ukb_selected==2],group=AllData$UKBSel[AllData$ukb_selected==0|AllData$ukb_selected==2], longtable=FALSE, prec=3, file="../OUTPUT/UKBSumContCensusUKBW.tex",print.pval = "anova")

#TabVars3<-AllData[AllData$ukb_selected>=1, IncVars]
#colnames(TabVars3)<-Names
#tableContinuous(vars = TabVars3, stats=c("n","min","mean","max"), group=AllData$UKBSel[AllData$ukb_selected>=1], longtable=FALSE, prec=3,  file="../OUTPUT/UKBSumContUKBUKBSIB.tex",print.pval = "anova")

#Extended summary statistics:
AllData$SomeDegree <- as.integer(as.integer(AllData$Education)>=2)
AllData$BornInEngland<-(as.integer(AllData$BirthCountry==1))
AllData$BornInGB<-(as.integer(AllData$BirthCountry==1|AllData$BirthCountry==2|AllData$BirthCountry==4))
AllData$BornInUKOrEU<-(as.integer(AllData$BirthCountry<=7))
AllData$PaidWork<-(as.integer(AllData$Empstat==1))
AllData$Retired<-(as.integer(AllData$Empstat==2))
AllData$Incapacitated<-(as.integer(AllData$Empstat==4))
AllData$Unemployed<-(as.integer(AllData$Empstat==5))
AllData$OwnsHouse<-(as.integer(AllData$Tenure<=2))

Vars<-c("Female", "birthyearcenter","SomeDegree","ALevels","University","ReportsPoorHealth","BornInEngland",
        "BornInGB","BornInUKOrEU","PaidWork","Retired","Incapacitated","Unemployed","OwnsHouse","carsnoc")
TabVars<-AllData[, Vars]
Names<-c("Female","Birth year","Some Degree","At least A-Levels","University","Reports \`\`poor health\'\'","BornInEngland","Born In Great Britain",
         "Born In EU","Has paid work","Retired","Incapacitated","Unemployed","House owner","No. of cars")

colnames(TabVars)<-Names

fonts<-getFonts("bf")
tableContinuous2(vars = TabVars, weights=AllData$popweight, stats=c("n","min","mean","max"), group=AllData$UKBSel, longtable=FALSE,float=FALSE,prec=3,
                 col.tit=c(fonts$text("Var"),fonts$text("Dataset"),fonts$text("N"),
                           fonts$text("Min"),fonts$text("Q1"),fonts$text("Median"),fonts$text("Mean"),
                           fonts$text("Q3"),fonts$text("Max")),col.tit.font="bf",file="../OUTPUT/UKBSumContExtended.tex")

TabVars1<-AllData[AllData$ukb_selected<=1, Vars]
colnames(TabVars1)<-Names
tableContinuous2(vars = TabVars1, weights=AllData$popweight[AllData$ukb_selected<=1], stats=c("n","min","median","mean","max"), group=AllData$UKBSel[AllData$ukb_selected<=1], longtable=FALSE, float=FALSE, prec=3, 
                col.tit=header,file="../OUTPUT/UKBSumContCensusUKBExtended.tex",print.pval = "anova")

#TabVars2<-AllData[AllData$ukb_selected==0|AllData$ukb_selected==2, Vars]
#colnames(TabVars2)<-Names
#tableContinuous(vars = TabVars2, stats=c("n","min","median","mean","max"), group=AllData$UKBSel[AllData$ukb_selected==0|AllData$ukb_selected==2], longtable=FALSE, prec=3, 
#                file="../OUTPUT/UKBSumContCensusUKBSIBExtended.tex",print.pval = "anova")

#TabVars3<-AllData[AllData$ukb_selected>=1, Vars]
#colnames(TabVars3)<-Names
#tableContinuous(vars = TabVars3, stats=c("n","min","median","mean","max"), group=AllData$UKBSel[AllData$ukb_selected>=1], longtable=FALSE, prec=3,  file="../OUTPUT/UKBSumContUKBUKBSIBExtended.tex",print.pval = "anova")
