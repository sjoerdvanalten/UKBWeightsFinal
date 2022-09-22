#!/bin/bash
#SBATCH -t 12:00:00 ## WALL CLOCK TIME
#SBATCH -N 1 -c 1 ## REQUEST NODES AND CORES (ONLY STAGING NODES CAN REQUEST SINGLE CORES)
#SBATCH -p staging ## NODE TYPE: normal, fat, gpu, short, staging

#cd /projects/0/Galama/svalten/UKBWeights/CODE
UKBRAWPATH=${UKBRAWPATH:-/projects/0/Galama/svalten/GWAS_PIPELINE/INPUT}

echo "	
## Baseline Characteristics ##
48 ## Waist circumference
49 ## Hip circumference
21022 ## Age at recruitment
50 ## Standing height
52 ## Month of birth	
54 ## Assessment centre
31 ## Sex
189 ## Townsend deprivation index at recruitment
34 ## Year of birth
129 ## Place of birth in UK - north coordinate
130 ## Place of birth in UK - east coordinate
845 ## Age completed full time education 
1647 ## Country of birth (UK/Elsewhere)
20115 ## Country of birth (non-UK origin)
6138 ## Qualifications	
20277 ## Job Code at visit 
22501 ## Year ended full time education 
22700 ## Date first recorded at location 
22702 ## Home location - east co-ordinate (rounded)
22704 ## Home location - north co-ordinate (rounded)
20074 ##Home location at assessment - east co-ordinate (rounded)
20075 ##Home location at assessment - north co-ordinate (rounded)
20118 ##Home area population density - urban or rural
21003 ##Age when attended assessment centre
709 ##Number in household 
728 ##Number of vehicles in household 
6141 ##How are people in household related to participant
132 ##Job code at visit - entered
20024 ##Job code at visit -deduced 
53 ##Date of attending assessment centre

## Genotyping process and sample QC ##
22006 ## Genetic ethnic grouping
22009 ## Genetic principal components
22001 ## Genetic sex
22000 ## Genotype measurement batch
22007 ## Genotype measurement plate
22008 ## Genotype measurement well
22003 ## Heterozygosity
22004 ## Heterozygosity, PCA corrected
22005 ## Missingness
22021 ## Genetic kinship to other participants 
22027 ## Outliers for heterozygosity or missingness rate 
22019 ## Sex chromosome aneuploidy 

## Addictions
20401 ## Ever addicted to any substance of behaviour
20406 ## Ever addicted to alcohol

##Blood biochemistry
30680 ## Calcium 
30690 ## Cholesterol
30890 ## Vitamin D 
306000 ##Albumin 

##Blood count
93 ## Systolic blood pressure, manual reading
94 ## Diastolic blood pressure, manual reading
30000 ## White blood cell (leukocyte) count
30010 ## Red blood cell (erythrocyte) count

##Blood pressure##
4079 ##Diastolic blood pressure, automated reading
4080 ##Systolic blood pressure, automated reading

##Summary diagnoses
41202 ##Diagnoses -main ICD10

##Diet##
100940 ## Bread consumed 
1498 ##Coffee intake
1488 ##Tea intake
1289 ##Cooked vegetable intake 
1408 ## Cheese intake

##Early life factors##
1677 ##Breastfed as a baby
1777 ##Part of a multiple birth 
20022 ##Birth weight 
120 ##Birth weight known 
1707 ##Handedness (chirality/laterality)
1767 ##Adopted as a child
1787 ##Maternal smoking around birth 

##Work characteristics##
22604 ##Work hours - lumped
6142 ##Current employment status 
20119 ##Current employment status - corrected
22599 ##Number of jobs held 
757 ##Time employed in main current job 
767 ##Length of working week main job  
816 ##Job involves heavy manual or physical work

##SES##
680 ## or rent accomodation currently lived in
738 ##Average total household income before tax 

##Mental Health##
20126 ##Bipolar and major depression status 
20458 ##General happiness
20544 ##Mental health problems ever diagnosed by a professional

##Physical Measures##
21001 ## Body mass index (BMI)
2178 ## Overall health rating
2188 ## Long-standing illness, disability or infirmity
2335 ## Chest pain or discomfort 

##Smoking##
20160 ##Ever smoked
20116 ##Smoking status
3456 ##Number of cigarettes currently smoked daily
2887 ##Number of cigarettes previously smoked daily 
20453 ##Ever taken cannabis 
20454 ##Maximum frequency of taking cannabis 
46 ##Hand grip strength (left)
47 ##Hand grip strength (right) 

##Alcohol##
1558 ##Alcohol intake frequency

##Medical conditions##
22126 ##Doctor diagnosed hayfever or allergic rhinitis
22127 #Doctor diagnosed asthma
2443 ##Diabetes diagnosed by doctor
2966 ##Age high blood pressure diagnosed
6150 ##Vascular/heart problems diagnosed by doctor

##Physical activity
22032 ##IPAQ activity group 

##Spirometry
20150 ##Force expiratory volume in 1-second (FEV1), Best measurement
20258 ##FEV1/FVC ratio z-score 


## Ethnicity ##
21000 ## Ethnic background
3659 ## Year immigrated to UK (United Kingdom)

##Death Information## 
40000 ##Date of Death " \
 > ../TEMP/UKB.FIELDS2EXTRACT_V4_owndata.txt

./ukbconv $UKBRAWPATH/basket2005284.ukb41217.enc_ukb r \
-i../TEMP/UKB.FIELDS2EXTRACT_V4_owndata.txt \
-o../INPUT/basket2005284.ukb41217_subset_Vtest.R