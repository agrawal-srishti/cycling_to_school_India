
rm(list=ls())

#### 1. Load the required libraries ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(haven)
library(survey)
library(jtools)
library(remotes)
library(svrepmisc)
library(tidyverse)
library(ggrepel)
library(ggpubr)
library(mgcv)
library(tidymv)
library(xtable)
library(prettyR)


#### 2. Set the working directory (specify the path of the directory where you store the data) ####

setwd('')


#### 3. Read data for round 1 of our analysis NSS 64: 2007-08 (July 2007-June 2008) #### 

hh_identify64 <- read.table('Nss64_25.2_new format/Block-1 & 2   Identification of sample household .txt', header = TRUE, sep="\t")
hhs64 <- read.table('Nss64_25.2_new format/Block-3  Household  characteristics.txt', header = TRUE, sep="\t")
ind64 <- read.table('Nss64_25.2_new format/Block-4  Demographic and other particulars of household members.txt', header = TRUE, sep="\t")
edu64 <- read.table('Nss64_25.2_new format/Block-5  Education particulars of those aged 5-29 years who are currently attending primary level and above.txt', header = TRUE, sep="\t")
exp64 <- read.table('Nss64_25.2_new format/Block-6  Particulars of private expenditure for those aged 5-29 yearts who are currently attending at primary level and above.txt', header = TRUE, sep="\t")
nonedu64 <- read.table('Nss64_25.2_new format/Block-7  Particulars of currently not attending persons aged 5-29 years.txt', header = TRUE, sep="\t")

#### 3.1 Block-1 household identification ####

#names(hh_identify64)[2]<-"FSU_slno"
names(hh_identify64)[3]<-"round"
names(hh_identify64)[4]<-"schedule_no"
names(hh_identify64)[5]<-"sample"
names(hh_identify64)[6]<-"sector"
names(hh_identify64)[7]<-"state"
names(hh_identify64)[14]<-"FOD_subregion"
#names(hh_identify64)[15]<-"hg_sb_no"
#names(hh_identify64)[16]<-"sss_no"
#names(hh_identify64)[17]<-"hhsample_no"
names(hh_identify64)[18]<-"level"
names(hh_identify64)[24]<-"date_of_survey"
names(hh_identify64)[32]<-"hh_id" #FSU_slno,hg_sb_no,sss_no,hhsample_no
names(hh_identify64)[33]<-"hh_wgt"
names(hh_identify64)[34]<-"state_region"
names(hh_identify64)[35]<-"state_region_district"
hh_identify64$year <- 2007

## select the required columns
hh_identify64<- subset(hh_identify64, select=c("year","hh_id","hh_wgt","sector","state","state_region",
                                               "state_region_district","date_of_survey","level",
                                               "round","schedule_no","sample","FOD_subregion"))

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
hh_identify64<- hh_identify64 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
hh_identify64$sector[hh_identify64$sector==1] <- "rural"
hh_identify64$sector[hh_identify64$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss64_25.2_new format/district_lookup_64.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"
hh_identify64<- hh_identify64 %>% left_join(district_lookup, by="state_region_district")


#### 3.2 Block-3 Household characteristics ####

names(hhs64)[3]<-"round"
names(hhs64)[4]<-"schedule_no"
names(hhs64)[5]<-"sample"
names(hhs64)[6]<-"sector"
names(hhs64)[7]<-"state"
names(hhs64)[14]<-"FOD_subregion"
names(hhs64)[18]<-"level"
names(hhs64)[19]<-"hh_size"
names(hhs64)[20]<-"nic" #2004 code
names(hhs64)[21]<-"nco" #2004 code
names(hhs64)[22]<-"hh_type"
names(hhs64)[23]<-"religion"
names(hhs64)[24]<-"social_group"
# names(hhs64)[25]<-"land_possess"
# names(hhs64)[26]<-"hh_exp_dependants" #Is the household incurring any expenditure during current academic session/year on dependants aged 5-29 years studying away from home? (Yes-1, No-2)
# names(hhs64)[27]<-"no_of_dependants"
# names(hhs64)[28]<-"amt_dependants"
names(hhs64)[29]<-"primaryclass_dist_nearest_school"
names(hhs64)[30]<-"upperprimaryclass_dist_nearest_school"
names(hhs64)[31]<-"secclass_dist_nearest_school"
# names(hhs64)[32]<-"purchase_hh_cons_exp_30days"
# names(hhs64)[33]<-"homeproducedstock_hh_cons_exp_30days"
# names(hhs64)[34]<-"goodsnservices_hh_cons_exp_30days"
# names(hhs64)[35]<-"giftsnloans_hh_cons_exp_30days"
# names(hhs64)[36]<-"freecollection_hh_cons_exp_30days"
names(hhs64)[37]<-"hh_exp_month"
names(hhs64)[38]<-"hh_id"
names(hhs64)[39]<-"hh_wgt"
names(hhs64)[40]<-"state_region"
names(hhs64)[41]<-"state_region_district"
hhs64$hh_comp <- NA
hhs64$hh_internet <- NA
hhs64$year <- 2007
#household monthly expenditure per capita
hhs64$hhexp_percapita <- hhs64$hh_exp_month/hhs64$hh_size

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
hhs64<- hhs64 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
hhs64$sector[hhs64$sector==1] <- "rural"
hhs64$sector[hhs64$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss64_25.2_new format/district_lookup_64.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"
hhs64<- hhs64 %>% left_join(district_lookup, by="state_region_district")

##divide hh monthly expenditure per capita into 5 quintiles
#specify survey design: strata at district and sector (rural/urban); household weights (hh_wgt)
svyd_hhs64 <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=hhs64)
svyquantile(~hhexp_percapita, subset(svyd_hhs64, !is.na(hhs64$hhexp_percapita)), quantiles = seq(0, 1 , by=0.2))
hhs64$hepc_q <- cut(hhs64$hhexp_percapita, breaks = c(0,450,595.83,783.33,1170,26751),
                    labels = c("Q1","Q2","Q3","Q4","Q5"),na.rm=T)

#calculate standardised monthly per capita expenditure 
hepc_mean <- svymean(~hhexp_percapita, svyd_hhs64, na=TRUE)
hepc_sd <- svysd(~hhexp_percapita, svyd_hhs64, na=TRUE)
hhs64$std_mpce <- (hhs64$hhexp_percapita-hepc_mean)/hepc_sd

##hh type
hhs64$hh_type[which(hhs64$hh_type %in% c(1,2,19,29))]<-"others"
hhs64$hh_type[which(hhs64$hh_type %in% c(11))]<-"self-employed non-agriculture"
hhs64$hh_type[which(hhs64$hh_type %in% c(14))]<-"self-employed agriculture"
hhs64$hh_type[which(hhs64$hh_type %in% c(12))]<-"agricultural labour"
hhs64$hh_type[which(hhs64$hh_type %in% c(13))]<-"other labour"
hhs64$hh_type[which(hhs64$hh_type %in% c(21))]<-"self-employed"
hhs64$hh_type[which(hhs64$hh_type %in% c(22))]<-"regular wage/salary"
hhs64$hh_type[which(hhs64$hh_type %in% c(23))]<-"casual labour"

##select variables
hhs64<- subset(hhs64, select=c("year","hh_id","hh_wgt","sector","state","state_region",
                               "state_region_district","hh_size","nic","nco",
                               "hh_type","religion","social_group","primaryclass_dist_nearest_school",
                               "upperprimaryclass_dist_nearest_school","secclass_dist_nearest_school",
                               "hh_exp_month","hh_comp","hh_internet",
                               "level","round","schedule_no","sample","FOD_subregion","hhexp_percapita","hepc_q","std_mpce"))



#### 3.3 Block-4 Demographic and other particulars of household members ####

names(ind64)[3]<-"round"
names(ind64)[4]<-"schedule_no"
names(ind64)[5]<-"sample"
names(ind64)[6]<-"sector"
names(ind64)[7]<-"state"
names(ind64)[14]<-"FOD_subregion"
names(ind64)[18]<-"level"
names(ind64)[19]<-"psrl_no"
names(ind64)[20]<-"relation_to_head"
names(ind64)[21]<-"sex"
names(ind64)[22]<-"age" #For infants below one year of age, '0' will be entered
names(ind64)[23]<-"marital_status"
names(ind64)[24]<-"edu_level"
# names(ind64)[25]<-"edu_attend"
# names(ind64)[26]<-"edu_enroll"
names(ind64)[27]<-"hh_id"
names(ind64)[28]<-"person_id" #hh_id,psrl_no
names(ind64)[29]<-"hh_wgt"
names(ind64)[30]<-"state_region"
names(ind64)[31]<-"state_region_district"
# names(ind64)[32]<-"hh_size"
# # names(ind64)[33]<-"nic_2004"
# # names(ind64)[34]<-"nco_2004"
# names(ind64)[35]<-"hh_type"
# names(ind64)[36]<-"religion"
# names(ind64)[37]<-"social_group"
# names(ind64)[38]<-"land_possess"
# names(ind64)[39]<-"hh_exp_dependants" #Is the household incurring any expenditure during current academic session/year on dependants aged 5-29 years studying away from home? (Yes-1, No-2)
# names(ind64)[40]<-"no_of_dependants"
# names(ind64)[41]<-"amt_dependants"
# names(ind64)[42]<-"primaryclass_dist_nearest_school"
# names(ind64)[43]<-"upperprimaryclass_dist_nearest_school"
# names(ind64)[44]<-"secclass_dist_nearest_school"
# names(ind64)[45]<-"purchase_hh_cons_exp_30days"
# names(ind64)[46]<-"homeproducedstock_hh_cons_exp_30days"
# names(ind64)[47]<-"goodsnservices_hh_cons_exp_30days"
# names(ind64)[48]<-"giftsnloans_hh_cons_exp_30days"
# names(ind64)[49]<-"freecollection_hh_cons_exp_30days"
# names(ind64)[50]<-"hh_exp_month"
ind64$year<-2007

## reversal of gender coding 2007
ind64$sex[ind64$sex==1]<-0
ind64$sex[ind64$sex==2]<-1
ind64$sex[ind64$sex==0]<-2

## select variables
ind64<- subset(ind64, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                               "state_region_district","relation_to_head","sex","age","marital_status","edu_level",
                               "level","round","schedule_no","sample","FOD_subregion"))


##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
ind64<- ind64 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
ind64$sector[ind64$sector==1] <- "rural"
ind64$sector[ind64$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss64_25.2_new format/district_lookup_64.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"
ind64 <- ind64 %>% left_join(district_lookup, by="state_region_district")

#### 3.4 Block-5 Education particulars of those aged 5-29 years who are currently attending primary level and above ####

names(edu64)[3]<-"round"
names(edu64)[4]<-"schedule_no"
names(edu64)[5]<-"sample"
names(edu64)[6]<-"sector"
names(edu64)[7]<-"state"
names(edu64)[14]<-"FOD_subregion"
names(edu64)[18]<-"level"
names(edu64)[19]<-"psrl_no"
names(edu64)[20]<-"age"
names(edu64)[21]<-"age_school_entry"
# names(edu64)[22]<-"no_of_courses_attended"
# names(edu64)[23]<-"course_number"
# names(edu64)[24]<-"type_current_edu"
names(edu64)[25]<-"course_curr_attend"
names(edu64)[26]<-"edu_level_curr_attend"
names(edu64)[27]<-"present_class_study" #for example: btech 1st year is 12+1=13
names(edu64)[28]<-"study_prev_year" #to collect info about repeaters
names(edu64)[29]<-"type_of_inst"
names(edu64)[30]<-"nature_of_inst"
names(edu64)[31]<-"medium_of_instruction"
names(edu64)[32]<-"type_of_course"
names(edu64)[33]<-"is_edu_free"
# names(edu64)[34]<-"is_tuition_fee_waived"
names(edu64)[35]<-"amt_waiv" #if tuition fee is waived
# names(edu64)[36]<-"reason_for_waiver"
names(edu64)[37]<-"schlor"
names(edu64)[38]<-"schlor_amt" #if receiving scholarship/stipend
names(edu64)[39]<-"reason_schlor" #Is the household incurring any expenditure during current academic session/year on dependants aged 5-29 years studying away from home? (Yes-1, No-2)
names(edu64)[40]<-"textbooks"
names(edu64)[41]<-"stationery"
names(edu64)[42]<-"midday_meal"
names(edu64)[43]<-"agency_meal"
names(edu64)[44]<-"dist_inst"
names(edu64)[45]<-"mode_of_transport"
names(edu64)[46]<-"concession_pt" #for students availing public transport
names(edu64)[47]<-"changed_edu_inst"
names(edu64)[48]<-"hh_id"
names(edu64)[49]<-"person_id"
names(edu64)[50]<-"hh_wgt"
names(edu64)[51]<-"state_region"
names(edu64)[52]<-"state_region_district"
# names(edu64)[53]<-"relation_to_head"
# names(edu64)[54]<-"sex"
# names(edu64)[55]<-"marital_status"
# names(edu64)[56]<-"edu_level"
# names(edu64)[57]<-"edu_attend"
# names(edu64)[58]<-"edu_enroll"
# names(edu64)[59]<-"hh_size"
# # names(edu64)[60]<-"nic_2004"
# # names(edu64)[61]<-"nco_2004"
# names(edu64)[62]<-"hh_type"
# names(edu64)[63]<-"religion"
# names(edu64)[64]<-"social_group"
# names(edu64)[65]<-"land_possess"
# names(edu64)[66]<-"hh_exp_dependants" #Is the household incurring any expenditure during current academic session/year on dependants aged 5-29 years studying away from home? (Yes-1, No-2)
# names(edu64)[67]<-"no_of_dependants"
# names(edu64)[68]<-"amt_dependants"
# names(edu64)[69]<-"primaryclass_dist_nearest_school"
# names(edu64)[70]<-"upperprimaryclass_dist_nearest_school"
# names(edu64)[71]<-"secclass_dist_nearest_school"
# names(edu64)[77]<-"hh_exp_month"
edu64$reason_pvt_inst <- NA
edu64$pvt_coaching <- NA
edu64$agency_schlor <-NA
edu64$year<-2007
edu64$study_prev_year[edu64$study_prev_year==0]<-NA
edu64$present_year <- (edu64$present_class_study-edu64$study_prev_year)
edu64$present_year[edu64$present_year>=1]<-2
edu64$present_year[edu64$present_year==0]<-1

## select variables
edu64<- subset(edu64, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                               "state_region_district","age","age_school_entry","course_curr_attend","present_class_study",
                               "present_year","type_of_inst","reason_pvt_inst","nature_of_inst",
                               "medium_of_instruction","edu_level_curr_attend","type_of_course","is_edu_free",
                               "amt_waiv","schlor",
                               "schlor_amt","reason_schlor","agency_schlor","textbooks","stationery",
                               "midday_meal","agency_meal","mode_of_transport","dist_inst",
                               "concession_pt","changed_edu_inst","pvt_coaching",
                               "level","round","schedule_no","sample","FOD_subregion"))


##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
edu64<- edu64 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
edu64$sector[edu64$sector==1] <- "rural"
edu64$sector[edu64$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss64_25.2_new format/district_lookup_64.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"

edu64 <- edu64 %>% left_join(district_lookup, by="state_region_district")

##codes for edu_level_curr_attend
edu64$edu_level_curr_attend[which(edu64$edu_level_curr_attend %in% 7)]<-"primary"
edu64$edu_level_curr_attend[which(edu64$edu_level_curr_attend %in% 8)]<-"upper primary/middle"
edu64$edu_level_curr_attend[which(edu64$edu_level_curr_attend %in% 10)]<-"secondary"
edu64$edu_level_curr_attend[which(edu64$edu_level_curr_attend %in% 11)]<-"higher secondary"
edu64$edu_level_curr_attend[which(edu64$edu_level_curr_attend %in% c(21,22,23,24,29))]<-"diploma/certificate (below graduation)"
edu64$edu_level_curr_attend[which(edu64$edu_level_curr_attend %in% c(31,32,33,34,39))]<-"diploma/certificate (graduation & above)"
edu64$edu_level_curr_attend[which(edu64$edu_level_curr_attend %in% c(41,42,43,44,49))]<-"graduate"
edu64$edu_level_curr_attend[which(edu64$edu_level_curr_attend %in% c(51,52,53,54,59))]<-"post graduate and above"

##codes for type of institution
edu64$type_of_inst[which(edu64$type_of_inst %in% c(1,2))]<-"government"
edu64$type_of_inst[which(edu64$type_of_inst %in% c(3))]<-"pvt aided"
edu64$type_of_inst[which(edu64$type_of_inst %in% c(4))]<-"pvt unaided"
edu64$type_of_inst[which(edu64$type_of_inst %in% c(5))]<-NA

##changed edu inst in last one year
edu64$changed_edu_inst[which(edu64$changed_edu_inst %in% c(1))]<-"no"
edu64$changed_edu_inst[which(edu64$changed_edu_inst %in% c(2))]<-"govt to pvt"
edu64$changed_edu_inst[which(edu64$changed_edu_inst %in% c(3))]<-"pvt to govt"
edu64$changed_edu_inst[which(edu64$changed_edu_inst %in% c(4))]<-"govt to govt"
edu64$changed_edu_inst[which(edu64$changed_edu_inst %in% c(5))]<-"pvt to pvt"



#### 3.5 Block-6  Particulars of private expenditure for those aged 5-29 years who are currently attending at primary level and above ####

names(exp64)[3]<-"round"
names(exp64)[4]<-"schedule_no"
names(exp64)[5]<-"sample"
names(exp64)[6]<-"sector"
names(exp64)[7]<-"state"
names(exp64)[14]<-"FOD_subregion"
names(exp64)[18]<-"level"
names(exp64)[19]<-"psrl_no"
names(exp64)[20]<-"age"
names(exp64)[21]<-"tuition_fee"
names(exp64)[22]<-"exam_fee"
names(exp64)[23]<-"other_fee"
names(exp64)[24]<-"exp_books"
names(exp64)[25]<-"exp_stationery"
names(exp64)[26]<-"exp_uniform"
names(exp64)[27]<-"exp_transport"
names(exp64)[28]<-"hh_id"
names(exp64)[29]<-"person_id"
names(exp64)[30]<-"hh_wgt"
names(exp64)[31]<-"state_region"
names(exp64)[32]<-"state_region_district"
names(exp64)[33]<-"exp_pvtcoaching"
names(exp64)[34]<-"exp_other"
names(exp64)[35]<-"totalexp_course" #course 1 is basic course acc to NSS definition
# names(exp64)[36]<-"exp_course2"
# names(exp64)[37]<-"exp_other_courses"
# names(exp64)[38]<-"totalexp_course"
# names(exp64)[39]<-"if_given_donation"
# names(exp64)[40]<-"amt_donation_paid"
# names(exp64)[41]<-"agency_donation_paid"
# names(exp64)[42]<-"relation_to_head"
# names(exp64)[43]<-"sex"
# names(exp64)[44]<-"marital_status"
# names(exp64)[45]<-"edu_level"
# names(exp64)[46]<-"edu_attend"
# names(exp64)[47]<-"edu_enroll"
# names(exp64)[48]<-"hh_size"
# names(exp64)[49]<-"nic_2004"
# names(exp64)[50]<-"nco_2004"
# names(exp64)[51]<-"hh_type"
# names(exp64)[52]<-"religion"
# names(exp64)[53]<-"social_group"
# names(exp64)[54]<-"land_possess"
# names(exp64)[55]<-"hh_exp_dependants" #Is the household incurring any expenditure during current academic session/year on dependants aged 5-29 years studying away from home? (Yes-1, No-2)
# names(exp64)[56]<-"no_of_dependants"
# names(exp64)[57]<-"amt_dependants"
# names(exp64)[58]<-"primaryclass_dist_nearest_school"
# names(exp64)[59]<-"upperprimaryclass_dist_nearest_school"
# names(exp64)[60]<-"secclass_dist_nearest_school"
# names(exp64)[61]<-"purchase_hh_cons_exp_30days"
# names(exp64)[62]<-"homeproducedstock_hh_cons_exp_30days"
# names(exp64)[63]<-"goodsnservices_hh_cons_exp_30days"
# names(exp64)[64]<-"giftsnloans_hh_cons_exp_30days"
# names(exp64)[65]<-"freecollection_hh_cons_exp_30days"
# names(exp64)[66]<-"hh_exp_month"
exp64$course_fee <- rowSums(exp64[,c("tuition_fee", "exam_fee","other_fee")], na.rm=TRUE)
exp64$exp_books_stat_uniform <- rowSums(exp64[,c("exp_books", "exp_stationery","exp_uniform")], na.rm=TRUE)
exp64$year<-2007

## select variables
exp64<- subset(exp64, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                               "state_region_district","age","course_fee","exp_books_stat_uniform",
                               "exp_transport","exp_pvtcoaching","exp_other","totalexp_course",
                               "level","round","schedule_no","sample","FOD_subregion"))


##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
exp64<- exp64 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
exp64$sector[exp64$sector==1] <- "rural"
exp64$sector[exp64$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss64_25.2_new format/district_lookup_64.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"
exp64 <- exp64 %>% left_join(district_lookup, by="state_region_district")

#### 3.6 Block-7  Particulars of currently not attending persons aged 5-29 years ####

names(nonedu64)[2]<-"FSU_slno"
names(nonedu64)[3]<-"round"
names(nonedu64)[4]<-"schedule_no"
names(nonedu64)[5]<-"sample"
names(nonedu64)[6]<-"sector"
names(nonedu64)[7]<-"state"
names(nonedu64)[14]<-"FOD_subregion"
names(nonedu64)[15]<-"hg_sb_no"
names(nonedu64)[16]<-"sss_no"
names(nonedu64)[17]<-"hhsample_no"
names(nonedu64)[18]<-"level"
names(nonedu64)[19]<-"psrl_no"
names(nonedu64)[20]<-"age"
names(nonedu64)[21]<-"ever_enrolled"
names(nonedu64)[22]<-"age_first_enroll"
names(nonedu64)[23]<-"level_last_enroll"
names(nonedu64)[24]<-"edu_enrolled"
names(nonedu64)[25]<-"if_completed_lastenroll_level"
names(nonedu64)[26]<-"class_comp_drop"
names(nonedu64)[27]<-"age_discontinued" #discontinued and dropped are same for this survey
names(nonedu64)[28]<-"inst_type_last"
names(nonedu64)[29]<-"reason_never_enroll"
# names(nonedu64)[30]<-"usual_principal_activity"
names(nonedu64)[31]<-"hh_id"
names(nonedu64)[32]<-"person_id"
names(nonedu64)[33]<-"hh_wgt"
names(nonedu64)[34]<-"state_region"
names(nonedu64)[35]<-"state_region_district"
# names(nonedu64)[36]<-"relation_to_head"
# names(nonedu64)[37]<-"sex"
# names(nonedu64)[38]<-"marital_status"
# names(nonedu64)[39]<-"edu_level"
# names(nonedu64)[40]<-"edu_attend"
# names(nonedu64)[41]<-"edu_enroll"
# names(nonedu64)[42]<-"hh_size"
# names(nonedu64)[43]<-"nic_2004"
# names(nonedu64)[44]<-"nco_2004"
# names(nonedu64)[45]<-"hh_type"
# names(nonedu64)[46]<-"religion"
# names(nonedu64)[47]<-"social_group"
# names(nonedu64)[48]<-"land_possess"
# names(nonedu64)[49]<-"hh_exp_dependants" #Is the household incurring any expenditure during current academic session/year on dependants aged 5-29 years studying away from home? (Yes-1, No-2)
# names(nonedu64)[50]<-"no_of_dependants"
# names(nonedu64)[51]<-"amt_dependants"
# names(nonedu64)[52]<-"primaryclass_dist_nearest_school"
# names(nonedu64)[53]<-"upperprimaryclass_dist_nearest_school"
# names(nonedu64)[54]<-"secclass_dist_nearest_school"
# names(nonedu64)[55]<-"purchase_hh_cons_exp_30days"
# names(nonedu64)[56]<-"homeproducedstock_hh_cons_exp_30days"
# names(nonedu64)[57]<-"goodsnservices_hh_cons_exp_30days"
# names(nonedu64)[58]<-"giftsnloans_hh_cons_exp_30days"
# names(nonedu64)[59]<-"freecollection_hh_cons_exp_30days"
# names(nonedu64)[60]<-"hh_exp_month"
nonedu64$year<-2007

## select variables
nonedu64<- subset(nonedu64, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                                     "state_region_district","age","age_first_enroll","level_last_enroll",
                                     "edu_enrolled","if_completed_lastenroll_level","class_comp_drop",
                                     "age_discontinued","inst_type_last","reason_never_enroll",
                                     "level","round","schedule_no","sample","FOD_subregion"))


##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
nonedu64<- nonedu64 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
nonedu64$sector[nonedu64$sector==1] <- "rural"
nonedu64$sector[nonedu64$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss64_25.2_new format/district_lookup_64.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"
nonedu64 <- nonedu64 %>% left_join(district_lookup, by="state_region_district")



#### 4. Read data for round 2 of our analysis NSS 71: 2014 (Jan 2014-June 2014) #### 

hh_identify71 <- read.table('Nss71_25.2_new format/Block-1&2- Level-01-Identification of sample household and particulars of field operations.txt', header = TRUE, sep="\t")
hhs71 <- read.table('Nss71_25.2_new format/Block-3-Level-02 Household Characteristics.txt', header = TRUE, sep="\t")
ind71 <- read.table('Nss71_25.2_new format/Block-4 - Level -03 Demographic and other particulars of Household members.txt', header = TRUE, sep="\t")
edu71 <- read.table('Nss71_25.2_new format/Block-5 - Level-05 education particulars on basic course.txt', header = TRUE, sep="\t")
exp71 <- read.table('Nss71_25.2_new format/Block-6 - Level-04 Particulars of expenditure.txt', header = TRUE, sep="\t")
nonedu71 <- read.table('Nss71_25.2_new format/Block-7 - Level-06 Particulars of persons currently not attending any educational institute.txt', header = TRUE, sep="\t")

#### 4.1 Block-1&2- Level-01-Identification of sample household and particulars of field operations ####

names(hh_identify71)[1]<-"hh_id"
names(hh_identify71)[4]<-"round"
names(hh_identify71)[5]<-"schedule_no"
names(hh_identify71)[6]<-"sample"
names(hh_identify71)[7]<-"sector"
names(hh_identify71)[8]<-"state_region"
names(hh_identify71)[9]<-"state"
#In round 64 it is state, region, district but this round has only state and district, so we have to use different lookup tables
names(hh_identify71)[11]<-"state_region_district" 
names(hh_identify71)[16]<-"FOD_subregion"
names(hh_identify71)[20]<-"level"
names(hh_identify71)[28]<-"date_of_survey"
names(hh_identify71)[39]<-"hh_wgt"
hh_identify71$year <- 2014

## select variables
hh_identify71<- subset(hh_identify71, select=c("year","hh_id","hh_wgt","sector","state","state_region",
                                               "state_region_district","date_of_survey","level",
                                               "round","schedule_no","sample","FOD_subregion"))

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
hh_identify71$state[hh_identify71$state==36]<-28
hh_identify71<- hh_identify71 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
hh_identify71$sector[hh_identify71$sector==1] <- "rural"
hh_identify71$sector[hh_identify71$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss71_25.2_new format/district_lookup_71.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"
hh_identify71<- hh_identify71 %>% left_join(district_lookup, by="state_region_district")


#### 4.2 Block-3-Level-02 Household Characteristics ####

names(hhs71)[1]<-"hh_id"
names(hhs71)[4]<-"round"
names(hhs71)[5]<-"schedule_no"
names(hhs71)[6]<-"sample"
names(hhs71)[7]<-"sector"
names(hhs71)[8]<-"state_region"
names(hhs71)[9]<-"state"
names(hhs71)[11]<-"state_region_district"
names(hhs71)[16]<-"FOD_subregion"
names(hhs71)[20]<-"level"
names(hhs71)[21]<-"hh_size"
names(hhs71)[22]<-"nic"
names(hhs71)[23]<-"nco"
names(hhs71)[24]<-"hh_type"
names(hhs71)[25]<-"religion"
names(hhs71)[26]<-"social_group"
names(hhs71)[27]<-"primaryclass_dist_nearest_school"
names(hhs71)[28]<-"upperprimaryclass_dist_nearest_school"
names(hhs71)[29]<-"secclass_dist_nearest_school"
names(hhs71)[30]<-"hh_comp"
names(hhs71)[31]<-"hh_internet" #Whether any member has access to internet
names(hhs71)[32]<-"hh_exp_month"
# names(hhs71)[33]<-"nss"
# names(hhs71)[34]<-"nsc"
names(hhs71)[36]<-"hh_wgt"
hhs71$year<-2014
#hh monthly expenditure per capita
hhs71$hhexp_percapita <- hhs71$hh_exp_month/hhs71$hh_size

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
hhs71$state[hhs71$state==36]<-28
hhs71<- hhs71 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
hhs71$sector[hhs71$sector==1] <- "rural"
hhs71$sector[hhs71$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss71_25.2_new format/district_lookup_71.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"
hhs71<- hhs71 %>% left_join(district_lookup, by="state_region_district")

##divide hh monthly expenditure per capita into 5 quintiles
#specify survey design: strata at district and sector (rural/urban); household weights (hh_wgt)
svyd_hhs71 <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=hhs71)
svyquantile(~hhexp_percapita, subset(svyd_hhs71, !is.na(hhs71$hhexp_percapita)), quantiles = seq(0, 1 , by=0.2))
hhs71$hepc_q <- cut(hhs71$hhexp_percapita, breaks = c(0,900,1250,1625,2500,62501),
                    labels = c("Q1","Q2","Q3","Q4","Q5"),na.rm=T)

#calculate standardised monthly per capita expenditure 
hepc_mean <- svymean(~hhexp_percapita, svyd_hhs71, na=TRUE)
hepc_sd <- svysd(~hhexp_percapita, svyd_hhs71, na=TRUE)
hhs71$std_mpce <- (hhs71$hhexp_percapita-hepc_mean)/hepc_sd

##hh type
hhs71$hh_type[which(hhs71$hh_type %in% c(9))]<-"others"
hhs71$hh_type[which(hhs71$sector=="rural" & hhs71$hh_type %in% c(2))]<-"self-employed non-agriculture"
hhs71$hh_type[which(hhs71$sector=="rural" & hhs71$hh_type %in% c(1))]<-"self-employed agriculture"
hhs71$hh_type[which(hhs71$sector=="rural" & hhs71$hh_type %in% c(4))]<-"agricultural labour"
hhs71$hh_type[which(hhs71$sector=="rural" & hhs71$hh_type %in% c(5))]<-"other labour"
hhs71$hh_type[which(hhs71$sector=="urban" & hhs71$hh_type %in% c(1,2))]<-"self-employed"
hhs71$hh_type[which(hhs71$hh_type %in% c(3))]<-"regular wage/salary"
hhs71$hh_type[which(hhs71$sector=="urban" & hhs71$hh_type %in% c(4,5))]<-"casual labour"

##select variables
hhs71<- subset(hhs71, select=c("year","hh_id","hh_wgt","sector","state","state_region",
                               "state_region_district","hh_size","nic","nco",
                               "hh_type","religion","social_group","primaryclass_dist_nearest_school",
                               "upperprimaryclass_dist_nearest_school","secclass_dist_nearest_school",
                               "hh_exp_month","hh_comp","hh_internet",
                               "level","round","schedule_no","sample","FOD_subregion","hhexp_percapita","hepc_q","std_mpce"))


#### 4.3 Block-4 - Level -03 Demographic and other particulars of Household members ####

names(ind71)[1]<-"hh_id"
names(ind71)[4]<-"round"
names(ind71)[5]<-"schedule_no"
names(ind71)[6]<-"sample"
names(ind71)[7]<-"sector"
names(ind71)[8]<-"state_region"
names(ind71)[9]<-"state"
names(ind71)[11]<-"state_region_district"
names(ind71)[16]<-"FOD_subregion"
names(ind71)[20]<-"level"
names(ind71)[21]<-"psrl_no"
names(ind71)[22]<-"relation_to_head"
names(ind71)[23]<-"sex"
names(ind71)[24]<-"age"
names(ind71)[25]<-"marital_status"
names(ind71)[26]<-"edu_level" #highest level successfully completed
# names(ind71)[27]<-"comp_operate"
# names(ind71)[28]<-"comp_word"
# names(ind71)[29]<-"internet_search"
# names(ind71)[30]<-"internet_email"
# names(ind71)[31]<-"edu_attend" #indicates whether the person is currently attending any educational institution or not
# names(ind71)[32]<-"student_hostel"
# names(ind71)[33]<-"edu_enroll" #who is attending an educational institution, is necessarily enrolled in that institution
names(ind71)[37]<-"hh_wgt"
ind71$psrl_no <- str_remove(ind71$psrl_no,"^0+")
ind71$person_id <- paste0(ind71$hh_id,ind71$psrl_no)
ind71$year<-2014

## select variables
ind71<- subset(ind71, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                               "state_region_district","relation_to_head","sex","age","marital_status","edu_level",
                               "level","round","schedule_no","sample","FOD_subregion"))

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
ind71$state[ind71$state==36]<-28
ind71<- ind71 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
ind71$sector[ind71$sector==1] <- "rural"
ind71$sector[ind71$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss71_25.2_new format/district_lookup_71.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"
ind71<- ind71 %>% left_join(district_lookup, by="state_region_district")

#### 4.4 Block-5 - Level-05 education particulars on basic course ####

names(edu71)[1]<-"hh_id"
names(edu71)[4]<-"round"
names(edu71)[5]<-"schedule_no"
names(edu71)[6]<-"sample"
names(edu71)[7]<-"sector"
names(edu71)[8]<-"state_region"
names(edu71)[9]<-"state"
names(edu71)[11]<-"state_region_district"
names(edu71)[16]<-"FOD_subregion"
names(edu71)[20]<-"level"
names(edu71)[21]<-"psrl_no"
names(edu71)[22]<-"age"
names(edu71)[23]<-"age_school_entry"
names(edu71)[24]<-"edu_level_curr_attend"
names(edu71)[25]<-"course_curr_attend"
# names(edu71)[26]<-"duration_acad_session" 
names(edu71)[27]<-"present_year" #to collect info about repeaters
names(edu71)[28]<-"type_of_inst"
names(edu71)[29]<-"nature_of_inst"
names(edu71)[30]<-"reason_pvt_inst"
names(edu71)[31]<-"medium_of_instruction"
# names(edu71)[32]<-"lang_home"
names(edu71)[33]<-"type_of_course"
names(edu71)[34]<-"is_edu_free"
# names(edu71)[35]<-"tut_fee_waiv"
names(edu71)[36]<-"amt_waiv" #if tuition fee is waived
# names(edu71)[37]<-"reason_waiv" #if tuition fee is waived
names(edu71)[38]<-"schlor"
names(edu71)[39]<-"schlor_amt" #if receiving scholarship/stipend
names(edu71)[40]<-"reason_schlor"
names(edu71)[41]<-"agency_schlor"
names(edu71)[42]<-"textbooks"
names(edu71)[43]<-"stationery"
names(edu71)[44]<-"midday_meal"
names(edu71)[45]<-"agency_meal"
names(edu71)[46]<-"mode_of_transport"
names(edu71)[47]<-"concession_pt" #for students availing public transport
names(edu71)[48]<-"dist_inst"
names(edu71)[49]<-"changed_edu_inst" #in last one year
names(edu71)[50]<-"pvt_coaching"
# names(edu71)[51]<-"purpose_pvt_coaching"
names(edu71)[55]<-"hh_wgt"
edu71$person_id <- paste0(edu71$hh_id,edu71$psrl_no)
edu71$year<-2014
edu71$present_class_study <- NA

##select variables
edu71<- subset(edu71, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                               "state_region_district","age","age_school_entry","course_curr_attend","present_class_study",
                               "present_year","type_of_inst","reason_pvt_inst","nature_of_inst",
                               "medium_of_instruction","edu_level_curr_attend","type_of_course","is_edu_free",
                               "amt_waiv","schlor",
                               "schlor_amt","reason_schlor","agency_schlor","textbooks","stationery",
                               "midday_meal","agency_meal","mode_of_transport","dist_inst",
                               "concession_pt","changed_edu_inst","pvt_coaching",
                               "level","round","schedule_no","sample","FOD_subregion"))

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
edu71$state[edu71$state==36]<-28
edu71<- edu71 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
edu71$sector[edu71$sector==1] <- "rural"
edu71$sector[edu71$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss71_25.2_new format/district_lookup_71.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"
edu71 <- edu71 %>% left_join(district_lookup, by="state_region_district")

##codes for edu_level_curr_attend
edu71$edu_level_curr_attend[which(edu71$edu_level_curr_attend %in% 7)]<-"primary"
edu71$edu_level_curr_attend[which(edu71$edu_level_curr_attend %in% 8)]<-"upper primary/middle"
edu71$edu_level_curr_attend[which(edu71$edu_level_curr_attend %in% c(10,12))]<-"secondary"
edu71$edu_level_curr_attend[which(edu71$edu_level_curr_attend %in% c(11,13))]<-"higher secondary"
edu71$edu_level_curr_attend[which(edu71$edu_level_curr_attend %in% 14)]<-"diploma/certificate (graduation & above)"
edu71$edu_level_curr_attend[which(edu71$edu_level_curr_attend %in% 15)]<-"graduate"
edu71$edu_level_curr_attend[which(edu71$edu_level_curr_attend %in% 16)]<-"post graduate and above"

##codes for type of institution
edu71$type_of_inst[which(edu71$type_of_inst %in% c(1))]<-"government"
edu71$type_of_inst[which(edu71$type_of_inst %in% c(2))]<-"pvt aided"
edu71$type_of_inst[which(edu71$type_of_inst %in% c(3))]<-"pvt unaided"
edu71$type_of_inst[which(edu71$type_of_inst %in% c(4))]<-NA

##changed edu inst in last one year
edu71$changed_edu_inst[which(edu71$changed_edu_inst %in% c(1))]<-"no"
edu71$changed_edu_inst[which(edu71$changed_edu_inst %in% c(2))]<-"govt to pvt"
edu71$changed_edu_inst[which(edu71$changed_edu_inst %in% c(3))]<-"pvt to govt"
edu71$changed_edu_inst[which(edu71$changed_edu_inst %in% c(4))]<-"govt to govt"
edu71$changed_edu_inst[which(edu71$changed_edu_inst %in% c(5))]<-"pvt to pvt"


#### 4.5 Block-6 - Level-04 Particulars of expenditure ####

names(exp71)[1]<-"hh_id"
names(exp71)[4]<-"round"
names(exp71)[5]<-"schedule_no"
names(exp71)[6]<-"sample"
names(exp71)[7]<-"sector"
names(exp71)[8]<-"state_region"
names(exp71)[9]<-"state"
names(exp71)[11]<-"state_region_district"
names(exp71)[16]<-"FOD_subregion"
names(exp71)[20]<-"level"
names(exp71)[21]<-"psrl_no"
names(exp71)[22]<-"age"
names(exp71)[23]<-"course_fee" #includes tuition fee, exam fee and other fee payments
names(exp71)[24]<-"exp_books_stat_uniform" 
names(exp71)[25]<-"exp_transport"
names(exp71)[26]<-"exp_pvtcoaching"
names(exp71)[27]<-"exp_other" #includes study tours etc.
names(exp71)[28]<-"totalexp_course"
# names(exp71)[29]<-"state_hostel"
names(exp71)[33]<-"hh_wgt"
exp71$person_id<-paste0(exp71$hh_id,exp71$psrl_no)
exp71$year<-2014

## select variables
exp71<- subset(exp71, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                               "state_region_district","age","course_fee","exp_books_stat_uniform",
                               "exp_transport","exp_pvtcoaching","exp_other","totalexp_course",
                               "level","round","schedule_no","sample","FOD_subregion"))

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
exp71$state[exp71$state==36]<-28
exp71<- exp71 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
exp71$sector[exp71$sector==1] <- "rural"
exp71$sector[exp71$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss71_25.2_new format/district_lookup_71.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"
exp71 <- exp71 %>% left_join(district_lookup, by="state_region_district")


#### 4.6 Block-7 - Level-06 Particulars of persons currently not attending any educational institute ####

names(nonedu71)[2]<-"hh_id"
names(nonedu71)[4]<-"round"
names(nonedu71)[5]<-"schedule_no"
names(nonedu71)[6]<-"sample"
names(nonedu71)[7]<-"sector"
names(nonedu71)[8]<-"state_region"
names(nonedu71)[9]<-"state"
names(nonedu71)[11]<-"state_region_district"
names(nonedu71)[16]<-"FOD_subregion"
names(nonedu71)[20]<-"level"
names(nonedu71)[21]<-"psrl_no"
names(nonedu71)[22]<-"age"
names(nonedu71)[23]<-"ever_enrolled"
names(nonedu71)[24]<-"age_first_enroll"
names(nonedu71)[25]<-"level_last_enroll"
names(nonedu71)[26]<-"edu_enrolled"
names(nonedu71)[27]<-"if_completed_lastenroll_level"
names(nonedu71)[28]<-"class_comp_drop"
names(nonedu71)[29]<-"age_discontinued" #discontinued and dropped are same for this survey
names(nonedu71)[30]<-"inst_type_last"
names(nonedu71)[31]<-"reason_never_enroll"
names(nonedu71)[35]<-"hh_wgt"
nonedu71$person_id<-paste0(nonedu71$hh_id,nonedu71$psrl_no)
nonedu71$year<-2014

## select variables
nonedu71<- subset(nonedu71, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                                     "state_region_district","age","age_first_enroll","level_last_enroll",
                                     "edu_enrolled","if_completed_lastenroll_level","class_comp_drop",
                                     "age_discontinued","inst_type_last","reason_never_enroll",
                                     "level","round","schedule_no","sample","FOD_subregion"))

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
nonedu71$state[nonedu71$state==36]<-28
nonedu71<- nonedu71 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
nonedu71$sector[nonedu71$sector==1] <- "rural"
nonedu71$sector[nonedu71$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("Nss71_25.2_new format/district_lookup_71.csv")
colnames(district_lookup)[1]<-"state_region_district"
colnames(district_lookup)[2]<-"district_name"
nonedu71 <- nonedu71 %>% left_join(district_lookup, by="state_region_district")


#### 5. Read data for round 3 of our analysis NSS 75: 2017-18 (July 2017-June 2018) ####

hh_identify75 <- read.table('NSS round 75/Level-01 Blocks-1,2,11 Identification of households.txt', header = TRUE, sep="\t")
hhs75 <- read.table('NSS round 75/Level-02 Block 3 Household characteristics.txt', header = TRUE, sep="\t")
ind_edu75 <- read.table('NSS round 75/Level-03 Block 3.1  household members of age 3 to 35 years currently attending education.txt', header = TRUE, sep="\t")
ind75 <- read.table('NSS round 75/Level-04 Block-4 demographic and other particulars of household members.txt', header = TRUE, sep="\t")
edu75 <- read.table('NSS round 75/Level-05 Block-5  education particulars on basic course of the persons of age 3 to 35 years who are currently attending education.txt', header = TRUE, sep="\t")
exp75 <- read.table('NSS round 75/Level-06 Block-6  particulars of expenditure for persons of age 3 to 35 years currently attending at pre-primary and above level.txt', header = TRUE, sep="\t")
nonedu75 <- read.table('NSS round 75/Level-07 Block-7 particulars of currently not attending persons of age 3 to 35 years.txt', header = TRUE, sep="\t")
# training75 <- read.table('NSS round 75/Level-08 Block-8 particulars of formal vocational or technical training received by household members of age 12-59 years.txt', header = TRUE, sep="\t")

#### 5.1 Level-01 Blocks-1,2,11 Identification of households ####

names(hh_identify75)[1] <- "char"

hh_identify75$FSU_slno <- as.numeric(substr(hh_identify75$char,4,8))
hh_identify75$round <- as.numeric(substr(hh_identify75$char,9,10))
hh_identify75$schedule_no <- as.numeric(substr(hh_identify75$char,11,13))
hh_identify75$sample <- as.numeric(substr(hh_identify75$char,14,14))
hh_identify75$sector <- as.numeric(substr(hh_identify75$char,15,15))
hh_identify75$state_region <- as.numeric(substr(hh_identify75$char,16,18))
hh_identify75$state_region_district <- as.numeric(substr(hh_identify75$char,19,20))
hh_identify75$FOD_subregion <- as.numeric(substr(hh_identify75$char,27,30))
hh_identify75$hg_sb_no <- as.numeric(substr(hh_identify75$char,31,31))
hh_identify75$sss_no <- as.numeric(substr(hh_identify75$char,32,32))
hh_identify75$hhsample_no <- as.numeric(substr(hh_identify75$char,33,34))
hh_identify75$level <- as.numeric(substr(hh_identify75$char,35,36))
hh_identify75$date_of_survey <- as.numeric(substr(hh_identify75$char,59,64))
hh_identify75$mult <- as.numeric(substr(hh_identify75$char,133,142))
hh_identify75$hh_wgt <- (hh_identify75$mult)/100
hh_identify75$hh_id <- paste0(hh_identify75$FSU_slno,hh_identify75$hg_sb_no,hh_identify75$sss_no,hh_identify75$hhsample_no)
hh_identify75$year <- 2017

hh_identify75$state_region <- str_pad(hh_identify75$state_region, 3, pad = "0")
hh_identify75$state <- as.numeric(substr(hh_identify75$state_region,1,2))

## select variables
hh_identify75<- subset(hh_identify75, select=c("year","hh_id","hh_wgt","sector","state","state_region",
                                               "state_region_district","date_of_survey","level",
                                               "round","schedule_no","sample","FOD_subregion"))

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
hh_identify75$state[hh_identify75$state==36]<-28
hh_identify75<- hh_identify75 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
hh_identify75$sector[hh_identify75$sector==1] <- "rural"
hh_identify75$sector[hh_identify75$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("NSS round 75/district_lookup_75.csv")
district_lookup$region_code <- str_pad(district_lookup$region_code, 3, pad = "0")
district_lookup$district_code <- gsub("[()]", "", district_lookup$district_code)
district_lookup$state_region_district <- paste0(district_lookup$region_code,district_lookup$district_code)
names(district_lookup)[names(district_lookup) == "name.of.district"] <- "district_name"
district_lookup <- subset(district_lookup, select=c("state_region_district","district_name"))

hh_identify75$state_region_district <- str_pad(hh_identify75$state_region_district, 2, pad = "0")
hh_identify75$state_region_district <- paste0(hh_identify75$state_region,hh_identify75$state_region_district)

hh_identify75<- hh_identify75 %>% left_join(district_lookup, by="state_region_district")


#### 5.2 Level-02 Block 3 Household characteristics ####

names(hhs75)[1] <- "char"

hhs75$FSU_slno <- as.numeric(substr(hhs75$char,4,8))
hhs75$round <- as.numeric(substr(hhs75$char,9,10))
hhs75$schedule_no <- as.numeric(substr(hhs75$char,11,13))
hhs75$sample <- as.numeric(substr(hhs75$char,14,14))
hhs75$sector <- as.numeric(substr(hhs75$char,15,15))
hhs75$state_region <- as.numeric(substr(hhs75$char,16,18))
hhs75$state_region_district <- as.numeric(substr(hhs75$char,19,20))
hhs75$FOD_subregion <- as.numeric(substr(hhs75$char,27,30))
hhs75$hg_sb_no <- as.numeric(substr(hhs75$char,31,31))
hhs75$sss_no <- as.numeric(substr(hhs75$char,32,32))
hhs75$hhsample_no <- as.numeric(substr(hhs75$char,33,34))
hhs75$level <- as.numeric(substr(hhs75$char,35,36))
hhs75$hh_size <- as.numeric(substr(hhs75$char,42,43))
hhs75$nic <- as.numeric(substr(hhs75$char,44,48)) #code is of 2008
hhs75$nco <- as.numeric(substr(hhs75$char,49,51))
hhs75$hh_type <- as.numeric(substr(hhs75$char,52,52))
hhs75$religion <- as.numeric(substr(hhs75$char,53,53))
hhs75$social_group <- as.numeric(substr(hhs75$char,54,54))
hhs75$primaryclass_dist_nearest_school <- as.numeric(substr(hhs75$char,55,55))
hhs75$upperprimaryclass_dist_nearest_school <- as.numeric(substr(hhs75$char,56,56))
hhs75$secclass_dist_nearest_school <- as.numeric(substr(hhs75$char,57,57))
hhs75$hh_comp <- as.numeric(substr(hhs75$char,58,58))
hhs75$hh_internet <- as.numeric(substr(hhs75$char,59,59))
# hhs75$hh_edu <- as.numeric(substr(hhs75$char,60,60))
# hhs75$student_hostel <- as.numeric(substr(hhs75$char,61,61))
# hhs75$hh_state <- as.numeric(substr(hhs75$char,62,63)) #if a student lives in hostel, location of parent household
# hhs75$hh_district <- as.numeric(substr(hhs75$char,64,65)) #if a student lives in hostel, location of parent household
# hhs75$hh_sector <- as.numeric(substr(hhs75$char,66,66)) #if a student lives in hostel, location of parent household
hhs75$hh_exp_month <- as.numeric(substr(hhs75$char,67,74)) 
hhs75$mult <- as.numeric(substr(hhs75$char,133,142))
hhs75$hh_wgt <- (hhs75$mult)/100
hhs75$hh_id <- paste0(hhs75$FSU_slno,hhs75$hg_sb_no,hhs75$sss_no,hhs75$hhsample_no)
hhs75$year <- 2017
hhs75$state_region <- str_pad(hhs75$state_region, 3, pad = "0")
hhs75$state <- as.numeric(substr(hhs75$state_region,1,2))
#hh monthly expenditure per capita
hhs75$hhexp_percapita <- hhs75$hh_exp_month/hhs75$hh_size

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
hhs75$state[hhs75$state==36]<-28
hhs75<- hhs75 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
hhs75$sector[hhs75$sector==1] <- "rural"
hhs75$sector[hhs75$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("NSS round 75/district_lookup_75.csv")
district_lookup$region_code <- str_pad(district_lookup$region_code, 3, pad = "0")
district_lookup$district_code <- gsub("[()]", "", district_lookup$district_code)
district_lookup$state_region_district <- paste0(district_lookup$region_code,district_lookup$district_code)
names(district_lookup)[names(district_lookup) == "name.of.district"] <- "district_name"
district_lookup <- subset(district_lookup, select=c("state_region_district","district_name"))

hhs75$state_region_district <- str_pad(hhs75$state_region_district, 2, pad = "0")
hhs75$state_region_district <- paste0(hhs75$state_region,hhs75$state_region_district)

hhs75<- hhs75 %>% left_join(district_lookup, by="state_region_district")

##divide hh monthly expenditure per capita into 5 quintiles
#specify survey design: strata at district and sector (rural/urban); household weights (hh_wgt)
svyd_hhs75 <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=hhs75)
svyquantile(~hhexp_percapita, subset(svyd_hhs75, !is.na(hhs75$hhexp_percapita)), quantiles = seq(0, 1 , by=0.2))
hhs75$hepc_q <- cut(hhs75$hhexp_percapita, breaks = c(0,1200,1625,2142.86,3250,80201),
                    labels = c("Q1","Q2","Q3","Q4","Q5"),na.rm=T)

#calculate standardised monthly per capita expenditure 
hepc_mean <- svymean(~hhexp_percapita, svyd_hhs75, na=TRUE)
hepc_sd <- svysd(~hhexp_percapita, svyd_hhs75, na=TRUE)
hhs75$std_mpce <- (hhs75$hhexp_percapita-hepc_mean)/hepc_sd

##hh type
hhs75$hh_type[which(hhs75$hh_type %in% c(9))]<-"others"
hhs75$hh_type[which(hhs75$sector=="rural" & hhs75$hh_type %in% c(2))]<-"self-employed non-agriculture"
hhs75$hh_type[which(hhs75$sector=="rural" & hhs75$hh_type %in% c(1))]<-"self-employed agriculture"
hhs75$hh_type[which(hhs75$sector=="rural" & hhs75$hh_type %in% c(3,5))]<-"agricultural labour"
hhs75$hh_type[which(hhs75$sector=="rural" & hhs75$hh_type %in% c(4,6))]<-"other labour"
hhs75$hh_type[which(hhs75$sector=="urban" & hhs75$hh_type %in% c(1))]<-"self-employed"
hhs75$hh_type[which(hhs75$sector=="urban" & hhs75$hh_type %in% c(2))]<-"regular wage/salary"
hhs75$hh_type[which(hhs75$sector=="urban" & hhs75$hh_type %in% c(3))]<-"casual labour"

##selecting variables
hhs75<- subset(hhs75, select=c("year","hh_id","hh_wgt","sector","state","state_region",
                               "state_region_district","hh_size","nic","nco",
                               "hh_type","religion","social_group","primaryclass_dist_nearest_school",
                               "upperprimaryclass_dist_nearest_school","secclass_dist_nearest_school",
                               "hh_exp_month","hh_comp","hh_internet",
                               "level","round","schedule_no","sample","FOD_subregion","hhexp_percapita","hepc_q","std_mpce"))

#### 5.3 Level-03 Block 3.1  household members of age 3 to 35 years currently attending education ####

names(ind_edu75)[1] <- "char"

ind_edu75$FSU_slno <- as.numeric(substr(ind_edu75$char,4,8))
ind_edu75$round <- as.numeric(substr(ind_edu75$char,9,10))
ind_edu75$schedule_no <- as.numeric(substr(ind_edu75$char,11,13))
ind_edu75$sample <- as.numeric(substr(ind_edu75$char,14,14))
ind_edu75$sector <- as.numeric(substr(ind_edu75$char,15,15))
ind_edu75$nss_region <- as.numeric(substr(ind_edu75$char,16,18))
ind_edu75$district <- as.numeric(substr(ind_edu75$char,19,20))
ind_edu75$FOD_subregion <- as.numeric(substr(ind_edu75$char,27,30))
ind_edu75$hg_sb_no <- as.numeric(substr(ind_edu75$char,31,31))
ind_edu75$sss_no <- as.numeric(substr(ind_edu75$char,32,32))
ind_edu75$hhsample_no <- as.numeric(substr(ind_edu75$char,33,34))
ind_edu75$level <- as.numeric(substr(ind_edu75$char,35,36))
ind_edu75$psrl_no <- as.numeric(substr(ind_edu75$char,40,41))
ind_edu75$sex <- as.numeric(substr(ind_edu75$char,42,42))
ind_edu75$age <- as.numeric(substr(ind_edu75$char,43,45))
ind_edu75$state <- as.numeric(substr(ind_edu75$char,46,47))
ind_edu75$district <- as.numeric(substr(ind_edu75$char,48,49))
ind_edu75$sector <- as.numeric(substr(ind_edu75$char,50,50))
ind_edu75$residence_type <- as.numeric(substr(ind_edu75$char,51,51))
ind_edu75$edu_enroll <- as.numeric(substr(ind_edu75$char,52,53))
ind_edu75$hh_exp_dependants <- as.numeric(substr(ind_edu75$char,54,54))
ind_edu75$amt_dependants <- as.numeric(substr(ind_edu75$char,55,62))
ind_edu75$nss <- as.numeric(substr(ind_edu75$char,127,129))
ind_edu75$nsc <- as.numeric(substr(ind_edu75$char,130,132))
ind_edu75$mult <- as.numeric(substr(ind_edu75$char,133,142))


#### 5.4 Level-04 Block-4 demographic and other particulars of household members ####

names(ind75)[1] <- "char"

ind75$FSU_slno <- as.numeric(substr(ind75$char,4,8))
ind75$round <- as.numeric(substr(ind75$char,9,10))
ind75$schedule_no <- as.numeric(substr(ind75$char,11,13))
ind75$sample <- as.numeric(substr(ind75$char,14,14))
ind75$sector <- as.numeric(substr(ind75$char,15,15))
ind75$state_region <- as.numeric(substr(ind75$char,16,18))
ind75$state_region_district <- as.numeric(substr(ind75$char,19,20))
ind75$FOD_subregion <- as.numeric(substr(ind75$char,27,30))
ind75$hg_sb_no <- as.numeric(substr(ind75$char,31,31))
ind75$sss_no <- as.numeric(substr(ind75$char,32,32))
ind75$hhsample_no <- as.numeric(substr(ind75$char,33,34))
ind75$level <- as.numeric(substr(ind75$char,35,36))
ind75$psrl_no <- as.numeric(substr(ind75$char,40,41))
ind75$relation_to_head <- as.numeric(substr(ind75$char,42,42))
ind75$sex <- as.numeric(substr(ind75$char,43,43))
ind75$age <- as.numeric(substr(ind75$char,44,46))
ind75$marital_status <- as.numeric(substr(ind75$char,47,47))
ind75$edu_level <- as.numeric(substr(ind75$char,48,49))
# ind75$technical_edu_level <- as.numeric(substr(ind75$char,50,51))
# ind75$class_completed <- as.numeric(substr(ind75$char,52,53))
# ind75$years_after_class_completed <- as.numeric(substr(ind75$char,54,55))
# ind75$enroll_status <- as.numeric(substr(ind75$char,56,56))
# ind75$age5yr_comp <- as.numeric(substr(ind75$char,57,57))
# ind75$age5yr_internet <- as.numeric(substr(ind75$char,58,58))
# ind75$age5yr_internet_30days <- as.numeric(substr(ind75$char,59,59))
# ind75$age12to59_training <- as.numeric(substr(ind75$char,60,60))
# ind75$disability_certi <- as.numeric(substr(ind75$char,61,61))
# ind75$type_of_disability <- as.numeric(substr(ind75$char,62,62))
ind75$mult <- as.numeric(substr(ind75$char,133,142))
ind75$year<-2014
ind75$hh_wgt <- (ind75$mult)/100
ind75$hh_id <- paste0(ind75$FSU_slno,ind75$hg_sb_no,ind75$sss_no,ind75$hhsample_no)
ind75$person_id <- paste0(ind75$hh_id,ind75$psrl_no)
ind75$year <- 2017
ind75$state_region <- str_pad(ind75$state_region, 3, pad = "0")
ind75$state <- as.numeric(substr(ind75$state_region,1,2))

##select variables
ind75<- subset(ind75, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                               "state_region_district","relation_to_head","sex","age","marital_status","edu_level",
                               "level","round","schedule_no","sample","FOD_subregion"))

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
ind75$state[ind75$state==36]<-28
ind75<- ind75 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
ind75$sector[ind75$sector==1] <- "rural"
ind75$sector[ind75$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("NSS round 75/district_lookup_75.csv")
district_lookup$region_code <- str_pad(district_lookup$region_code, 3, pad = "0")
district_lookup$district_code <- gsub("[()]", "", district_lookup$district_code)
district_lookup$state_region_district <- paste0(district_lookup$region_code,district_lookup$district_code)
names(district_lookup)[names(district_lookup) == "name.of.district"] <- "district_name"
district_lookup <- subset(district_lookup, select=c("state_region_district","district_name"))

ind75$state_region_district <- str_pad(ind75$state_region_district, 2, pad = "0")
ind75$state_region_district <- paste0(ind75$state_region,ind75$state_region_district)

ind75 <- ind75 %>% left_join(district_lookup, by="state_region_district")

#### 5.5 Level-05 Block-5  education particulars on basic course of the persons of age 3 to 35 years who are currently attending education ####

names(edu75)[1] <- "char"

edu75$FSU_slno <- as.numeric(substr(edu75$char,4,8))
edu75$round <- as.numeric(substr(edu75$char,9,10))
edu75$schedule_no <- as.numeric(substr(edu75$char,11,13))
edu75$sample <- as.numeric(substr(edu75$char,14,14))
edu75$sector <- as.numeric(substr(edu75$char,15,15))
edu75$state_region <- as.numeric(substr(edu75$char,16,18))
edu75$state_region_district <- as.numeric(substr(edu75$char,19,20))
edu75$FOD_subregion <- as.numeric(substr(edu75$char,27,30))
edu75$hg_sb_no <- as.numeric(substr(edu75$char,31,31))
edu75$sss_no <- as.numeric(substr(edu75$char,32,32))
edu75$hhsample_no <- as.numeric(substr(edu75$char,33,34))
edu75$level <- as.numeric(substr(edu75$char,35,36))
edu75$psrl_no <- as.numeric(substr(edu75$char,40,41))
edu75$age <- as.numeric(substr(edu75$char,42,44))
edu75$age_school_entry <- as.numeric(substr(edu75$char,45,46))
# edu75$lang_home <- as.numeric(substr(edu75$char,47,48))
edu75$medium_of_instruction <- as.numeric(substr(edu75$char,49,50))
edu75$edu_level_curr_attend <- as.numeric(substr(edu75$char,51,52))
edu75$course_curr_attend <- as.numeric(substr(edu75$char,53,54))
edu75$type_of_course <- as.numeric(substr(edu75$char,55,55))
# edu75$duration_acad_session <- as.numeric(substr(edu75$char,56,57))
edu75$present_year <- as.numeric(substr(edu75$char,58,58))
edu75$type_of_inst <- as.numeric(substr(edu75$char,59,59))
edu75$nature_of_inst <- as.numeric(substr(edu75$char,60,60))
edu75$reason_pvt_inst <- as.numeric(substr(edu75$char,61,61))
# edu75$diability_spcl_school <- as.numeric(substr(edu75$char,63,63))
edu75$is_edu_free <- as.numeric(substr(edu75$char,64,64))
# edu75$tut_fee_paid <- as.numeric(substr(edu75$char,65,65))
# edu75$reason_tut_fee <- as.numeric(substr(edu75$char,66,66)) #reason for tuition fee partly paid/partly payable or tuition fee not paid/not payable
edu75$amt_waiv <- as.numeric(substr(edu75$char,67,74))
edu75$schlor <- as.numeric(substr(edu75$char,75,75))
edu75$schlor_amt <- as.numeric(substr(edu75$char,76,83))
edu75$reason_schlor <- as.numeric(substr(edu75$char,84,84))
edu75$agency_schlor <- as.numeric(substr(edu75$char,85,85))
edu75$textbooks <- as.numeric(substr(edu75$char,86,86))
edu75$stationery <- as.numeric(substr(edu75$char,87,87))
edu75$midday_meal <- as.numeric(substr(edu75$char,88,88))
edu75$agency_meal <- as.numeric(substr(edu75$char,89,89))
edu75$mode_of_transport <- as.numeric(substr(edu75$char,90,90))
edu75$concession_pt <- as.numeric(substr(edu75$char,91,91))
edu75$dist_inst <- as.numeric(substr(edu75$char,92,92))
edu75$changed_edu_inst <- as.numeric(substr(edu75$char,93,93))
edu75$pvt_coaching <- as.numeric(substr(edu75$char,94,94))
edu75$mult <- as.numeric(substr(edu75$char,133,142))
edu75$hh_wgt <- (edu75$mult)/100
edu75$hh_id <- paste0(edu75$FSU_slno,edu75$hg_sb_no,edu75$sss_no,edu75$hhsample_no)
edu75$person_id <- paste0(edu75$hh_id,edu75$psrl_no)
edu75$state_region <- str_pad(edu75$state_region, 3, pad = "0")
edu75$state <- as.numeric(substr(edu75$state_region,1,2))
edu75$year<-2017  
edu75$present_class_study <- NA

## select variables
edu75<- subset(edu75, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                               "state_region_district","age","age_school_entry","course_curr_attend","present_class_study",
                               "present_year","type_of_inst","reason_pvt_inst","nature_of_inst",
                               "medium_of_instruction","edu_level_curr_attend","type_of_course","is_edu_free",
                               "amt_waiv","schlor","schlor_amt","reason_schlor","agency_schlor","textbooks",
                               "stationery","midday_meal","agency_meal","mode_of_transport","dist_inst",
                               "concession_pt","changed_edu_inst","pvt_coaching",
                               "level","round","schedule_no","sample","FOD_subregion"))

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
edu75$state[edu75$state==36]<-28
edu75<- edu75 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
edu75$sector[edu75$sector==1] <- "rural"
edu75$sector[edu75$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("NSS round 75/district_lookup_75.csv")
district_lookup$region_code <- str_pad(district_lookup$region_code, 3, pad = "0")
district_lookup$district_code <- gsub("[()]", "", district_lookup$district_code)
district_lookup$state_region_district <- paste0(district_lookup$region_code,district_lookup$district_code)
names(district_lookup)[names(district_lookup) == "name.of.district"] <- "district_name"
district_lookup <- subset(district_lookup, select=c("state_region_district","district_name"))

edu75$state_region_district <- str_pad(edu75$state_region_district, 2, pad = "0")
edu75$state_region_district <- paste0(edu75$state_region,edu75$state_region_district)

edu75 <- edu75 %>% left_join(district_lookup, by="state_region_district")

##codes for edu_level_curr_attend
# edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% 3)]<-"NFEC"
# edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% 4)]<-"TLC/AEC"
# edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% 5)]<-"other non-formal"
# edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% 6)]<-"pre-primary"

#see note on pg 12 of KI NSS75 document
edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% c(3,4,5))]<-"nonformal"
edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% 6)]<-"preprimary"
edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% 7)]<-"primary"
edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% 8)]<-"upper primary/middle"
edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% c(10,12))]<-"secondary"
edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% c(11,13))]<-"higher secondary"
edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% 14)]<-"diploma/certificate (graduation & above)"
edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% 15)]<-"graduate"
edu75$edu_level_curr_attend[which(edu75$edu_level_curr_attend %in% 16)]<-"post graduate and above"

##codes for type of institution
edu75$type_of_inst[which(edu75$type_of_inst %in% c(1))]<-"government"
edu75$type_of_inst[which(edu75$type_of_inst %in% c(2))]<-"pvt aided"
edu75$type_of_inst[which(edu75$type_of_inst %in% c(3))]<-"pvt unaided"
edu75$type_of_inst[which(edu75$type_of_inst %in% c(4))]<-NA

##changed edu inst in last one year
edu75$changed_edu_inst[which(edu75$changed_edu_inst %in% c(5))]<-"no"
edu75$changed_edu_inst[which(edu75$changed_edu_inst %in% c(1))]<-"govt to pvt"
edu75$changed_edu_inst[which(edu75$changed_edu_inst %in% c(2))]<-"pvt to govt"
edu75$changed_edu_inst[which(edu75$changed_edu_inst %in% c(3))]<-"govt to govt"
edu75$changed_edu_inst[which(edu75$changed_edu_inst %in% c(4))]<-"pvt to pvt"


#### 5.6 Level-06 Block-6  particulars of expenditure for persons of age 3 to 35 years currently attending at pre-primary and above level ####

names(exp75)[1] <- "char"

exp75$FSU_slno <- as.numeric(substr(exp75$char,4,8))
exp75$round <- as.numeric(substr(exp75$char,9,10))
exp75$schedule_no <- as.numeric(substr(exp75$char,11,13))
exp75$sample <- as.numeric(substr(exp75$char,14,14))
exp75$sector <- as.numeric(substr(exp75$char,15,15))
exp75$state_region <- as.numeric(substr(exp75$char,16,18))
exp75$state_region_district <- as.numeric(substr(exp75$char,19,20))
exp75$FOD_subregion <- as.numeric(substr(exp75$char,27,30))
exp75$hg_sb_no <- as.numeric(substr(exp75$char,31,31))
exp75$sss_no <- as.numeric(substr(exp75$char,32,32))
exp75$hhsample_no <- as.numeric(substr(exp75$char,33,34))
exp75$level <- as.numeric(substr(exp75$char,35,36))
exp75$psrl_no <- as.numeric(substr(exp75$char,40,41))
exp75$age <- as.numeric(substr(exp75$char,42,44))
exp75$course_fee <- as.numeric(substr(exp75$char,45,52))
exp75$exp_books_stat_uniform <- as.numeric(substr(exp75$char,53,60))
exp75$exp_transport <- as.numeric(substr(exp75$char,61,68))
exp75$exp_pvtcoaching <- as.numeric(substr(exp75$char,69,76))
exp75$exp_other <- as.numeric(substr(exp75$char,77,84))
exp75$totalexp_course <- as.numeric(substr(exp75$char,85,92))
# exp75$source_of_funding_first <- as.numeric(substr(exp75$char,93,94))
# exp75$source_of_funding_second <- as.numeric(substr(exp75$char,95,96))
# exp75$exp_edu_after_basic <- as.numeric(substr(exp75$char,97,104))
# exp75$exp_prep_higherstudy <- as.numeric(substr(exp75$char,105,112))
# exp75$work_more30days <- as.numeric(substr(exp75$char,113,113)) #for persons aged 15-35 years, whether worked for 30 days or more during last 365 days
# exp75$eco_act <- as.numeric(substr(exp75$char,114,114)) #status of economic activity for "yes" in prev ques
# exp75$available_for_work <- as.numeric(substr(exp75$char,115,115)) # whether seeking/available for work for "yes" in "work_more30days"
exp75$mult <- as.numeric(substr(exp75$char,133,142))
exp75$hh_wgt <- (exp75$mult)/100
exp75$hh_id <- paste0(exp75$FSU_slno,exp75$hg_sb_no,exp75$sss_no,exp75$hhsample_no)
exp75$person_id <- paste0(exp75$hh_id,exp75$psrl_no)
exp75$state_region <- str_pad(exp75$state_region, 3, pad = "0")
exp75$state <- as.numeric(substr(exp75$state_region,1,2))
exp75$year<-2017  

## select variables
exp75<- subset(exp75, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                               "state_region_district","age","course_fee","exp_books_stat_uniform",
                               "exp_transport","exp_pvtcoaching","exp_other","totalexp_course",
                               "level","round","schedule_no","sample","FOD_subregion"))

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
exp75$state[exp75$state==36]<-28
exp75<- exp75 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
exp75$sector[exp75$sector==1] <- "rural"
exp75$sector[exp75$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("NSS round 75/district_lookup_75.csv")
district_lookup$region_code <- str_pad(district_lookup$region_code, 3, pad = "0")
district_lookup$district_code <- gsub("[()]", "", district_lookup$district_code)
district_lookup$state_region_district <- paste0(district_lookup$region_code,district_lookup$district_code)
names(district_lookup)[names(district_lookup) == "name.of.district"] <- "district_name"
district_lookup <- subset(district_lookup, select=c("state_region_district","district_name"))

exp75$state_region_district <- str_pad(exp75$state_region_district, 2, pad = "0")
exp75$state_region_district <- paste0(exp75$state_region,exp75$state_region_district)

exp75 <- exp75 %>% left_join(district_lookup, by="state_region_district")

#### 5.7 Level-07 Block-7 particulars of currently not attending persons of age 3 to 35 years ####

names(nonedu75)[1] <- "char"

nonedu75$FSU_slno <- as.numeric(substr(nonedu75$char,4,8))
nonedu75$round <- as.numeric(substr(nonedu75$char,9,10))
nonedu75$schedule_no <- as.numeric(substr(nonedu75$char,11,13))
nonedu75$sample <- as.numeric(substr(nonedu75$char,14,14))
nonedu75$sector <- as.numeric(substr(nonedu75$char,15,15))
nonedu75$state_region <- as.numeric(substr(nonedu75$char,16,18))
nonedu75$state_region_district <- as.numeric(substr(nonedu75$char,19,20))
nonedu75$FOD_subregion <- as.numeric(substr(nonedu75$char,27,30))
nonedu75$hg_sb_no <- as.numeric(substr(nonedu75$char,31,31))
nonedu75$sss_no <- as.numeric(substr(nonedu75$char,32,32))
nonedu75$hhsample_no <- as.numeric(substr(nonedu75$char,33,34))
nonedu75$level <- as.numeric(substr(nonedu75$char,35,36))
nonedu75$psrl_no <- as.numeric(substr(nonedu75$char,40,41))
nonedu75$age <- as.numeric(substr(nonedu75$char,42,44))
nonedu75$ever_enrolled <- as.numeric(substr(nonedu75$char,45,45))
nonedu75$age_first_enroll <- as.numeric(substr(nonedu75$char,46,47))
nonedu75$level_last_enroll <- as.numeric(substr(nonedu75$char,48,49))
nonedu75$edu_enrolled <- as.numeric(substr(nonedu75$char,50,50))
nonedu75$if_completed_lastenroll_level <- as.numeric(substr(nonedu75$char,51,51))
nonedu75$class_comp_drop <- as.numeric(substr(nonedu75$char,52,53))
nonedu75$age_discontinued <- as.numeric(substr(nonedu75$char,54,55))
nonedu75$inst_type_last <- as.numeric(substr(nonedu75$char,56,56))
# nonedu75$if_prepare_higherstudy <- as.numeric(substr(nonedu75$char,57,57))
# nonedu75$exp_higherstudy <- as.numeric(substr(nonedu75$char,58,65))
nonedu75$reason_never_enroll <- as.numeric(substr(nonedu75$char,66,67))
nonedu75$mult <- as.numeric(substr(nonedu75$char,133,142))
nonedu75$hh_wgt <- (nonedu75$mult)/100
nonedu75$hh_id <- paste0(nonedu75$FSU_slno,nonedu75$hg_sb_no,nonedu75$sss_no,nonedu75$hhsample_no)
nonedu75$person_id <- paste0(nonedu75$hh_id,nonedu75$psrl_no)
nonedu75$state_region <- str_pad(nonedu75$state_region, 3, pad = "0")
nonedu75$state <- as.numeric(substr(nonedu75$state_region,1,2))
nonedu75$year<-2017  

## select variables
nonedu75<- subset(nonedu75, select=c("year","hh_id","psrl_no","person_id","hh_wgt","sector","state","state_region",
                                     "state_region_district","age","age_first_enroll","level_last_enroll",
                                     "edu_enrolled","if_completed_lastenroll_level","class_comp_drop",
                                     "age_discontinued","inst_type_last","reason_never_enroll",
                                     "level","round","schedule_no","sample","FOD_subregion"))

##joining state names with state code
state_lookup <- read.csv("Nss64_25.2_new format/state_lookup_64.csv")
#as Telangana was not present during vol 64, hence we include it in Andhra Pradesh
nonedu75$state[nonedu75$state==36]<-28
nonedu75<- nonedu75 %>% left_join(state_lookup, by="state")

##sector: 1-rural, 2-urban
nonedu75$sector[nonedu75$sector==1] <- "rural"
nonedu75$sector[nonedu75$sector==2] <- "urban"

##joining district names
district_lookup <- read.csv("NSS round 75/district_lookup_75.csv")
district_lookup$region_code <- str_pad(district_lookup$region_code, 3, pad = "0")
district_lookup$district_code <- gsub("[()]", "", district_lookup$district_code)
district_lookup$state_region_district <- paste0(district_lookup$region_code,district_lookup$district_code)
names(district_lookup)[names(district_lookup) == "name.of.district"] <- "district_name"
district_lookup <- subset(district_lookup, select=c("state_region_district","district_name"))

nonedu75$state_region_district <- str_pad(nonedu75$state_region_district, 2, pad = "0")
nonedu75$state_region_district <- paste0(nonedu75$state_region,nonedu75$state_region_district)

nonedu75 <- nonedu75 %>% left_join(district_lookup, by="state_region_district")



#### 5.8 Level-08 Block-8 particulars of formal vocational or technical training received by household members of age 12-59 years ####

# names(training75)[1] <- "char"
# 
# training75$FSU_slno <- as.numeric(substr(training75$char,4,8))
# training75$round <- as.numeric(substr(training75$char,9,10))
# training75$schedule_no <- as.numeric(substr(training75$char,11,13))
# training75$sample <- as.numeric(substr(training75$char,14,14))
# training75$sector <- as.numeric(substr(training75$char,15,15))
# training75$nss_region <- as.numeric(substr(training75$char,16,18))
# training75$district <- as.numeric(substr(training75$char,19,20))
# training75$FOD_subregion <- as.numeric(substr(training75$char,27,30))
# training75$hg_sb_no <- as.numeric(substr(training75$char,31,31))
# training75$sss_no <- as.numeric(substr(training75$char,32,32))
# training75$hhsample_no <- as.numeric(substr(training75$char,33,34))
# training75$level <- as.numeric(substr(training75$char,35,36))
# training75$psrl_no <- as.numeric(substr(training75$char,40,41))
# training75$age <- as.numeric(substr(training75$char,42,44))
# training75$field_of_training <- as.numeric(substr(training75$char,45,46))
# training75$duration_of_training <- as.numeric(substr(training75$char,47,47))
# training75$type_of_training <- as.numeric(substr(training75$char,48,48))
# training75$source_of_funding_training <- as.numeric(substr(training75$char,49,50))
# training75$training_completed_last365d <- as.numeric(substr(training75$char,51,51))
# training75$nss <- as.numeric(substr(training75$char,127,129))
# training75$nsc <- as.numeric(substr(training75$char,130,132))
# training75$mult <- as.numeric(substr(training75$char,133,142))


#### 6. Combine dataset (nss vol 64,71,75) for separate levels ####

hh_identify <- rbind(hh_identify64, hh_identify71, hh_identify75)
hhs <- rbind(hhs64, hhs71, hhs75)
ind <- rbind(ind64, ind71, ind75)
edu <- rbind(edu64, edu71, edu75)
exp <- rbind(exp64, exp71, exp75)
nonedu <- rbind(nonedu64, nonedu71, nonedu75)

#### 7. Assign unique household id and person id specific to year ####

hh_identify$unique_hhid <- paste0(hh_identify$year,"_",hh_identify$hh_id)
hhs$unique_hhid <- paste0(hhs$year,"_",hhs$hh_id)
ind$unique_hhid <- paste0(ind$year,"_",ind$hh_id)
edu$unique_hhid <- paste0(edu$year,"_",edu$hh_id)
exp$unique_hhid <- paste0(exp$year,"_",exp$hh_id)
nonedu$unique_hhid <- paste0(nonedu$year,"_",nonedu$hh_id)

ind$unique_pid <- paste0(ind$year,"_",ind$hh_id,"_",ind$psrl_no)
edu$unique_pid <- paste0(edu$year,"_",edu$hh_id,"_",edu$psrl_no)
exp$unique_pid <- paste0(exp$year,"_",exp$hh_id,"_",exp$psrl_no)
nonedu$unique_pid <- paste0(nonedu$year,"_",nonedu$hh_id,"_",nonedu$psrl_no)

#### 8. Household characteristics ####

hh_identify_join <- subset(hh_identify, select=c("unique_hhid","date_of_survey"))
hhs <- hhs %>% left_join(hh_identify_join, by="unique_hhid")

#### 9. Individual characteristics ####

hhs_join <- subset(hhs, select=c("unique_hhid","hh_size","nic","nco","hh_type",
                                 "religion","social_group","primaryclass_dist_nearest_school",
                                 "upperprimaryclass_dist_nearest_school","secclass_dist_nearest_school",
                                 "hh_exp_month","hh_comp","hh_internet","hhexp_percapita","hepc_q","std_mpce"))

ind = merge(x = ind, y = hhs_join, by = "unique_hhid", all.x = TRUE)

#### 10. Education and expenditure particulars ####

ind_join<-subset(ind, select=c("unique_pid","relation_to_head","sex","marital_status","edu_level",
                               "hh_size","nic","nco","hh_type","religion","social_group",
                               "primaryclass_dist_nearest_school","upperprimaryclass_dist_nearest_school",
                               "secclass_dist_nearest_school","hh_exp_month","hh_comp","hh_internet",
                               "hhexp_percapita","hepc_q","std_mpce"))

exp_join<-subset(exp, select=c("unique_pid","course_fee","exp_books_stat_uniform",
                               "exp_transport","exp_pvtcoaching","exp_other","totalexp_course"))

edu = merge(x = edu, y = ind_join, by = "unique_pid", all.x = TRUE)
edu = merge(x = edu, y = exp_join, by = "unique_pid", all.x = TRUE)

#### 11. Particulars of currently not attending persons ####

ind_join<-subset(ind, select=c("unique_pid","relation_to_head","sex","marital_status","edu_level",
                               "hh_size","nic","nco","hh_type","religion","social_group",
                               "primaryclass_dist_nearest_school","upperprimaryclass_dist_nearest_school",
                               "secclass_dist_nearest_school","hh_exp_month","hh_comp","hh_internet",
                               "hhexp_percapita","hepc_q","std_mpce"))

nonedu = merge(x = nonedu, y = ind_join, by = "unique_pid", all.x = TRUE)

#### 12. Education particulars for age group 5-29 years ####
# 2007 and 2014 data reported education details for persons aged 5-29 years 
# 2017 data reported education details for age group 3-35 years
# Therefore, for consistency, we select persons aged 5-29 years and reported attending education from the 2017 round

edu_5to29yrs <- edu[!(edu$year==2017 & (edu$age>29|edu$age<5)),] 


#### 13. Correcting spelling of states ####

edu$state_name[which(edu$state_name %in% c("Maharastra"))]<-"Maharashtra"
edu$state_name[which(edu$state_name %in% c("Uttaranchal"))]<-"Uttarakhand"
edu$state_name[which(edu$state_name %in% c("Pondicheri"))]<-"Puducherry"
edu$state_name[which(edu$state_name %in% c("Andhra Pardesh"))]<-"Andhra Pradesh"
edu$state_name[which(edu$state_name %in% c("Andaman & Nicober"))]<-"Andaman & Nicobar Islands"


#### 14. Selecting people of age 5-17 years who reported going to school ####
school_5to29yrs <- edu_5to29yrs[which(edu_5to29yrs$edu_level_curr_attend %in% c("primary","upper primary/middle",
                                                                                "secondary","higher secondary")),]
school_5to17yrs <- school_5to29yrs[school_5to29yrs$age %in% c(5:17),]

#### 15. Validating our survey design by matching it with estimates reported in NSSO publications ####

### Using 2007 data, find the percentage of people aged 5-29 years studying in 12th standard
class12_age <- edu64 %>% group_by(age, present_class_study) %>% summarise(count=n())
class12_age<-class12_age[class12_age$present_class_study==12,]

class12_age<-class12_age[!is.na(class12_age$age),]

class12_age$percent<-class12_age$count*100/3380

## See pg 40 of NSS64 report
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu64)
svytable(~edu_level_curr_attend, svyd)/2417454.13

### Check pg 17 of KI NSS75 document
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu75)
svytable(~edu_level_curr_attend, svyd)/5572105.74

svytable(~edu_level_curr_attend, subset(svyd, sector=="rural"))/3959321.06

svytable(~state_name+edu_level_curr_attend, subset(svyd, sector=="rural"))















