
rm(list=ls())

#### 1. Set working directory ####
setwd('')

#### 2. Read the file ####
school_5to17years

#### 3. Descriptive Summary ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

## age category
edu_temp$age_grp <- NA
edu_temp$age_grp<-cut(edu_temp$age,breaks = c(4, 10, 13, 15, 17),labels = c("5-10","11-13","14-15","16-17"),na.rm=T)
edu_temp$age_grp <- factor(edu_temp$age_grp, 
                           levels = c("5-10","11-13","14-15","16-17"), 
                           ordered = TRUE)

## caste
colnames(edu_temp)[colnames(edu_temp) == "social_group"] <- "caste"

edu_temp$caste[which(edu_temp$caste==1)]<-"ST"
edu_temp$caste[which(edu_temp$caste==2)]<-"SC"
edu_temp$caste[which(edu_temp$caste==3)]<-"OBC"
edu_temp$caste[which(edu_temp$caste==9)]<-"others"

edu_temp$caste <- factor(edu_temp$caste, 
                         levels = c("ST","SC","OBC","others"), 
                         ordered = TRUE)

## grade
edu_temp$edu_level_curr_attend <- factor(edu_temp$edu_level_curr_attend, 
                                         levels = c("primary","upper primary/middle","secondary","higher secondary"), 
                                         ordered = TRUE)


## mode of transport
edu_temp$mode_of_transport[which(edu_temp$mode_of_transport==1)]<-"walk"
edu_temp$mode_of_transport[which(edu_temp$mode_of_transport==2)]<-"school bus"
edu_temp$mode_of_transport[which(edu_temp$mode_of_transport==3)]<-"public transport"
edu_temp$mode_of_transport[which(edu_temp$mode_of_transport==4)]<-"bicycle"
edu_temp$mode_of_transport[which(edu_temp$mode_of_transport==9)]<-"others"

edu_temp$mode_of_transport <- factor(edu_temp$mode_of_transport, 
                                     levels = c("walk","school bus","public transport","bicycle","others"), 
                                     ordered = TRUE)


## distance to school
edu_temp$dist_inst[which(edu_temp$dist_inst==1)]<-"d<1 km"
edu_temp$dist_inst[which(edu_temp$dist_inst==2)]<-"1 km<=d<2 km"
edu_temp$dist_inst[which(edu_temp$dist_inst==3)]<-"2 km<=d<3 km"
edu_temp$dist_inst[which(edu_temp$dist_inst==4)]<-"3 km<=d<5 km"
edu_temp$dist_inst[which(edu_temp$dist_inst==5)]<-"d>=5 km"

edu_temp$dist_inst <- factor(edu_temp$dist_inst, 
                             levels = c("d<1 km","1 km<=d<2 km","2 km<=d<3 km","3 km<=d<5 km","d>=5 km"), 
                             ordered = TRUE)


## type of school
edu_temp$type_of_inst[which(edu_temp$type_of_inst %in% c("pvt aided","pvt unaided"))]<-"private"

## combine sex and sector
edu_temp$sex_sector[which(edu_temp$sex=="Female" & edu_temp$sector=="rural")]<-"RF"
edu_temp$sex_sector[which(edu_temp$sex=="Male" & edu_temp$sector=="rural")]<-"RM"
edu_temp$sex_sector[which(edu_temp$sex=="Female" & edu_temp$sector=="urban")]<-"UF"
edu_temp$sex_sector[which(edu_temp$sex=="Male" & edu_temp$sector=="urban")]<-"UM"


##survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

##age group
age_grp <- data.frame(svyby(~age_grp, by = ~year,
                            design = subset(svyd, sex_sector=="UF"), svymean))
age_grp$se.age_grp11.13 <- NULL
age_grp$se.age_grp5.10 <- NULL
age_grp$se.age_grp14.15 <- NULL
age_grp$se.age_grp16.17 <- NULL

age_grp <- age_grp %>%
  mutate_at(vars(2:5), ~ .*100)

age_grp<-age_grp %>%
  mutate_at(vars(2:5), ~ round(., 1))

rownames(age_grp) <- NULL

age_grp <-data.frame(t(age_grp)) 
age_grp <- age_grp[-1, ]


## grade enrolled
grade <- data.frame(svyby(~edu_level_curr_attend, by = ~year, subset(svyd, sex_sector=="UF" & !is.na(edu_level_curr_attend)), 
                          svymean))

grade$se.edu_level_curr_attendhigher.secondary <- NULL
grade$se.edu_level_curr_attendprimary <- NULL
grade$se.edu_level_curr_attendsecondary <- NULL
grade$se.edu_level_curr_attendupper.primary.middle <- NULL

grade <- grade %>%
  mutate_at(vars(2:5), ~ .*100)

grade <- grade %>%
  mutate_at(vars(2:5), ~ round(., 1))

rownames(grade) <- NULL

grade <- data.frame(t(grade))
grade <- grade[-1, ]


## mode of transport
transport <- data.frame(svyby(~mode_of_transport, by = ~year, subset(svyd, sex_sector=="UF" & !is.na(mode_of_transport)), 
                              svymean))

transport$se.mode_of_transportbicycle <- NULL
transport$se.mode_of_transportothers <- NULL
transport$se.mode_of_transportpublic.transport <- NULL
transport$se.mode_of_transportschool.bus <- NULL
transport$se.mode_of_transportwalk <- NULL

transport <- transport %>%
  mutate_at(vars(2:6), ~ .*100)

transport <- transport %>%
  mutate_at(vars(2:6), ~ round(., 1))

rownames(transport) <- NULL

transport <- data.frame(t(transport))
transport <- transport[-1, ]


## distance to school
distsch <- data.frame(svyby(~dist_inst, by = ~year, subset(svyd, sex_sector=="UF" & !is.na(dist_inst)), 
                            svymean))

distsch$se.dist_inst1.km..d.2.km <- NULL
distsch$se.dist_inst2.km..d.3.km <- NULL
distsch$se.dist_inst3.km..d.5.km <- NULL
distsch$se.dist_instd.1.km <- NULL
distsch$se.dist_instd..5.km <- NULL

distsch <- distsch %>%
  mutate_at(vars(2:6), ~ .*100)

distsch<-distsch %>%
  mutate_at(vars(2:6), ~ round(., 1))

rownames(distsch) <- NULL

distsch <- data.frame(t(distsch))
distsch <- distsch[-1, ]


## type of school
typesch <- data.frame(svyby(~type_of_inst, by = ~year, subset(svyd, sex_sector=="UF" & !is.na(type_of_inst)), 
                            svymean))

typesch$se.type_of_instgovernment <- NULL
typesch$se.type_of_instprivate <- NULL

typesch <- typesch %>%
  mutate_at(vars(2:3), ~ .*100)

typesch<-typesch %>%
  mutate_at(vars(2:3), ~ round(., 1))

rownames(typesch) <- NULL

typesch <- data.frame(t(typesch))
typesch <- typesch[-1, ]


## hh monthly exp per capita
income <- data.frame(svyby(~hepc_q, by = ~year, subset(svyd, sex_sector=="UF" & !is.na(hepc_q)), 
                           svymean))

income$se.hepc_qQ1 <- NULL
income$se.hepc_qQ2 <- NULL
income$se.hepc_qQ3 <- NULL
income$se.hepc_qQ4 <- NULL
income$se.hepc_qQ5 <- NULL

income <- income %>%
  mutate_at(vars(2:6), ~ .*100)

income<-income %>%
  mutate_at(vars(2:6), ~ round(., 1))

rownames(income) <- NULL

income <- data.frame(t(income))
income <- income[-1, ]


## caste
caste <- data.frame(svyby(~caste, by = ~year, subset(svyd, sex_sector=="UF" & !is.na(caste)), 
                          svymean))

caste$se.casteST <- NULL
caste$se.casteSC <- NULL
caste$se.casteOBC <- NULL
caste$se.casteothers <- NULL

caste <- caste %>%
  mutate_at(vars(2:5), ~ .*100)

caste<-caste %>%
  mutate_at(vars(2:5), ~ round(., 1))

rownames(caste) <- NULL

caste <- data.frame(t(caste))
caste <- caste[-1, ]

summary_uf <- rbind(age_grp, grade, transport, distsch, typesch, income, caste)
write.csv(summary_uf, "summary_uf.csv")


#### 4. Table: Cycling share all India ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

##survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

cycle <- svyby(~ cycle, by = ~ year, svyd, svymean, vartype = c("se","ci")) #https://www.rdocumentation.org/packages/survey/versions/4.1-1/topics/svyby
colnames(cycle)[colnames(cycle) == "cycle"] <- "mean"
cycle$mean<-format(round(cycle$mean*100, 1),1)
cycle$se<-round(cycle$se*100, 1)
cycle$ci_l<-round(cycle$ci_l*100, 1)
cycle$ci_u<-round(cycle$ci_u*100, 1)
rownames(cycle)<-NULL


#### 5. Table: Sample Size by state (stratified by sector and gender) for all three years ####

edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

sample_size <- edu_temp %>% group_by(state_name, year, sector, sex) %>% summarize(sample_size = n())

sample_size$type<-NA
sample_size$type[sample_size$sector=="rural" & sample_size$sex=="Female"] <- "RF"
sample_size$type[sample_size$sector=="rural" & sample_size$sex=="Male"] <- "RM"
sample_size$type[sample_size$sector=="urban" & sample_size$sex=="Female"] <- "UF"
sample_size$type[sample_size$sector=="urban" & sample_size$sex=="Male"] <- "UM"

sample_size$sector<-NULL
sample_size$sex<-NULL

## 2007 
ss_2007 <- sample_size[sample_size$year=="2007",]
ss_2007$year<-NULL
ss_2007<-ss_2007 %>% spread(type, sample_size)
ss_2007$persons <- ss_2007$RF+ss_2007$RM+ss_2007$UF+ss_2007$UM

## 2014
ss_2014 <- sample_size[sample_size$year=="2014",]
ss_2014$year<-NULL
ss_2014<-ss_2014 %>% spread(type, sample_size)
ss_2014$persons <- ss_2014$RF+ss_2014$RM+ss_2014$UF+ss_2014$UM

## 2017
ss_2017 <- sample_size[sample_size$year=="2017",]
ss_2017$year<-NULL
ss_2017<-ss_2017 %>% spread(type, sample_size)
ss_2017$persons <- ss_2017$RF+ss_2017$RM+ss_2017$UF+ss_2017$UM

## sample size combined file

ss <- ss_2007 %>% left_join(ss_2014, by="state_name")
ss <- ss %>% left_join(ss_2017, by="state_name")
write.csv(ss, "ss.csv")

#### 6. Tables: Cycle share by states over three years (total, rural, urban, RF, RM, UF, UM) ####

edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

### cycle share for 31 states (rural+urban)

## 2017
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_p_2017 <- svyby(~ cycle, by = ~ state_name, subset(svyd, year==2017), svymean, vartype = c("se","ci"))
colnames(cycle_p_2017)[colnames(cycle_p_2017) == "cycle"] <- "mean"
cycle_p_2017$mean<-cycle_p_2017$mean*100
cycle_p_2017$mean1 <- sprintf("%0.1f", cycle_p_2017$mean)
cycle_p_2017$mean1 <- sprintf(paste0("%0", max(nchar(cycle_p_2017$mean1)), "s"), cycle_p_2017$mean1)
# cycle_p_2017$mean<-str_pad(cycle_p_2017$mean,4,side="left", pad=" ")
# cycle_p_2017$mean<-decimal.align(cycle_p_2017$mean,dechar=".",nint=2,ndec=1,pad.left=TRUE)
cycle_p_2017$ci_l<-format(round(cycle_p_2017$ci_l*100, 1),1)
cycle_p_2017$ci_u<-format(round(cycle_p_2017$ci_u*100, 1),1)
rownames(cycle_p_2017)<-NULL
cycle_p_2017$se<-NULL
cycle_p_2017$mode_share_cycle<-paste0(cycle_p_2017$mean, " ", "(", cycle_p_2017$ci_l, ",", cycle_p_2017$ci_u, ")")
cycle_p_2017$mean<-NULL
cycle_p_2017$ci_l<-NULL
cycle_p_2017$ci_u<-NULL
# cycle_p_2017_out <- xtable(cycle_p_2017)
# print(cycle_p_2017_out, type='html', file="cycle_p_2017.html")
write_csv(cycle_p_2017,"cycle_p_2017.csv")

## 2014
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_p_2014 <- svyby(~ cycle, by = ~ state_name, subset(svyd, year==2014), svymean, vartype = c("se","ci"))
colnames(cycle_p_2014)[colnames(cycle_p_2014) == "cycle"] <- "mean"
cycle_p_2014$mean<-round(cycle_p_2014$mean*100, 1)
cycle_p_2014$ci_l<-round(cycle_p_2014$ci_l*100, 1)
cycle_p_2014$ci_u<-round(cycle_p_2014$ci_u*100, 1)
rownames(cycle_p_2014)<-NULL
cycle_p_2014$se<-NULL
cycle_p_2014$mode_share_cycle<-paste0(cycle_p_2014$mean, " ", "(", cycle_p_2014$ci_l, ",", cycle_p_2014$ci_u, ")")
cycle_p_2014$mean<-NULL
cycle_p_2014$ci_l<-NULL
cycle_p_2014$ci_u<-NULL
write.csv(cycle_p_2014,"cycle_p_2014.csv")
## 2007
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_p_2007 <- svyby(~ cycle, by = ~ state_name, subset(svyd, year==2007), svymean, vartype = c("se","ci"))
colnames(cycle_p_2007)[colnames(cycle_p_2007) == "cycle"] <- "mean"
cycle_p_2007$mean<-round(cycle_p_2007$mean*100, 1)
cycle_p_2007$ci_l<-round(cycle_p_2007$ci_l*100, 1)
cycle_p_2007$ci_u<-round(cycle_p_2007$ci_u*100, 1)
rownames(cycle_p_2007)<-NULL
cycle_p_2007$se<-NULL
cycle_p_2007$mode_share_cycle<-paste0(cycle_p_2007$mean, " ", "(", cycle_p_2007$ci_l, ",", cycle_p_2007$ci_u, ")")
cycle_p_2007$mean<-NULL
cycle_p_2007$ci_l<-NULL
cycle_p_2007$ci_u<-NULL
write.csv(cycle_p_2007,"cycle_p_2007.csv")

## cycle share combined file
cycle_p <- cycle_p_2007 %>% left_join(cycle_p_2014, by="state_name")
cycle_p <- cycle_p %>% left_join(cycle_p_2017, by="state_name")
write.csv(cycle_p,"cycle_p.csv")


### cycle share for 31 states (rural)

## 2017
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_r_2017 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2017 & sector=="rural")), 
                      svymean, vartype = c("se","ci"))
colnames(cycle_r_2017)[colnames(cycle_r_2017) == "cycle"] <- "mean"
cycle_r_2017$mean<-round(cycle_r_2017$mean*100, 1)
cycle_r_2017$ci_l<-round(cycle_r_2017$ci_l*100, 1)
cycle_r_2017$ci_u<-round(cycle_r_2017$ci_u*100, 1)
rownames(cycle_r_2017)<-NULL
cycle_r_2017$se<-NULL
cycle_r_2017$mode_share_cycle<-paste0(cycle_r_2017$mean, " ", "(", cycle_r_2017$ci_l, ",", cycle_r_2017$ci_u, ")")
cycle_r_2017$mean<-NULL
cycle_r_2017$ci_l<-NULL
cycle_r_2017$ci_u<-NULL

## 2014
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_r_2014 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2014 & sector=="rural")), 
                      svymean, vartype = c("se","ci"))
colnames(cycle_r_2014)[colnames(cycle_r_2014) == "cycle"] <- "mean"
cycle_r_2014$mean<-round(cycle_r_2014$mean*100, 1)
cycle_r_2014$ci_l<-round(cycle_r_2014$ci_l*100, 1)
cycle_r_2014$ci_u<-round(cycle_r_2014$ci_u*100, 1)
rownames(cycle_r_2014)<-NULL
cycle_r_2014$se<-NULL
cycle_r_2014$mode_share_cycle<-paste0(cycle_r_2014$mean, " ", "(", cycle_r_2014$ci_l, ",", cycle_r_2014$ci_u, ")")
cycle_r_2014$mean<-NULL
cycle_r_2014$ci_l<-NULL
cycle_r_2014$ci_u<-NULL

## 2007
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_r_2007 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2007 & sector=="rural")), 
                      svymean, vartype = c("se","ci"))
colnames(cycle_r_2007)[colnames(cycle_r_2007) == "cycle"] <- "mean"
cycle_r_2007$mean<-round(cycle_r_2007$mean*100, 1)
cycle_r_2007$ci_l<-round(cycle_r_2007$ci_l*100, 1)
cycle_r_2007$ci_u<-round(cycle_r_2007$ci_u*100, 1)
rownames(cycle_r_2007)<-NULL
cycle_r_2007$se<-NULL
cycle_r_2007$mode_share_cycle<-paste0(cycle_r_2007$mean, " ", "(", cycle_r_2007$ci_l, ",", cycle_r_2007$ci_u, ")")
cycle_r_2007$mean<-NULL
cycle_r_2007$ci_l<-NULL
cycle_r_2007$ci_u<-NULL

## cycle share combined file
cycle_r <- cycle_r_2007 %>% left_join(cycle_r_2014, by="state_name")
cycle_r <- cycle_r %>% left_join(cycle_r_2017, by="state_name")
write.csv(cycle_r,"cycle_r.csv")

### cycle share for 31 states (urban)

## 2017
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_u_2017 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2017 & sector=="urban")), 
                      svymean, vartype = c("se","ci"))
colnames(cycle_u_2017)[colnames(cycle_u_2017) == "cycle"] <- "mean"
cycle_u_2017$mean<-round(cycle_u_2017$mean*100, 1)
cycle_u_2017$ci_l<-round(cycle_u_2017$ci_l*100, 1)
cycle_u_2017$ci_u<-round(cycle_u_2017$ci_u*100, 1)
rownames(cycle_u_2017)<-NULL
cycle_u_2017$se<-NULL
cycle_u_2017$mode_share_cycle<-paste0(cycle_u_2017$mean, " ", "(", cycle_u_2017$ci_l, ",", cycle_u_2017$ci_u, ")")
cycle_u_2017$mean<-NULL
cycle_u_2017$ci_l<-NULL
cycle_u_2017$ci_u<-NULL

## 2014
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_u_2014 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2014 & sector=="urban")), 
                      svymean, vartype = c("se","ci"))
colnames(cycle_u_2014)[colnames(cycle_u_2014) == "cycle"] <- "mean"
cycle_u_2014$mean<-round(cycle_u_2014$mean*100, 1)
cycle_u_2014$ci_l<-round(cycle_u_2014$ci_l*100, 1)
cycle_u_2014$ci_u<-round(cycle_u_2014$ci_u*100, 1)
rownames(cycle_u_2014)<-NULL
cycle_u_2014$se<-NULL
cycle_u_2014$mode_share_cycle<-paste0(cycle_u_2014$mean, " ", "(", cycle_u_2014$ci_l, ",", cycle_u_2014$ci_u, ")")
cycle_u_2014$mean<-NULL
cycle_u_2014$ci_l<-NULL
cycle_u_2014$ci_u<-NULL

## 2007
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_u_2007 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2007 & sector=="urban")), 
                      svymean, vartype = c("se","ci"))
colnames(cycle_u_2007)[colnames(cycle_u_2007) == "cycle"] <- "mean"
cycle_u_2007$mean<-round(cycle_u_2007$mean*100, 1)
cycle_u_2007$ci_l<-round(cycle_u_2007$ci_l*100, 1)
cycle_u_2007$ci_u<-round(cycle_u_2007$ci_u*100, 1)
rownames(cycle_u_2007)<-NULL
cycle_u_2007$se<-NULL
cycle_u_2007$mode_share_cycle<-paste0(cycle_u_2007$mean, " ", "(", cycle_u_2007$ci_l, ",", cycle_u_2007$ci_u, ")")
cycle_u_2007$mean<-NULL
cycle_u_2007$ci_l<-NULL
cycle_u_2007$ci_u<-NULL

## cycle share combined file
cycle_u <- cycle_u_2007 %>% left_join(cycle_u_2014, by="state_name")
cycle_u <- cycle_u %>% left_join(cycle_u_2017, by="state_name")
write.csv(cycle_u,"cycle_u.csv")


### cycle share for 31 states (rural female)

## 2017
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_rf_2017 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2017 & sector=="rural" & sex=="Female")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_rf_2017)[colnames(cycle_rf_2017) == "cycle"] <- "mean"
cycle_rf_2017$mean<-round(cycle_rf_2017$mean*100, 1)
cycle_rf_2017$ci_l<-round(cycle_rf_2017$ci_l*100, 1)
cycle_rf_2017$ci_u<-round(cycle_rf_2017$ci_u*100, 1)
rownames(cycle_rf_2017)<-NULL
cycle_rf_2017$se<-NULL
cycle_rf_2017$mode_share_cycle<-paste0(cycle_rf_2017$mean, " ", "(", cycle_rf_2017$ci_l, ",", cycle_rf_2017$ci_u, ")")
cycle_rf_2017$mean<-NULL
cycle_rf_2017$ci_l<-NULL
cycle_rf_2017$ci_u<-NULL

## 2014
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_rf_2014 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2014 & sector=="rural" & sex=="Female")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_rf_2014)[colnames(cycle_rf_2014) == "cycle"] <- "mean"
cycle_rf_2014$mean<-round(cycle_rf_2014$mean*100, 1)
cycle_rf_2014$ci_l<-round(cycle_rf_2014$ci_l*100, 1)
cycle_rf_2014$ci_u<-round(cycle_rf_2014$ci_u*100, 1)
rownames(cycle_rf_2014)<-NULL
cycle_rf_2014$se<-NULL
cycle_rf_2014$mode_share_cycle<-paste0(cycle_rf_2014$mean, " ", "(", cycle_rf_2014$ci_l, ",", cycle_rf_2014$ci_u, ")")
cycle_rf_2014$mean<-NULL
cycle_rf_2014$ci_l<-NULL
cycle_rf_2014$ci_u<-NULL

## 2007
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_rf_2007 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2007 & sector=="rural" & sex=="Female")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_rf_2007)[colnames(cycle_rf_2007) == "cycle"] <- "mean"
cycle_rf_2007$mean<-round(cycle_rf_2007$mean*100, 1)
cycle_rf_2007$ci_l<-round(cycle_rf_2007$ci_l*100, 1)
cycle_rf_2007$ci_u<-round(cycle_rf_2007$ci_u*100, 1)
rownames(cycle_rf_2007)<-NULL
cycle_rf_2007$se<-NULL
cycle_rf_2007$mode_share_cycle<-paste0(cycle_rf_2007$mean, " ", "(", cycle_rf_2007$ci_l, ",", cycle_rf_2007$ci_u, ")")
cycle_rf_2007$mean<-NULL
cycle_rf_2007$ci_l<-NULL
cycle_rf_2007$ci_u<-NULL

## cycle share combined file
cycle_rf <- cycle_rf_2007 %>% left_join(cycle_rf_2014, by="state_name")
cycle_rf <- cycle_rf %>% left_join(cycle_rf_2017, by="state_name")
write.csv(cycle_rf,"cycle_rf.csv")


### cycle share for 31 states (rural male)

## 2017
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_rm_2017 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2017 & sector=="rural" & sex=="Male")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_rm_2017)[colnames(cycle_rm_2017) == "cycle"] <- "mean"
cycle_rm_2017$mean<-round(cycle_rm_2017$mean*100, 1)
cycle_rm_2017$ci_l<-round(cycle_rm_2017$ci_l*100, 1)
cycle_rm_2017$ci_u<-round(cycle_rm_2017$ci_u*100, 1)
rownames(cycle_rm_2017)<-NULL
cycle_rm_2017$se<-NULL
cycle_rm_2017$mode_share_cycle<-paste0(cycle_rm_2017$mean, " ", "(", cycle_rm_2017$ci_l, ",", cycle_rm_2017$ci_u, ")")
cycle_rm_2017$mean<-NULL
cycle_rm_2017$ci_l<-NULL
cycle_rm_2017$ci_u<-NULL

## 2014
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_rm_2014 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2014 & sector=="rural" & sex=="Male")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_rm_2014)[colnames(cycle_rm_2014) == "cycle"] <- "mean"
cycle_rm_2014$mean<-round(cycle_rm_2014$mean*100, 1)
cycle_rm_2014$ci_l<-round(cycle_rm_2014$ci_l*100, 1)
cycle_rm_2014$ci_u<-round(cycle_rm_2014$ci_u*100, 1)
rownames(cycle_rm_2014)<-NULL
cycle_rm_2014$se<-NULL
cycle_rm_2014$mode_share_cycle<-paste0(cycle_rm_2014$mean, " ", "(", cycle_rm_2014$ci_l, ",", cycle_rm_2014$ci_u, ")")
cycle_rm_2014$mean<-NULL
cycle_rm_2014$ci_l<-NULL
cycle_rm_2014$ci_u<-NULL

## 2007
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_rm_2007 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2007 & sector=="rural" & sex=="Male")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_rm_2007)[colnames(cycle_rm_2007) == "cycle"] <- "mean"
cycle_rm_2007$mean<-round(cycle_rm_2007$mean*100, 1)
cycle_rm_2007$ci_l<-round(cycle_rm_2007$ci_l*100, 1)
cycle_rm_2007$ci_u<-round(cycle_rm_2007$ci_u*100, 1)
rownames(cycle_rm_2007)<-NULL
cycle_rm_2007$se<-NULL
cycle_rm_2007$mode_share_cycle<-paste0(cycle_rm_2007$mean, " ", "(", cycle_rm_2007$ci_l, ",", cycle_rm_2007$ci_u, ")")
cycle_rm_2007$mean<-NULL
cycle_rm_2007$ci_l<-NULL
cycle_rm_2007$ci_u<-NULL

## cycle share combined file
cycle_rm <- cycle_rm_2007 %>% left_join(cycle_rm_2014, by="state_name")
cycle_rm <- cycle_rm %>% left_join(cycle_rm_2017, by="state_name")
write.csv(cycle_rm,"cycle_rm.csv")


### cycle share for 31 states (urban female)

## 2017
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_uf_2017 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2017 & sector=="urban" & sex=="Female")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_uf_2017)[colnames(cycle_uf_2017) == "cycle"] <- "mean"
cycle_uf_2017$mean<-round(cycle_uf_2017$mean*100, 1)
cycle_uf_2017$ci_l<-round(cycle_uf_2017$ci_l*100, 1)
cycle_uf_2017$ci_u<-round(cycle_uf_2017$ci_u*100, 1)
rownames(cycle_uf_2017)<-NULL
cycle_uf_2017$se<-NULL
cycle_uf_2017$mode_share_cycle<-paste0(cycle_uf_2017$mean, " ", "(", cycle_uf_2017$ci_l, ",", cycle_uf_2017$ci_u, ")")
cycle_uf_2017$mean<-NULL
cycle_uf_2017$ci_l<-NULL
cycle_uf_2017$ci_u<-NULL

## 2014
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_uf_2014 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2014 & sector=="urban" & sex=="Female")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_uf_2014)[colnames(cycle_uf_2014) == "cycle"] <- "mean"
cycle_uf_2014$mean<-round(cycle_uf_2014$mean*100, 1)
cycle_uf_2014$ci_l<-round(cycle_uf_2014$ci_l*100, 1)
cycle_uf_2014$ci_u<-round(cycle_uf_2014$ci_u*100, 1)
rownames(cycle_uf_2014)<-NULL
cycle_uf_2014$se<-NULL
cycle_uf_2014$mode_share_cycle<-paste0(cycle_uf_2014$mean, " ", "(", cycle_uf_2014$ci_l, ",", cycle_uf_2014$ci_u, ")")
cycle_uf_2014$mean<-NULL
cycle_uf_2014$ci_l<-NULL
cycle_uf_2014$ci_u<-NULL

## 2007
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_uf_2007 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2007 & sector=="urban" & sex=="Female")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_uf_2007)[colnames(cycle_uf_2007) == "cycle"] <- "mean"
cycle_uf_2007$mean<-round(cycle_uf_2007$mean*100, 1)
cycle_uf_2007$ci_l<-round(cycle_uf_2007$ci_l*100, 1)
cycle_uf_2007$ci_u<-round(cycle_uf_2007$ci_u*100, 1)
rownames(cycle_uf_2007)<-NULL
cycle_uf_2007$se<-NULL
cycle_uf_2007$mode_share_cycle<-paste0(cycle_uf_2007$mean, " ", "(", cycle_uf_2007$ci_l, ",", cycle_uf_2007$ci_u, ")")
cycle_uf_2007$mean<-NULL
cycle_uf_2007$ci_l<-NULL
cycle_uf_2007$ci_u<-NULL

## cycle share combined file
cycle_uf <- cycle_uf_2007 %>% left_join(cycle_uf_2014, by="state_name")
cycle_uf <- cycle_uf %>% left_join(cycle_uf_2017, by="state_name")
write.csv(cycle_uf,"cycle_uf.csv")


### cycle share for 31 states (urban male)

## 2017
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_um_2017 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2017 & sector=="urban" & sex=="Male")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_um_2017)[colnames(cycle_um_2017) == "cycle"] <- "mean"
cycle_um_2017$mean<-round(cycle_um_2017$mean*100, 1)
cycle_um_2017$ci_l<-round(cycle_um_2017$ci_l*100, 1)
cycle_um_2017$ci_u<-round(cycle_um_2017$ci_u*100, 1)
rownames(cycle_um_2017)<-NULL
cycle_um_2017$se<-NULL
cycle_um_2017$mode_share_cycle<-paste0(cycle_um_2017$mean, " ", "(", cycle_um_2017$ci_l, ",", cycle_um_2017$ci_u, ")")
cycle_um_2017$mean<-NULL
cycle_um_2017$ci_l<-NULL
cycle_um_2017$ci_u<-NULL

## 2014
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_um_2014 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2014 & sector=="urban" & sex=="Male")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_um_2014)[colnames(cycle_um_2014) == "cycle"] <- "mean"
cycle_um_2014$mean<-round(cycle_um_2014$mean*100, 1)
cycle_um_2014$ci_l<-round(cycle_um_2014$ci_l*100, 1)
cycle_um_2014$ci_u<-round(cycle_um_2014$ci_u*100, 1)
rownames(cycle_um_2014)<-NULL
cycle_um_2014$se<-NULL
cycle_um_2014$mode_share_cycle<-paste0(cycle_um_2014$mean, " ", "(", cycle_um_2014$ci_l, ",", cycle_um_2014$ci_u, ")")
cycle_um_2014$mean<-NULL
cycle_um_2014$ci_l<-NULL
cycle_um_2014$ci_u<-NULL

## 2007
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)
cycle_um_2007 <- svyby(~ cycle, by = ~ state_name, subset(svyd, (year==2007 & sector=="urban" & sex=="Male")), 
                       svymean, vartype = c("se","ci"))
colnames(cycle_um_2007)[colnames(cycle_um_2007) == "cycle"] <- "mean"
cycle_um_2007$mean<-round(cycle_um_2007$mean*100, 1)
cycle_um_2007$ci_l<-round(cycle_um_2007$ci_l*100, 1)
cycle_um_2007$ci_u<-round(cycle_um_2007$ci_u*100, 1)
rownames(cycle_um_2007)<-NULL
cycle_um_2007$se<-NULL
cycle_um_2007$mode_share_cycle<-paste0(cycle_um_2007$mean, " ", "(", cycle_um_2007$ci_l, ",", cycle_um_2007$ci_u, ")")
cycle_um_2007$mean<-NULL
cycle_um_2007$ci_l<-NULL
cycle_um_2007$ci_u<-NULL

## cycle share combined file
cycle_um <- cycle_um_2007 %>% left_join(cycle_um_2014, by="state_name")
cycle_um <- cycle_um %>% left_join(cycle_um_2017, by="state_name")
write.csv(cycle_um,"cycle_um.csv")


#### 7. Arrow plot: Change in cycling mode share by state stratified by rural/urban and gender groups ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

##combining states
edu_temp$state_name[which(edu_temp$state_name %in% c("Mizoram","Nagaland","Sikkim","Meghalaya"))]<-"NE"
edu_temp$state_name[which(edu_temp$state_name %in% c("Jammu & Kashmir","Himachal Pradesh"))]<-"Hilly region"
edu_temp$state_name[which(edu_temp$state_name %in% c("Daman & Diu","Dadra & Nagar Haveli"))]<-"DD"
edu_temp <- subset(edu_temp, !state_name %in% c("Lakshadweep", "Andaman & Nicobar Islands"))

##specify survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

##obtaining proportions
cycle <- svyby(~ cycle, by = ~ year+sector+state_name+sex, svyd, svymean, vartype = c("se","ci")) #https://www.rdocumentation.org/packages/survey/versions/4.1-1/topics/svyby
colnames(cycle)[colnames(cycle) == "cycle"] <- "mean"
cycle$mean<-as.numeric(format(round(cycle$mean*100, 1),2,format="f"))
cycle$ci_l<-round(cycle$ci_l*100, 1)
cycle$ci_u<-round(cycle$ci_u*100, 1)
rownames(cycle)<-NULL
cycle$se<-NULL

cycle_india <- svyby(~ cycle, by = ~ year+sector+sex, svyd, svymean, vartype = c("se","ci"))
colnames(cycle_india)[colnames(cycle_india) == "cycle"] <- "mean"
cycle_india$mean<-as.numeric(format(round(cycle_india$mean*100, 1),2,format="f"))
cycle_india$ci_l<-round(cycle_india$ci_l*100, 1)
cycle_india$ci_u<-round(cycle_india$ci_u*100, 1)
rownames(cycle_india)<-NULL
cycle_india$se<-NULL
cycle_india$state_name <- "INDIA"
cycle_india <- cycle_india[ ,c("year","sector","state_name","sex","mean","ci_l","ci_u")]

cycle <- rbind(cycle, cycle_india)

### rural females

cycle_rfg<-cycle %>% filter(!year==2014) %>% filter(sector=="rural") %>% filter(sex=="Female") %>% select(-c("sex","sector"))

##bds data
bds <- read.csv("Bicycle distribution India/BDS starting year.csv")

## indicate BDS

cycle_rfg$bds<-NA
cycle_rfg$bds[cycle_rfg$state_name %in% c("Assam","Bihar","Chhattisgarh","DD","Gujarat","Haryana","Jharkhand","Karnataka",
                                          "Kerala","Madhya Pradesh","Odisha","Puducherry","Punjab","Rajasthan", 
                                          "Tamil Nadu","Tripura","Uttar Pradesh","Uttarakhand","West Bengal")] <- "(B)"

cycle_rfg$bds[is.na(cycle_rfg$bds)] <- "     "

cycle_rfg$state_name <- paste0(cycle_rfg$state_name, cycle_rfg$bds)


##data required for graph
cycle_rfg$state_name<-factor(cycle_rfg$state_name)
cycle_rfg$year<-as.factor(cycle_rfg$year)

cycle_rfg <- cycle_rfg |> 
  select(state_name, year, mean) |> 
  pivot_wider(names_from = year, names_prefix = 'year_', values_from = mean) |> 
  mutate(
    change = year_2017 - year_2007, 
    sign_change = (change > 0),
    state_name = fct_reorder(state_name, year_2017*if_else(sign_change, -1, 1))
  )

cycle_rfg <- cycle_rfg %>% arrange(-year_2017)

## plot
a <- ggplot(cycle_rfg, aes(x = year_2007, xend = year_2017,
                           y = forcats::fct_inorder(state_name), yend = forcats::fct_inorder(state_name),
                           color = sign_change)) +
  geom_segment(arrow = arrow(angle = 30, length = unit(0.3, 'cm'), ends = 'last',type = 'closed'),size = 1.5) +
  scale_color_manual(values=c( "#FF3333","#0000FF"))+
  geom_text(aes(x = year_2007, y = state_name,label = year_2007),color="black",size=11,
            nudge_x= ifelse(cycle_rfg$sign_change == TRUE, -1, 1))+
  geom_text(aes(x = year_2017, y = state_name,label = year_2017),color="black",size=11,
            nudge_x= ifelse(cycle_rfg$sign_change == TRUE, 1.2, -1))+
  # geom_text_repel(aes(x = year_2007, y = state_name, label = mode_share_cycle.x),color="black")+
  # geom_text_repel(aes(x = year_2017, y = state_name, label = mode_share_cycle),color="black")+
  scale_x_continuous(limits = c(-1.8, 33),breaks = seq(0, 30, by = 5), expand = c(0, 0)) +
  scale_y_discrete(expand = expansion(add = c(0.5, 1)),limits=rev) +
  labs(x = "Mode share of cycling (%)", y = "")+
  ggtitle('a) Rural Girls') +
  theme_bw() +
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=42, colour = "black"),
        axis.text.x = element_text(size=36, colour = "black"),
        axis.text.y = element_text(size=36, colour = "black"),
        #axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0,vjust=0.5,size=50, colour = "black"),
        legend.position = "none")


##rural males

cycle_rmg<-cycle %>% filter(!year==2014) %>% filter(sector=="rural") %>% filter(sex=="Male") %>% select(-c("sex","sector"))

## indicate BDS

cycle_rmg$bds<-NA
cycle_rmg$bds[cycle_rmg$state_name %in% c("Haryana","Jharkhand","Karnataka",
                                          "Kerala","Madhya Pradesh","Odisha","Puducherry",
                                          "Tamil Nadu","Uttar Pradesh","West Bengal")] <- "(B)"

cycle_rmg$bds[is.na(cycle_rmg$bds)] <- "     "

cycle_rmg$state_name <- paste0(cycle_rmg$state_name, cycle_rmg$bds)

cycle_rmg$state_name<-factor(cycle_rmg$state_name)
cycle_rmg$year<-as.factor(cycle_rmg$year)


cycle_rmg <- cycle_rmg |> 
  select(state_name, year, mean) |> 
  pivot_wider(names_from = year, names_prefix = 'year_', values_from = mean) |> 
  mutate(
    change = year_2017 - year_2007, 
    sign_change = (change > 0),
    state_name = fct_reorder(state_name, year_2017*if_else(sign_change, -1, 1))
  )

cycle_rmg <- cycle_rmg %>% arrange(-year_2017)

b <- ggplot(cycle_rmg, aes(x = year_2007, xend = year_2017,
                           y = forcats::fct_inorder(state_name), yend = forcats::fct_inorder(state_name), 
                           color = sign_change)) +
  geom_segment(arrow = arrow(angle = 30, length = unit(0.3, 'cm'), ends = 'last',type = 'closed'),size = 1.5) +
  scale_color_manual(values=c( "#FF3333","#0000FF"))+
  geom_text(aes(x = year_2007, y = state_name,label = year_2007),color="black",size=11,
            nudge_x= ifelse(cycle_rmg$sign_change == TRUE, -1, 1))+
  geom_text(aes(x = year_2017, y = state_name,label = year_2017),color="black",size=11,
            nudge_x= ifelse(cycle_rmg$sign_change == TRUE, 1.2, -1))+
  #geom_text_repel(aes(x = year_2007, y = state_name, label = year_2007),color="black")+
  #geom_text_repel(aes(x = year_2017, y = state_name, label = year_2017),color="black")+
  scale_x_continuous(name="Mode share of cycling (%)",limits = c(-1.8, 33),breaks = seq(0, 30, by = 5), expand = c(0, 0)) +
  scale_y_discrete(name="",expand = expansion(add = c(0.5, 1)),limits=rev) +
  ggtitle('b) Rural Boys') +
  theme_bw() + 
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=42, colour = "black"),
        axis.text.x = element_text(size=36, colour = "black"),
        axis.text.y = element_text(size=36, colour = "black"),
        #axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0,vjust=0.5,size=50, colour = "black"),
        legend.position = "none")


##urban females

cycle_ufg<-cycle %>% filter(!year==2014) %>% filter(sector=="urban") %>% filter(sex=="Female") %>% select(-c("sex","sector"))


## indicate BDS

cycle_ufg$bds<-NA
cycle_ufg$bds[cycle_ufg$state_name %in% c("Assam","Bihar","Chhattisgarh","DD","Gujarat","Jharkhand",
                                          "Kerala","Odisha","Puducherry","Punjab","Rajasthan", 
                                          "Tripura","Uttar Pradesh","Uttarakhand","West Bengal")] <- "(B)"

cycle_ufg$bds[is.na(cycle_ufg$bds)] <- "     "

cycle_ufg$state_name <- paste0(cycle_ufg$state_name, cycle_ufg$bds)


cycle_ufg$state_name<-factor(cycle_ufg$state_name)
cycle_ufg$year<-as.factor(cycle_ufg$year)


cycle_ufg <- cycle_ufg |> 
  select(state_name, year, mean) |> 
  pivot_wider(names_from = year, names_prefix = 'year_', values_from = mean) |> 
  mutate(
    change = year_2017 - year_2007, 
    sign_change = (change > 0),
    state_name = fct_reorder(state_name, year_2017*if_else(sign_change, -1, 1))
  )

cycle_ufg <- cycle_ufg %>% arrange(-year_2017)

c <- ggplot(cycle_ufg, aes(x = year_2007, xend = year_2017,
                           y = forcats::fct_inorder(state_name), yend = forcats::fct_inorder(state_name), 
                           color = sign_change)) +
  geom_segment(arrow = arrow(angle = 30, length = unit(0.3, 'cm'), ends = 'last',type = 'closed'),size = 1.5) +
  scale_color_manual(values=c( "#FF3333","#0000FF"))+
  geom_text(aes(x = year_2007, y = state_name,label = year_2007),color="black",size=11,
            nudge_x= ifelse(cycle_ufg$sign_change == TRUE, -1.1, 1.5))+
  geom_text(aes(x = year_2017, y = state_name,label = year_2017),color="black",size=11,
            nudge_x= ifelse(cycle_ufg$sign_change == TRUE, 1.1, -1.3))+
  #geom_text_repel(aes(x = year_2007, y = state_name, label = year_2007),color="black")+
  #geom_text_repel(aes(x = year_2017, y = state_name, label = year_2017),color="black")+
  scale_x_continuous(name="Mode share of cycling (%)",limits = c(-1.8, 33),breaks = seq(0, 30, by = 5), expand = c(0, 0)) +
  scale_y_discrete(name="",expand = expansion(add = c(0.5, 1)),limits=rev) +
  ggtitle('a) Urban Girls') +
  theme_bw() + 
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=42, colour = "black"),
        axis.text.x = element_text(size=36, colour = "black"),
        axis.text.y = element_text(size=36, colour = "black"),
        #axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0,vjust=0.5,size=50, colour = "black"),
        legend.position = "none")

#ggsave("longitudinal variation in cycling for urban females.png", width=16, height=12)


##urban males

cycle_umg<-cycle %>% filter(!year==2014) %>% filter(sector=="urban") %>% filter(sex=="Male") %>% select(-c("sex","sector"))

## indicate BDS

cycle_umg$bds<-NA
cycle_umg$bds[cycle_umg$state_name %in% c("Jharkhand",
                                          "Kerala","Odisha","Puducherry","Uttar Pradesh","West Bengal")] <- "(B)"

cycle_umg$bds[is.na(cycle_umg$bds)] <- "     "

cycle_umg$state_name <- paste0(cycle_umg$state_name, cycle_umg$bds)

cycle_umg$state_name<-factor(cycle_umg$state_name)
cycle_umg$year<-as.factor(cycle_umg$year)

cycle_umg <- cycle_umg |> 
  select(state_name, year, mean) |> 
  pivot_wider(names_from = year, names_prefix = 'year_', values_from = mean) |> 
  mutate(
    change = year_2017 - year_2007, 
    sign_change = (change > 0),
    state_name = fct_reorder(state_name, year_2017*if_else(sign_change, -1, 1))
  )

cycle_umg <- cycle_umg %>% arrange(-year_2017)

d <- ggplot(cycle_umg, aes(x = year_2007, xend = year_2017,
                           y = forcats::fct_inorder(state_name), yend = forcats::fct_inorder(state_name), 
                           color = sign_change)) +
  geom_segment(arrow = arrow(angle = 30, length = unit(0.3, 'cm'), ends = 'last',type = 'closed'),size = 1.5) +
  scale_color_manual(values=c( "#FF3333","#0000FF"))+
  geom_text(aes(x = year_2007, y = state_name,label = year_2007),color="black",size=12,
            nudge_x= ifelse(cycle_umg$sign_change == TRUE, -1.2, 1))+
  geom_text(aes(x = year_2017, y = state_name,label = year_2017),color="black",size=12,
            nudge_x= ifelse(cycle_umg$sign_change == TRUE, 1.2, -1))+
  #geom_text_repel(aes(x = year_2007, y = state_name, label = year_2007),color="black")+
  #geom_text_repel(aes(x = year_2017, y = state_name, label = year_2017),color="black")+
  scale_x_continuous(name="Mode share of cycling (%)",limits = c(-1.8, 33),breaks = seq(0, 30, by = 5), expand = c(0, 0)) +
  scale_y_discrete(name="",expand = expansion(add = c(0.5, 1)),limits=rev) +
  ggtitle('b) Urban Boys') +
  theme_bw() + 
  theme(panel.border = element_blank(), axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size=42, colour = "black"),
        axis.text.x = element_text(size=36, colour = "black"),
        axis.text.y = element_text(size=36, colour = "black"),
        #axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0,vjust=0.5,size=50, colour = "black"),
        legend.position = "none")

#ggsave("longitudinal variation in cycling for urban males.png", width=16, height=12)

## legend

fake_dat <- tibble(
  cycle_share = c(1.1, 1),
  year_2007 = c(2, 1),
  year_2017 = c(1, 2)
) 

fake_dat_longer <- fake_dat |> 
  pivot_longer(
    cols = -cycle_share,
    names_to = 'label',
    values_to = 'mean',
    names_prefix = 'year_'
  ) 

custom_legend <- ggplot() +
  geom_rect(
    aes(xmin = 0.8, xmax = 2.2,
        ymin = 0.9, ymax = 1.2),
    fill = 'white',
    col = 'grey40'
  ) +
  geom_segment(
    data = fake_dat,
    mapping = aes(
      x = year_2007, xend = year_2017, 
      y = cycle_share, yend = cycle_share,
    ),
    arrow = arrow(angle = 30, length = unit(0.3, 'cm')),
    color = c( "#FF3333","#0000FF"),
    size = 1
  ) +
  geom_text(
    data = fake_dat_longer,
    mapping = aes(x = mean, y = cycle_share, label = label),
    hjust = c(-0.1, 1.1, 1.1, -0.1),
    size=6,
    color = rep(c( "#FF3333","#0000FF"), each = 2)
  )  +
  theme_void() +
  coord_cartesian(
    ylim = c(0.8, 1.3),
    xlim = c(0.75, 2.25), 
    expand = F
  ) 
custom_legend
ggsave("legend.png",width = 6, height=4)

ggarrange(a,b, nrow=1, ncol=2)
ggsave("rural_arrow_plot.png",height=20, width=37)

#ggsave("rural_arrow_plot.tiff",height=20, width=37,device='tiff', dpi=400, compression = "lzw")

ggarrange(c,d, nrow=1, ncol=2)
ggsave("urban_arrow_plot.png",height=20, width=37)

#ggsave("urban_arrow_plot.tiff",height=20, width=37,device='tiff', dpi=400, compression = "lzw")


#### 8. Tables: proportion of cycle trips by females vs mode share of cycling across years ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

##for mode share of cycling
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1

##for proportion of girls among cyclists
edu_temp$cyc_f<-0
edu_temp$cyc_f[which((edu_temp$cycle==1)&(edu_temp$sex=="Female"))]<-1

##survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

###rural
##mode share of cycling
cycle <- svyby(~ cycle, by = ~ year+state_name, subset(svyd, sector=="rural"), svymean, vartype = c("se","ci")) #https://www.rdocumentation.org/packages/survey/versions/4.1-1/topics/svyby
colnames(cycle)[colnames(cycle) == "cycle"] <- "mean"
cycle$mean<-round(cycle$mean*100, 1)
cycle$ci_l<-round(cycle$ci_l*100, 1)
cycle$ci_u<-round(cycle$ci_u*100, 1)
cycle$mode_share <- paste0(cycle$mean, " ", "(", cycle$ci_l, ",", cycle$ci_u, ")")
rownames(cycle)<-NULL
cycle$se<-NULL
cycle$mean<-NULL
cycle$ci_l<-NULL
cycle$ci_u<-NULL
cycle <- cycle %>% spread(year, mode_share)

##proportion of girls among cyclists (girls in cycling out of all who cycle, hence we subset cycle==1)
cyc_f<-svyby(~cyc_f, by = ~year+state_name, subset(svyd, (cycle==1 & sector=="rural")), svymean, vartype = c("se","ci"))
colnames(cyc_f)[colnames(cyc_f) == "cyc_f"] <- "mean"
cyc_f$mean<-round(cyc_f$mean*100, 1)
cyc_f$ci_l<-round(cyc_f$ci_l*100, 1)
cyc_f$ci_u<-round(cyc_f$ci_u*100, 1)
cyc_f$cyc_f <- paste0(cyc_f$mean, " ", "(", cyc_f$ci_l, ",", cyc_f$ci_u, ")")
rownames(cyc_f)<-NULL
cyc_f$se<-NULL
cyc_f$mean<-NULL
cyc_f$ci_l<-NULL
cyc_f$ci_u<-NULL
cyc_f <- cyc_f %>% spread(year, cyc_f)

df<-cycle %>% left_join(cyc_f, by=c("state_name"))
df[is.na(df)] <- "0 (0,0)"
write.csv(df, "rural_female_repn_cyc.csv")


###urban
##mode share of cycling
cycle <- svyby(~ cycle, by = ~ year+state_name, subset(svyd, sector=="urban"), svymean, vartype = c("se","ci")) #https://www.rdocumentation.org/packages/survey/versions/4.1-1/topics/svyby
colnames(cycle)[colnames(cycle) == "cycle"] <- "mean"
cycle$mean<-round(cycle$mean*100, 1)
cycle$ci_l<-round(cycle$ci_l*100, 1)
cycle$ci_u<-round(cycle$ci_u*100, 1)
cycle$mode_share <- paste0(cycle$mean, " ", "(", cycle$ci_l, ",", cycle$ci_u, ")")
rownames(cycle)<-NULL
cycle$se<-NULL
cycle$mean<-NULL
cycle$ci_l<-NULL
cycle$ci_u<-NULL
cycle <- cycle %>% spread(year, mode_share)

##proportion of cycle trips made by females (cycle trips by females out of all the cycling trips that is why we subset cycle==1)
cyc_f<-svyby(~cyc_f, by = ~year+state_name, subset(svyd, (cycle==1 & sector=="urban")), svymean, vartype = c("se","ci"))
colnames(cyc_f)[colnames(cyc_f) == "cyc_f"] <- "mean"
cyc_f$mean<-round(cyc_f$mean*100, 1)
cyc_f$ci_l<-round(cyc_f$ci_l*100, 1)
cyc_f$ci_u<-round(cyc_f$ci_u*100, 1)
cyc_f$cyc_f <- paste0(cyc_f$mean, " ", "(", cyc_f$ci_l, ",", cyc_f$ci_u, ")")
rownames(cyc_f)<-NULL
cyc_f$se<-NULL
cyc_f$mean<-NULL
cyc_f$ci_l<-NULL
cyc_f$ci_u<-NULL
cyc_f <- cyc_f %>% spread(year, cyc_f)

df<-cycle %>% left_join(cyc_f, by=c("state_name"))
df[is.na(df)] <- "0 (0,0)"
write.csv(df, "urban_female_repn_cyc.csv")


##india
##proportion of cycle trips made by females (cycle trips by females out of all the cycling trips that is why we subset cycle==1)
cyc_f_r<-svyby(~cyc_f, by = ~year, subset(svyd, (cycle==1 & sector=="rural")), svymean, vartype = c("se","ci"))
cyc_f_u<-svyby(~cyc_f, by = ~year, subset(svyd, (cycle==1 & sector=="urban")), svymean, vartype = c("se","ci"))


#### 9. Scatter plots: proportion of cycle trips by females vs mode share of cycling stratified by rural/urban ####
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]

edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1

edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

edu_temp$cyc_f<-0
edu_temp$cyc_f[which((edu_temp$cycle==1)&(edu_temp$sex=="Female"))]<-1

edu_temp$state_name[which(edu_temp$state_name %in% c("Mizoram","Nagaland","Sikkim","Meghalaya"))]<-"NE"
edu_temp$state_name[which(edu_temp$state_name %in% c("Jammu & Kashmir","Himachal Pradesh"))]<-"Hilly region"
edu_temp$state_name[which(edu_temp$state_name %in% c("Daman & Diu","Dadra & Nagar Haveli"))]<-"DD"
edu_temp <- subset(edu_temp, !state_name %in% c("Lakshadweep", "Andaman & Nicobar Islands"))

svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)


###rural
##mode share of cycling
cycle <- svyby(~ cycle, by = ~ year+state_name, subset(svyd, sector=="rural"), svymean) #https://www.rdocumentation.org/packages/survey/versions/4.1-1/topics/svyby
colnames(cycle)[colnames(cycle) == "cycle"] <- "mode_share"
cycle$mode_share<-round(cycle$mode_share*100, 1)
rownames(cycle)<-NULL
cycle$se<-NULL

cycle_india <- svyby(~ cycle, by = ~ year, subset(svyd, sector=="rural"), svymean)
colnames(cycle_india)[colnames(cycle_india) == "cycle"] <- "mode_share"
cycle_india$mode_share<-round(cycle_india$mode_share*100, 1)
rownames(cycle_india)<-NULL
cycle_india$se<-NULL
cycle_india$state_name <- "INDIA"
cycle_india <- cycle_india[ ,c("year","state_name","mode_share")]

cycle <- rbind(cycle, cycle_india)

##proportion of cycle trips made by females (cycle trips by females out of all the cycling trips that is why we subset cycle==1)
cyc_f<-svyby(~cyc_f, by = ~year+state_name, subset(svyd, (cycle==1 & sector=="rural")), svymean)
colnames(cyc_f)[colnames(cyc_f) == "cyc_f"] <- "prop_cyc_f"
cyc_f$prop_cyc_f<-round(cyc_f$prop_cyc_f*100, 1)
rownames(cyc_f)<-NULL
cyc_f$se<-NULL

cyc_f_india<-svyby(~cyc_f, by = ~year, subset(svyd, (cycle==1 & sector=="rural")), svymean)
colnames(cyc_f_india)[colnames(cyc_f_india) == "cyc_f"] <- "prop_cyc_f"
cyc_f_india$prop_cyc_f<-round(cyc_f_india$prop_cyc_f*100, 1)
rownames(cyc_f_india)<-NULL
cyc_f_india$se<-NULL
cyc_f_india$state_name <- "INDIA"
cyc_f_india <- cyc_f_india[ ,c("year","state_name","prop_cyc_f")]

cyc_f <- rbind(cyc_f, cyc_f_india)

df_r<-cycle %>% left_join(cyc_f, by=c("year","state_name"))
df_r[is.na(df_r)] <- 0
df_r <- df_r %>% filter(!year==2014)


a<-ggplot(df_r, aes(x=mode_share, y=prop_cyc_f)) + 
  geom_point(size=1.5)+
  geom_smooth(method = "gam", se = TRUE, linetype = "dashed", colour="black") +
  #geom_smooth()+
  #geom_text(data = df,aes(x=mode_share, y = prop_cyc_f, label = state_name),size=3,hjust=-0.1)+
  geom_text_repel(data = df_r,aes(label = state_name),size=4.5, max.overlaps = 30)+
  scale_y_continuous(name="Proportion of girls among cyclists (%)",expand = c(0,0), limits=c(0, 64), 
                     breaks = c(0,10,20,30,40,50,60), labels = c('0','10','20','30','40','50','60'))+
  scale_x_continuous(name="Mode share of cycling (%)",expand = c(0,0), limits=c(0, 32.5), 
                     breaks = c(0,5,10,15,20,25,30), labels = c('0','5','10','15','20','25','30'))+
  ggtitle("a) Rural")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size=28, colour = "black"),
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.y = element_text(size=24, colour = "black"),
        axis.title.x = element_text(size=24, colour = "black"),
        strip.text = element_text(size=24, colour = "black"))+
  facet_wrap(~year)

#ggsave("rural_proportion of cycle trips by females vs mode share of cycling.png", width=16, height=9)

###urban
##mode share of cycling
cycle <- svyby(~ cycle, by = ~ year+state_name, subset(svyd, sector=="urban"), svymean) #https://www.rdocumentation.org/packages/survey/versions/4.1-1/topics/svyby
colnames(cycle)[colnames(cycle) == "cycle"] <- "mode_share"
cycle$mode_share<-round(cycle$mode_share*100, 1)
rownames(cycle)<-NULL
cycle$se<-NULL

cycle_india <- svyby(~ cycle, by = ~ year, subset(svyd, sector=="urban"), svymean)
colnames(cycle_india)[colnames(cycle_india) == "cycle"] <- "mode_share"
cycle_india$mode_share<-round(cycle_india$mode_share*100, 1)
rownames(cycle_india)<-NULL
cycle_india$se<-NULL
cycle_india$state_name <- "INDIA"
cycle_india <- cycle_india[ ,c("year","state_name","mode_share")]

cycle <- rbind(cycle, cycle_india)

##proportion of cycle trips made by females
cyc_f<-svyby(~cyc_f, by = ~year+state_name, subset(svyd, (cycle==1 & sector=="urban")), svymean)
colnames(cyc_f)[colnames(cyc_f) == "cyc_f"] <- "prop_cyc_f"
cyc_f$prop_cyc_f<-round(cyc_f$prop_cyc_f*100, 1)
rownames(cyc_f)<-NULL
cyc_f$se<-NULL

cyc_f_india<-svyby(~cyc_f, by = ~year, subset(svyd, (cycle==1 & sector=="urban")), svymean)
colnames(cyc_f_india)[colnames(cyc_f_india) == "cyc_f"] <- "prop_cyc_f"
cyc_f_india$prop_cyc_f<-round(cyc_f_india$prop_cyc_f*100, 1)
rownames(cyc_f_india)<-NULL
cyc_f_india$se<-NULL
cyc_f_india$state_name <- "INDIA"
cyc_f_india <- cyc_f_india[ ,c("year","state_name","prop_cyc_f")]

cyc_f <- rbind(cyc_f, cyc_f_india)

df_u<-cycle %>% left_join(cyc_f, by=c("year","state_name"))
df_u[is.na(df_u)] <- 0
df_u <- df_u %>% filter(!year==2014)


b<-ggplot(df_u, aes(x=mode_share, y=prop_cyc_f)) + 
  geom_point(size=1.5)+
  geom_smooth(method = "gam", se = TRUE, linetype = "dashed", colour="black") +
  #geom_text(data = df,aes(x=mode_share, y = prop_cyc_f, label = state_name),size=3,hjust=-0.1)+
  geom_text_repel(data = df_u,aes(label = state_name),size=4.5, max.overlaps = 30)+
  scale_y_continuous(name="Proportion of girls among cyclists (%)",expand = c(0,0), limits=c(0, 64), 
                     breaks = c(0,10,20,30,40,50,60), labels = c('0','10','20','30','40','50','60'))+
  scale_x_continuous(name="Mode share of cycling (%)",expand = c(0,0), limits=c(0, 32.5), 
                     breaks = c(0,5,10,15,20,25,30), labels = c('0','5','10','15','20','25','30'))+
  ggtitle("b) Urban")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(size=28, colour = "black"),
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.y = element_text(size=24, colour = "black"),
        axis.title.x = element_text(size=24, colour = "black"),
        strip.text = element_text(size=24, colour = "black"))+
  facet_wrap(~year)

#ggsave("urban_proportion of cycle trips by females vs mode share of cycling.png", width=16, height=9)

ggarrange(a,b,ncol=1,nrow=2)
ggsave("female_repn.png",width=12,height=16)

#ggsave("female_repn.tiff",height=16, width=12,device='tiff', dpi=400, compression = "lzw")



#### 10. Tables: cycling levels by age group stratified by gender and rural/urban across the years ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]

edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1

edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

##define age groups 
edu_temp$age_grp <- NA
edu_temp$age_grp<-cut(edu_temp$age,breaks = c(4, 10, 13, 15, 17),labels = c("5-10","11-13","14-15","16-17"),na.rm=T)
edu_temp$age_grp <- factor(edu_temp$age_grp, 
                           levels = c("5-10","11-13","14-15","16-17"), 
                           ordered = TRUE)

##survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

##Rural
cycle_age_r <- svyby(~ cycle, by = ~ year+age_grp+sex, subset(svyd, sector=="rural"), svymean, vartype = c("se","ci")) #https://www.rdocumentation.org/packages/survey/versions/4.1-1/topics/svyby
colnames(cycle_age_r)[colnames(cycle_age_r) == "cycle"] <- "mean"
cycle_age_r$mean<-round(cycle_age_r$mean*100, 1)
cycle_age_r$ci_l<-round(cycle_age_r$ci_l*100, 1)
cycle_age_r$ci_u<-round(cycle_age_r$ci_u*100, 1)
rownames(cycle_age_r)<-NULL
cycle_age_r$se<-NULL
cycle_age_r$mean <- paste0(cycle_age_r$mean, " ","(", cycle_age_r$ci_l,",",cycle_age_r$ci_u,")")
cycle_age_r$ci_l<-NULL
cycle_age_r$ci_u<-NULL

cycle_age_rm <- cycle_age_r %>% filter(sex=="Male") %>% select(-c("sex")) %>% spread(year, mean)
cycle_age_rf <- cycle_age_r %>% filter(sex=="Female") %>% select(-c("sex")) %>% spread(year, mean)

cycle_age_r <- cycle_age_rf %>% left_join(cycle_age_rm, by="age_grp")

write.csv(cycle_age_r, "cycle_age_r.csv")

##Urban
cycle_age_u <- svyby(~ cycle, by = ~ year+age_grp+sex, subset(svyd, sector=="urban"), svymean, vartype = c("se","ci")) #https://www.rdocumentation.org/packages/survey/versions/4.1-1/topics/svyby
colnames(cycle_age_u)[colnames(cycle_age_u) == "cycle"] <- "mean"
cycle_age_u$mean<-round(cycle_age_u$mean*100, 1)
cycle_age_u$ci_l<-round(cycle_age_u$ci_l*100, 1)
cycle_age_u$ci_u<-round(cycle_age_u$ci_u*100, 1)
rownames(cycle_age_u)<-NULL
cycle_age_u$se<-NULL
cycle_age_u$mean <- paste0(cycle_age_u$mean, " ","(", cycle_age_u$ci_l,",",cycle_age_u$ci_u,")")
cycle_age_u$ci_l<-NULL
cycle_age_u$ci_u<-NULL

cycle_age_um <- cycle_age_u %>% filter(sex=="Male") %>% select(-c("sex")) %>% spread(year, mean)
cycle_age_uf <- cycle_age_u %>% filter(sex=="Female") %>% select(-c("sex")) %>% spread(year, mean)

cycle_age_u <- cycle_age_uf %>% left_join(cycle_age_um, by="age_grp")

write.csv(cycle_age_u, "cycle_age_u.csv")


#### 11. GAM plots: cycling levels by age across years stratified by gender and rural/urban ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

edu_temp$cycle <- as.factor(edu_temp$cycle)
edu_temp$sex <- as.factor(edu_temp$sex)

##Rural

##using GAM for year 2007; specify household weights
mod_r_2007 <- gam(cycle ~ s(age, by = sex, k=4),family = quasibinomial, weights = hh_wgt, 
                  data = subset(edu_temp, (sector=="rural" & year==2007)), method = "REML")

pred_r_2007 <- predict.gam(object = mod_r_2007, newdata = subset(edu_temp, (sector=="rural" & year==2007)), 
                           type = "response", se.fit = TRUE)
pred_r_2007$fit_min<- pred_r_2007$fit - pred_r_2007$se.fit*1.96
pred_r_2007$fit_max<- pred_r_2007$fit + pred_r_2007$se.fit*1.96

df <- as.data.frame(matrix(nrow=55484, ncol=6))
colnames(df)[1]<-"age"
colnames(df)[2]<-"sex"
colnames(df)[3]<-"fit"
colnames(df)[4]<-"se.fit"
colnames(df)[5]<-"fit_min"
colnames(df)[6]<-"fit_max"
df$age <- mod_r_2007[["model"]][["age"]]
df$sex <- mod_r_2007[["model"]][["sex"]]
df$fit <- (pred_r_2007[["fit"]])*100
df$se.fit <- (pred_r_2007[["se.fit"]])*100
df$fit_min <- (pred_r_2007[["fit_min"]])*100
df$fit_max <- (pred_r_2007[["fit_max"]])*100

a <- ggplot(df, aes(y=fit, x=age, color=sex, fill=sex)) + geom_smooth(method = "gam", se = TRUE, linewidth=1.2)+
  geom_ribbon(aes(ymin=fit_min, ymax=fit_max), alpha = 0.5, linetype=0)+
  scale_colour_manual(values=c("#00CC33","#0000FF"))+
  scale_fill_manual(values=c("#66FF99","#6666FF"))+theme_classic()+
  scale_x_continuous(name="Age (years)", expand = c(0,0), limits=c(5, 17.5), 
                     breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17), 
                     labels = c('5','6','7','8','9','10','11','12','13','14','15','16','17'))+
  scale_y_continuous(name="Mode share of cycling (%)", expand = c(0,0), limits=c(0, 38), 
                     breaks = c(0,5,10,15,20,25,30,35), labels = c('0','5','10','15','20','25','30','35'))+
  annotate("text", x = 14.0, y = 20, label = "Boys", size=9)+
  annotate("text", x = 14.4, y = 10, label = "Girls", size=9)+
  ggtitle("a) Rural: 2007")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        plot.title = element_text(size=32, colour = "black"),
        legend.position = "none",
        panel.border = element_blank(),
        panel.background = element_blank())


##using GAM for year 2017
mod_r_2017 <- gam(cycle ~ s(age, by = sex, k=4),family = quasibinomial, weights = hh_wgt, 
                  data = subset(edu_temp, (sector=="rural" & year==2017)), method = "REML")

pred_r_2017 <- predict.gam(object = mod_r_2017, newdata = subset(edu_temp, (sector=="rural" & year==2017)), 
                           type = "response", se.fit = TRUE)
pred_r_2017$fit_min<- pred_r_2017$fit- pred_r_2017$se.fit*1.96
pred_r_2017$fit_max<- pred_r_2017$fit+ pred_r_2017$se.fit*1.96

df <- as.data.frame(matrix(nrow=63639, ncol=6))
colnames(df)[1]<-"age"
colnames(df)[2]<-"sex"
colnames(df)[3]<-"fit"
colnames(df)[4]<-"se.fit"
colnames(df)[5]<-"fit_min"
colnames(df)[6]<-"fit_max"
df$age <- mod_r_2017[["model"]][["age"]]
df$sex <- mod_r_2017[["model"]][["sex"]]
df$fit <- (pred_r_2017[["fit"]])*100
df$se.fit <- (pred_r_2017[["se.fit"]])*100
df$fit_min <- (pred_r_2017[["fit_min"]])*100
df$fit_max <- (pred_r_2017[["fit_max"]])*100

b <- ggplot(df, aes(y=fit, x=age, color=sex, fill=sex)) + geom_smooth(method = "gam", se = TRUE, linewidth=1.2)+
  geom_ribbon(aes(ymin=fit_min, ymax=fit_max), alpha = 0.5, linetype=0)+
  scale_colour_manual(values=c("#00CC33","#0000FF"))+
  scale_fill_manual(values=c("#66FF99","#6666FF"))+theme_classic()+
  scale_x_continuous(name="Age (years)", expand = c(0,0), limits=c(5, 17.5), 
                     breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17), 
                     labels = c('5','6','7','8','9','10','11','12','13','14','15','16','17'))+
  scale_y_continuous(name="Mode share of cycling (%)", expand = c(0,0), limits=c(0, 38), 
                     breaks = c(0,5,10,15,20,25,30,35), labels = c('0','5','10','15','20','25','30','35'))+
  annotate("text", x = 13.7, y = 27, label = "Boys", size=9)+
  annotate("text", x = 14.6, y = 20, label = "Girls", size=9)+
  ggtitle("b) Rural: 2017")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        plot.title = element_text(size=32, colour = "black"),
        legend.position = "none",
        panel.border = element_blank(),
        panel.background = element_blank())


##Urban

mod_u_2007 <- gam(cycle ~ s(age, by = sex, k=4),family = quasibinomial, weights = hh_wgt, 
                  data = subset(edu_temp, (sector=="urban" & year==2007)), method = "REML")

pred_u_2007 <- predict.gam(object = mod_u_2007, newdata = subset(edu_temp, (sector=="urban" & year==2007)), 
                           type = "response", se.fit = TRUE)
pred_u_2007$fit_min<- pred_u_2007$fit- pred_u_2007$se.fit*1.96
pred_u_2007$fit_max<- pred_u_2007$fit+ pred_u_2007$se.fit*1.96

df <- as.data.frame(matrix(nrow=28443, ncol=6))
colnames(df)[1]<-"age"
colnames(df)[2]<-"sex"
colnames(df)[3]<-"fit"
colnames(df)[4]<-"se.fit"
colnames(df)[5]<-"fit_min"
colnames(df)[6]<-"fit_max"
df$age <- mod_u_2007[["model"]][["age"]]
df$sex <- mod_u_2007[["model"]][["sex"]]
df$fit <- (pred_u_2007[["fit"]])*100
df$se.fit <- (pred_u_2007[["se.fit"]])*100
df$fit_min <- (pred_u_2007[["fit_min"]])*100
df$fit_max <- (pred_u_2007[["fit_max"]])*100

c <- ggplot(df, aes(y=fit, x=age, color=sex, fill=sex)) + geom_smooth(method = "gam", se = TRUE, linewidth=1.2)+
  geom_ribbon(aes(ymin=fit_min, ymax=fit_max), alpha = 0.5, linetype=0)+
  scale_colour_manual(values=c("#00CC33","#0000FF"))+
  scale_fill_manual(values=c("#66FF99","#6666FF"))+theme_classic()+
  scale_x_continuous(name="Age (years)", expand = c(0,0), limits=c(5, 17.5), 
                     breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17), 
                     labels = c('5','6','7','8','9','10','11','12','13','14','15','16','17'))+
  scale_y_continuous(name="Mode share of cycling (%)", expand = c(0,0), limits=c(0, 30.5), 
                     breaks = c(0,5,10,15,20,25,30), labels = c('0','5','10','15','20','25','30'))+
  annotate("text", x = 14.1, y = 20, label = "Boys", size=9)+
  annotate("text", x = 14.3, y = 10, label = "Girls", size=9)+
  ggtitle("c) Urban: 2007")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        plot.title = element_text(size=32, colour = "black"),
        legend.position = "none",
        panel.border = element_blank(),
        panel.background = element_blank())



mod_u_2017 <- gam(cycle ~ s(age, by = sex, k=4),family = quasibinomial, weights = hh_wgt, 
                  data = subset(edu_temp, (sector=="urban" & year==2017)), method = "REML")

pred_u_2017 <- predict.gam(object = mod_u_2017, newdata = subset(edu_temp, (sector=="urban" & year==2017)), 
                           type = "response", se.fit = TRUE)
pred_u_2017$fit_min<- pred_u_2017$fit- pred_u_2017$se.fit*1.96
pred_u_2017$fit_max<- pred_u_2017$fit+ pred_u_2017$se.fit*1.96

df <- as.data.frame(matrix(nrow=36439, ncol=6))
colnames(df)[1]<-"age"
colnames(df)[2]<-"sex"
colnames(df)[3]<-"fit"
colnames(df)[4]<-"se.fit"
colnames(df)[5]<-"fit_min"
colnames(df)[6]<-"fit_max"
df$age <- mod_u_2017[["model"]][["age"]]
df$sex <- mod_u_2017[["model"]][["sex"]]
df$fit <- (pred_u_2017[["fit"]])*100
df$se.fit <- (pred_u_2017[["se.fit"]])*100
df$fit_min <- (pred_u_2017[["fit_min"]])*100
df$fit_max <- (pred_u_2017[["fit_max"]])*100

d <- ggplot(df, aes(y=fit, x=age, color=sex, fill=sex)) + geom_smooth(method = "gam", se = TRUE, linewidth=1.2)+
  geom_ribbon(aes(ymin=fit_min, ymax=fit_max), alpha = 0.5, linetype=0)+
  scale_colour_manual(values=c("#00CC33","#0000FF"))+
  scale_fill_manual(values=c("#66FF99","#6666FF"))+theme_classic()+
  scale_x_continuous(name="Age (years)", expand = c(0,0), limits=c(5, 17.5), 
                     breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17), 
                     labels = c('5','6','7','8','9','10','11','12','13','14','15','16','17'))+
  scale_y_continuous(name="Mode share of cycling (%)", expand = c(0,0), limits=c(0, 30.5), 
                     breaks = c(0,5,10,15,20,25,30), labels = c('0','5','10','15','20','25','30'))+
  annotate("text", x = 13.6, y = 16, label = "Boys", size=9)+
  annotate("text", x = 14.1, y = 10, label = "Girls", size=9)+
  ggtitle("d) Urban: 2017")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        plot.title = element_text(size=32, colour = "black"),
        legend.position = "none",
        panel.border = element_blank(),
        panel.background = element_blank())

library(ggpubr)
ggarrange(a,b,c,d, nrow = 2, ncol = 2)
ggsave("cycle_age_gam.png", height=12, width=15)

#ggsave("cycle_age_gam.tiff",height=12, width=15,device='tiff', dpi=400, compression = "lzw")



#### 12. Tables: cycling levels by distance across years stratified by gender and rural/urban ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

edu_temp$sex <- as.factor(edu_temp$sex)

edu_temp <- edu_temp[!is.na(edu_temp$dist_inst),]
edu_temp$dist_inst[which(edu_temp$dist_inst==1)]<-"<1"
edu_temp$dist_inst[which(edu_temp$dist_inst==2)]<-"[1,2)"
edu_temp$dist_inst[which(edu_temp$dist_inst==3)]<-"[2,3)"
edu_temp$dist_inst[which(edu_temp$dist_inst==4)]<-"[3,5)"
edu_temp$dist_inst[which(edu_temp$dist_inst==5)]<-">=5"

edu_temp$dist_inst <- factor(edu_temp$dist_inst, 
                             levels = c("<1","[1,2)","[2,3)","[3,5)",">=5"), 
                             ordered = TRUE)

##survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

cycle_dist <- svyby(~ cycle, by = ~ dist_inst+sex+year+sector, svyd, svymean, vartype = c("se","ci"))
colnames(cycle_dist)[colnames(cycle_dist) == "cycle"] <- "mean"
cycle_dist$mean<-round(cycle_dist$mean*100, 1)
cycle_dist$ci_l<-round(cycle_dist$ci_l*100, 1)
cycle_dist$ci_u<-round(cycle_dist$ci_u*100, 1)
rownames(cycle_dist)<-NULL
cycle_dist$se<-NULL
cycle_dist$mean <- paste0(cycle_dist$mean, " ","(", cycle_dist$ci_l,",",cycle_dist$ci_u,")")
cycle_dist$ci_l<-NULL
cycle_dist$ci_u<-NULL

##Rural
cycle_dist_r <- cycle_dist %>% filter(sector=="rural")

cycle_dist_rm <- cycle_dist_r %>% filter(sex=="Male") %>% select(-c("sex","sector")) %>% spread(year, mean)
cycle_dist_rf <- cycle_dist_r %>% filter(sex=="Female") %>% select(-c("sex","sector")) %>% spread(year, mean)

cycle_dist_r <- cycle_dist_rf %>% left_join(cycle_dist_rm, by="dist_inst")

write.csv(cycle_dist_r, "cycle_dist_r.csv")

##Urban
cycle_dist_u <- cycle_dist %>% filter(sector=="urban")

cycle_dist_um <- cycle_dist_u %>% filter(sex=="Male") %>% select(-c("sex","sector")) %>% spread(year, mean)
cycle_dist_uf <- cycle_dist_u %>% filter(sex=="Female") %>% select(-c("sex","sector")) %>% spread(year, mean)

cycle_dist_u <- cycle_dist_uf %>% left_join(cycle_dist_um, by="dist_inst")

write.csv(cycle_dist_u, "cycle_dist_u.csv")



#### 13. Bar plots: cycling levels by distance across years stratified by gender and rural/urban ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Boys"
edu_temp$sex[edu_temp$sex==2]<-"Girls"

edu_temp$sex <- as.factor(edu_temp$sex)

edu_temp <- edu_temp[!is.na(edu_temp$dist_inst),]
edu_temp$dist_inst[which(edu_temp$dist_inst==1)]<-"<1"
edu_temp$dist_inst[which(edu_temp$dist_inst==2)]<-"[1,2)"
edu_temp$dist_inst[which(edu_temp$dist_inst==3)]<-"[2,3)"
edu_temp$dist_inst[which(edu_temp$dist_inst==4)]<-"[3,5)"
edu_temp$dist_inst[which(edu_temp$dist_inst==5)]<-">=5"

edu_temp$dist_inst <- factor(edu_temp$dist_inst, 
                             levels = c("<1","[1,2)","[2,3)","[3,5)",">=5"), 
                             ordered = TRUE)

##survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

cycle_dist <- svyby(~ cycle, by = ~ dist_inst+sex+year+sector, svyd, svymean, vartype = c("se","ci"))
colnames(cycle_dist)[colnames(cycle_dist) == "cycle"] <- "mean"
cycle_dist$mean<-round(cycle_dist$mean*100, 1)
cycle_dist$ci_l<-round(cycle_dist$ci_l*100, 1)
cycle_dist$ci_u<-round(cycle_dist$ci_u*100, 1)
rownames(cycle_dist)<-NULL
cycle_dist$se<-NULL


cycle_dist_r_2007 <- cycle_dist %>% filter(year==2007) %>% filter(sector=="rural")
cycle_dist_r_2017 <- cycle_dist %>% filter(year==2017) %>% filter(sector=="rural")
cycle_dist_u_2007 <- cycle_dist %>% filter(year==2007) %>% filter(sector=="urban")
cycle_dist_u_2017 <- cycle_dist %>% filter(year==2017) %>% filter(sector=="urban")

a <- ggplot(cycle_dist_r_2007, aes(x = dist_inst,y = mean, fill = sex)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_manual(values=c("#6666FF","#66FF99"))+
  #geom_text(aes(label = mean), size = 6, position = position_dodge(width = .9),hjust=1.0, vjust = -0.2, colour = "black")+
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u,group = factor(sex, level = c('Boys','Girls'))), 
                width=0.3, size=0.8,position=position_dodge(0.9,preserve='single'),color="black") +
  ggtitle("a) Rural: 2007") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name="Mode share of cycling (%)",expand = c(0,0), limits=c(0, 47), 
                     breaks = c(0,5,10,15,20,25,30,35,40,45), labels = c('0','5','10','15','20','25','30','35','40','45'))+
  xlab("Distance to school (km)")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=32, colour = "black"),
        legend.title=element_blank(),
        legend.position = "none",
        legend.text = element_text(size=32, colour = "black"))

b <- ggplot(cycle_dist_r_2017, aes(x = dist_inst,y = mean, fill = sex)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_manual(values=c("#6666FF","#66FF99"))+
  #geom_text(aes(label = mean), size = 6, position = position_dodge(width = .9),hjust=1.0, vjust = -0.2, colour = "black")+
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u,group = factor(sex, level = c('Boys','Girls'))), 
                width=0.3, size=0.8,position=position_dodge(0.9,preserve='single'),color="black") +
  ggtitle("b) Rural: 2017") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name="Mode share of cycling (%)",expand = c(0,0), limits=c(0, 47), 
                     breaks = c(0,5,10,15,20,25,30,35,40,45), labels = c('0','5','10','15','20','25','30','35','40','45'))+
  xlab("Distance to school (km)")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=32, colour = "black"),
        legend.title=element_blank(),
        legend.position = "none",
        legend.text = element_text(size=32, colour = "black"))

c <- ggplot(cycle_dist_u_2007, aes(x = dist_inst,y = mean, fill = sex)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_manual(values=c("#6666FF","#66FF99"))+
  #geom_text(aes(label = mean), size = 6, position = position_dodge(width = .9),hjust=1.0, vjust = -0.2, colour = "black")+
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u,group = factor(sex, level = c('Boys','Girls'))), 
                width=0.3, size=0.8,position=position_dodge(0.9,preserve='single'),color="black") +
  ggtitle("c) Urban: 2007") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name="Mode share of cycling (%)",expand = c(0,0), limits=c(0, 47), 
                     breaks = c(0,5,10,15,20,25,30,35,40,45), labels = c('0','5','10','15','20','25','30','35','40','45'))+
  xlab("Distance to school (km)")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=32, colour = "black"),
        legend.title=element_blank(),
        legend.position = "none",
        legend.text = element_text(size=32, colour = "black"))

d <- ggplot(cycle_dist_u_2017, aes(x = dist_inst,y = mean, fill = sex)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_manual(values=c("#6666FF","#66FF99"))+
  #geom_text(aes(label = mean), size = 6, position = position_dodge(width = .9),hjust=1.0, vjust = -0.2, colour = "black")+
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u,group = factor(sex, level = c('Boys','Girls'))), 
                width=0.3, size=0.8,position=position_dodge(0.9,preserve='single'),color="black") +
  ggtitle("d) Urban: 2017") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name="Mode share of cycling (%)",expand = c(0,0), limits=c(0, 47), 
                     breaks = c(0,5,10,15,20,25,30,35,40,45), labels = c('0','5','10','15','20','25','30','35','40','45'))+
  xlab("Distance to school (km)")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=32, colour = "black"),
        legend.title=element_blank(),
        legend.position = "none",
        legend.text = element_text(size=32, colour = "black"))

ggarrange(a,b,c,d,nrow=2,ncol=2,common.legend = TRUE, legend = "bottom")
ggsave("cycle_dist_bar_plot.png", width=15, height = 12)

#ggsave("cycle_dist_bar_plot.tiff",height=12, width=15,device='tiff', dpi=400, compression = "lzw")


#### 14. Box plots: Change in cycling levels and BDS implementation ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

##survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

##overall cycling levels by state
cycle_p <- svyby(~ cycle, by = ~ state_name+year, svyd, svymean)
colnames(cycle_p)[colnames(cycle_p) == "cycle"] <- "mean"
cycle_p$mean<-round(cycle_p$mean*100,1)
cycle_p$se<-NULL
cycle_p <- cycle_p %>% filter(!year==2014) %>% spread(year, mean)
cycle_p$p_change <- cycle_p$`2017`-cycle_p$`2007`
cycle_p <- cycle_p %>% select(state_name, p_change)

##cycling levels in rural and urban areas of states
cycle_ru <- svyby(~ cycle, by = ~ state_name+year+sector, svyd, svymean)
colnames(cycle_ru)[colnames(cycle_ru) == "cycle"] <- "mean"
cycle_ru$mean<-round(cycle_ru$mean*100,1)
cycle_ru$se<-NULL
cycle_r <- cycle_ru %>% filter(!year==2014) %>% filter(sector=="rural") %>% select(-c("sector")) %>% spread(year, mean)
cycle_r$r_change <- cycle_r$`2017`-cycle_r$`2007`
cycle_r <- cycle_r %>% select(state_name, r_change)

cycle_u <- cycle_ru %>% filter(!year==2014) %>% filter(sector=="urban") %>% select(-c("sector")) %>% spread(year, mean)
cycle_u$u_change <- cycle_u$`2017`-cycle_u$`2007`
cycle_u <- cycle_u %>% select(state_name, u_change)

##cycling levels in rural and urban areas of states by gender
cycle_rumf <- svyby(~ cycle, by = ~ state_name+year+sector+sex, svyd, svymean)
colnames(cycle_rumf)[colnames(cycle_rumf) == "cycle"] <- "mean"
cycle_rumf$mean<-round(cycle_rumf$mean*100,1)
cycle_rumf$se<-NULL
cycle_rf <- cycle_rumf %>% filter(!year==2014) %>% filter(sector=="rural" & sex=="Female") %>% 
  select(-c("sector","sex")) %>% spread(year, mean)
cycle_rf$rf_change <- cycle_rf$`2017`-cycle_rf$`2007`
cycle_rf <- cycle_rf %>% select(state_name, rf_change)

cycle_rm <- cycle_rumf %>% filter(!year==2014) %>% filter(sector=="rural" & sex=="Male") %>% 
  select(-c("sector","sex")) %>% spread(year, mean)
cycle_rm$rm_change <- cycle_rm$`2017`-cycle_rm$`2007`
cycle_rm <- cycle_rm %>% select(state_name, rm_change)

cycle_uf <- cycle_rumf %>% filter(!year==2014) %>% filter(sector=="urban" & sex=="Female") %>% 
  select(-c("sector","sex")) %>% spread(year, mean)
cycle_uf$uf_change <- cycle_uf$`2017`-cycle_uf$`2007`
cycle_uf <- cycle_uf %>% select(state_name, uf_change)

cycle_um <- cycle_rumf %>% filter(!year==2014) %>% filter(sector=="urban" & sex=="Male") %>% 
  select(-c("sector","sex")) %>% spread(year, mean)
cycle_um$um_change <- cycle_um$`2017`-cycle_um$`2007`
cycle_um <- cycle_um %>% select(state_name, um_change)

##BDS data
bds <- read.csv("Bicycle distribution India/BDS starting year.csv")
bds$years_till_2017 <- NA
bds$years_till_2017 <- 2017-bds$bds_year
bds$years_till_2017[is.na(bds$years_till_2017)] <- 0
bds$bds_present <- 0
bds$bds_present[bds$years_till_2017>0]<-1

bds_join <- bds[,c("state_name","bds_year","bds_present","years_till_2017","rural","urban","RF","RM","UF","UM")]

##join BDS data to cycling levels
cycle_p <- cycle_p %>% left_join(cycle_r, by="state_name") %>% left_join(cycle_u, by="state_name") %>% 
  left_join(cycle_rf, by="state_name") %>% left_join(cycle_rm, by="state_name") %>% left_join(cycle_uf, by="state_name") %>% 
  left_join(cycle_um, by="state_name") %>% left_join(bds_join, by="state_name")

cycle_p$RF[is.na(cycle_p$RF)]<-0
cycle_p$RM[is.na(cycle_p$RM)]<-0
cycle_p$UF[is.na(cycle_p$UF)]<-0
cycle_p$UM[is.na(cycle_p$UM)]<-0

cycle_p$RF[cycle_p$RF==1]<-"BDS"
cycle_p$RF[cycle_p$RF==0]<-"No BDS"
cycle_p$RM[cycle_p$RM==1]<-"BDS"
cycle_p$RM[cycle_p$RM==0]<-"No BDS"
cycle_p$UF[cycle_p$UF==1]<-"BDS"
cycle_p$UF[cycle_p$UF==0]<-"No BDS"
cycle_p$UM[cycle_p$UM==1]<-"BDS"
cycle_p$UM[cycle_p$UM==0]<-"No BDS"

cycle_p$bds_present<-as.factor(cycle_p$bds_present)
cycle_p$RF<-as.factor(cycle_p$RF)
cycle_p$RM<-as.factor(cycle_p$RM)
cycle_p$UF<-as.factor(cycle_p$UF)
cycle_p$UM<-as.factor(cycle_p$UM)

#write.csv(cycle_p,"bds_comparison.csv")

##plot: overall
means <- aggregate(p_change ~  bds_present, cycle_p, mean)
# means$p_change<-round(means$p_change, 1)
# 
# a<-ggplot(cycle_p, aes(x = bds_present, y = p_change))+ 
#   geom_boxplot()+
#   ggtitle("a) Total") +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   stat_summary(fun=mean, colour="blue", geom="point", size=4, shape=18) + 
#   geom_label(data=means,aes(x=bds_present, y=19, label=paste0("Mean","=", p_change)), size=5.5) +
#   #geom_text(data = means, aes(label = p_change, y = p_change, x = bds_present),size=4.5,colour="black")+
#   scale_y_continuous(name="Change in cycling levels",expand = c(0,0), limits=c(-16, 21), 
#                      breaks = seq(-16, 20, by = 4), minor_breaks = seq(-8 , 16, 4))+
#   xlab("BDS present")+
#   #geom_hline(yintercept = 0, linetype='dashed', linewidth=1)+
#   theme_bw()+ 
#   #theme(panel.grid.minor.y = element_line(colour="gray", size=0.5), panel.grid.minor.x = element_blank(), 
#        # panel.grid.major.y = element_line(colour="gray", size=0.5), panel.grid.major.x = element_blank())+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
#   theme(axis.line = element_line(colour = "black"),
#         axis.text.x = element_text(size=18, colour = "black"),
#         axis.text.y = element_text(size=18, colour = "black"), 
#         axis.title.y = element_text(size=20, colour = "black"),
#         axis.title.x = element_text(size=20, colour = "black"),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         plot.title = element_text(size=24, colour = "black"))


# ggplot(cycle_p, aes(x = as.factor(rural), y = r_change))+ 
#   geom_boxplot()
# 
# ggplot(cycle_p, aes(x = as.factor(urban), y = u_change))+ 
#   geom_boxplot()

##plot: Rural girls
means <- cycle_p %>% group_by(RF) %>% summarize(sample_size = n(), mean=mean(rf_change)) 
means$mean<-round(means$mean, 1)

a <- ggplot(cycle_p, aes(x = RF, y = rf_change))+ 
  geom_boxplot()+
  ggtitle("a) Rural females") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun=mean, colour="blue", geom="point", size=4, shape=18) + 
  geom_label(data=means,aes(x=RF, y=19, label=paste0("Mean","=", mean)), size=8.5, label.size = NA, lwd=0) +
  geom_label(data=means,aes(x=RF, y=14, label=paste0("n","=", sample_size)), size=8.5, label.size = NA, lwd=0) +
  #geom_text(data = means, aes(label = p_change, y = p_change, x = bds_present),size=4.5,colour="black")+
  scale_y_continuous(name="Percentage point change in cycling levels",expand = c(0,0), limits=c(-16.5, 21), 
                     breaks = seq(-16, 20, by = 4), minor_breaks = seq(-8 , 16, 4))+
  xlab("BDS present")+
  #geom_hline(yintercept = 0, linetype='dashed', linewidth=1)+
  theme_bw()+ 
  #theme(panel.grid.minor.y = element_line(colour="gray", size=0.5), panel.grid.minor.x = element_blank(), 
  # panel.grid.major.y = element_line(colour="gray", size=0.5), panel.grid.major.x = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.y = element_text(size=24, colour = "black"),
        axis.title.x = element_text(size=24, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=28, colour = "black"))

##plot: Rural boys
means <- cycle_p %>% group_by(RM) %>% summarize(sample_size = n(), mean=mean(rm_change)) 
means$mean<-round(means$mean, 1)

b <- ggplot(cycle_p, aes(x = RM, y = rm_change))+ 
  geom_boxplot()+
  ggtitle("b) Rural males") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun=mean, colour="blue", geom="point", size=4, shape=18) + 
  geom_label(data=means,aes(x=RM, y=19, label=paste0("Mean","=", mean)), size=8.5, label.size = NA, lwd=0) +
  geom_label(data=means,aes(x=RM, y=14, label=paste0("n","=", sample_size)), size=8.5, label.size = NA, lwd=0) +
  #geom_text(data = means, aes(label = p_change, y = p_change, x = bds_present),size=4.5,colour="black")+
  scale_y_continuous(name="Percentage point change in cycling levels",expand = c(0,0), limits=c(-16.5, 21), 
                     breaks = seq(-16, 20, by = 4), minor_breaks = seq(-8 , 16, 4))+
  #geom_text(data = means, aes(label = p_change, y = p_change, x = bds_present),size=4.5,colour="black")+
  xlab("BDS present")+
  #geom_hline(yintercept = 0, linetype='dashed', linewidth=1)+
  theme_bw()+ 
  #theme(panel.grid.minor.y = element_line(colour="gray", size=0.5), panel.grid.minor.x = element_blank(), 
  # panel.grid.major.y = element_line(colour="gray", size=0.5), panel.grid.major.x = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.y = element_text(size=24, colour = "black"),
        axis.title.x = element_text(size=24, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=28, colour = "black"))

##plot: Urban girls
means <- cycle_p %>% group_by(UF) %>% summarize(sample_size = n(), mean=mean(uf_change)) 
means$mean<-round(means$mean, 1)

c <- ggplot(cycle_p, aes(x = UF, y = uf_change))+ 
  geom_boxplot()+
  ggtitle("c) Urban females") +
  theme(plot.title = element_text(hjust = 0.5)) +
  #stat_summary(fun=mean, colour="blue", geom="point", size=4, shape=18) + 
  geom_point(data = means, mapping = aes(x = UF, y = mean),colour="blue", size=4, shape=18)+
  geom_label(data=means,aes(x=UF, y=19, label=paste0("Mean","=", mean)), size=8.5, label.size = NA, lwd=0) +
  geom_label(data=means,aes(x=UF, y=14, label=paste0("n","=", sample_size)), size=8.5, label.size = NA, lwd=0) +
  #geom_text(data = means, aes(label = p_change, y = p_change, x = bds_present),size=4.5,colour="black")+
  scale_y_continuous(name="Percentage point change in cycling levels",expand = c(0,0), limits=c(-16.5, 21), 
                     breaks = seq(-16, 20, by = 4), minor_breaks = seq(-8 , 16, 4))+
  xlab("BDS present")+
  #geom_hline(yintercept = 0, linetype='dashed', linewidth=1)+
  theme_bw()+ 
  #theme(panel.grid.minor.y = element_line(colour="gray", size=0.5), panel.grid.minor.x = element_blank(), 
  # panel.grid.major.y = element_line(colour="gray", size=0.5), panel.grid.major.x = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.y = element_text(size=24, colour = "black"),
        axis.title.x = element_text(size=24, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=28, colour = "black"))

##plot: Urban boys
means <- cycle_p %>% group_by(UM) %>% summarize(sample_size = n(), mean=mean(um_change)) 
means$mean<-round(means$mean, 1)

d <- ggplot(cycle_p, aes(x = UM, y = um_change))+ 
  geom_boxplot()+
  ggtitle("d) Urban males") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_summary(fun=mean, colour="blue", geom="point", size=4, shape=18) + 
  geom_label(data=means,aes(x=UM, y=19, label=paste0("Mean","=", mean)), size=8.5, label.size = NA, lwd=0) +
  geom_label(data=means,aes(x=UM, y=14, label=paste0("n","=", sample_size)), size=8.5, label.size = NA, lwd=0) +
  #geom_text(data = means, aes(label = p_change, y = p_change, x = bds_present),size=4.5,colour="black")+
  scale_y_continuous(name="Percentage point change in cycling levels",expand = c(0,0), limits=c(-16.5, 21), 
                     breaks = seq(-16, 20, by = 4), minor_breaks = seq(-8 , 16, 4))+
  xlab("BDS present")+
  #geom_hline(yintercept = 0, linetype='dashed', linewidth=1)+
  theme_bw()+ 
  #theme(panel.grid.minor.y = element_line(colour="gray", size=0.5), panel.grid.minor.x = element_blank(), 
  # panel.grid.major.y = element_line(colour="gray", size=0.5), panel.grid.major.x = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.y = element_text(size=24, colour = "black"),
        axis.title.x = element_text(size=24, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=28, colour = "black"))

ggarrange(a,b,c,d, nrow=2, ncol=2)
ggsave("boxplot_bds_cyc.png", height=15, width=12)

#ggsave("boxplot_bds_cyc.tiff",height=15, width=14,device='tiff', dpi=400, compression = "lzw")


#### 15. Tables: Percentage point change in cycling mode share between 2007 and 2017 relative to number of years of BDS stratified by rural/urban and gender ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

##survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

cycle <- svyby(~ cycle, by = ~ state_name+year+sector+sex, svyd, svymean)
colnames(cycle)[colnames(cycle) == "cycle"] <- "mean"
cycle$mean<-round(cycle$mean*100, 4)
cycle$se<-round(cycle$se*100, 4)
rownames(cycle)<-NULL

cycle <- cycle %>% filter(!year==2014)

cycle_rf_2007 <- cycle %>% filter(sector=="rural" & year==2007 & sex=="Female") %>% select(-c("year","sector","sex"))
cycle_rf_2017 <- cycle %>% filter(sector=="rural" & year==2017 & sex=="Female") %>% select(-c("year","sector","sex"))

cycle_rm_2007 <- cycle %>% filter(sector=="rural" & year==2007 & sex=="Male") %>% select(-c("year","sector","sex"))
cycle_rm_2017 <- cycle %>% filter(sector=="rural" & year==2017 & sex=="Male") %>% select(-c("year","sector","sex"))

cycle_uf_2007 <- cycle %>% filter(sector=="urban" & year==2007 & sex=="Female") %>% select(-c("year","sector","sex"))
cycle_uf_2017 <- cycle %>% filter(sector=="urban" & year==2017 & sex=="Female") %>% select(-c("year","sector","sex"))

cycle_um_2007 <- cycle %>% filter(sector=="urban" & year==2007 & sex=="Male") %>% select(-c("year","sector","sex"))
cycle_um_2017 <- cycle %>% filter(sector=="urban" & year==2017 & sex=="Male") %>% select(-c("year","sector","sex"))

cycle_rf <- cycle_rf_2007 %>% left_join(cycle_rf_2017, by="state_name")
cycle_rm <- cycle_rm_2007 %>% left_join(cycle_rm_2017, by="state_name")
cycle_uf <- cycle_uf_2007 %>% left_join(cycle_uf_2017, by="state_name")
cycle_um <- cycle_um_2007 %>% left_join(cycle_um_2017, by="state_name")

###bds data
bds <- read.csv("Bicycle distribution India/BDS starting year.csv")

##calculating difference in cycling levels for the four-subgroups (rural girls, rural boys, urban girls and urban boys)
cycle_rf$mean.x <- round(cycle_rf$mean.x, 1)
cycle_rf$mean.y <- round(cycle_rf$mean.y, 1)

cycle_rm$mean.x <- round(cycle_rm$mean.x, 1)
cycle_rm$mean.y <- round(cycle_rm$mean.y, 1)

cycle_uf$mean.x <- round(cycle_uf$mean.x, 1)
cycle_uf$mean.y <- round(cycle_uf$mean.y, 1)

cycle_um$mean.x <- round(cycle_um$mean.x, 1)
cycle_um$mean.y <- round(cycle_um$mean.y, 1)

cycle_rf$pp_change_rf <- cycle_rf$mean.y - cycle_rf$mean.x
cycle_rf <- cycle_rf %>% select(state_name, pp_change_rf)

cycle_rm$pp_change_rm <- cycle_rm$mean.y - cycle_rm$mean.x
cycle_rm <- cycle_rm %>% select(state_name, pp_change_rm)

cycle_uf$pp_change_uf <- cycle_uf$mean.y - cycle_uf$mean.x
cycle_uf <- cycle_uf %>% select(state_name, pp_change_uf)

cycle_um$pp_change_um <- cycle_um$mean.y - cycle_um$mean.x
cycle_um <- cycle_um %>% select(state_name, pp_change_um)

##number of years with BDS till year 2017 for each state
bds$years_till_2017 <- NA
bds$years_till_2017 <- 2017-bds$bds_year
bds$years_till_2017[is.na(bds$years_till_2017)] <- 0


cycle_bds_pp <- bds %>% left_join(cycle_rf, by="state_name") %>% left_join(cycle_rm, by="state_name") %>% 
  left_join(cycle_uf, by="state_name") %>% left_join(cycle_um, by="state_name")

write.csv(cycle_bds_pp, "cycle_bds_pp_4.csv")



#### 16. Tables: Logistic regression between likelihood of cycling and user demographics, distance across years stratified by rural/urban ####

###data

edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1

## sex
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"1_m"
edu_temp$sex[edu_temp$sex==2]<-"2_f"

## age category
edu_temp$age_grp <- NA
edu_temp$age_grp[which(edu_temp$age>4 & edu_temp$age<11)]<-"1_age"
edu_temp$age_grp[which(edu_temp$age>10 & edu_temp$age<14)]<-"2_age"
edu_temp$age_grp[which(edu_temp$age>13 & edu_temp$age<16)]<-"3_age"
edu_temp$age_grp[which(edu_temp$age>15 & edu_temp$age<18)]<-"4_age"

## caste
colnames(edu_temp)[colnames(edu_temp) == "social_group"] <- "caste"

edu_temp$caste[which(edu_temp$caste==1)]<-"4_st"
edu_temp$caste[which(edu_temp$caste==2)]<-"3_sc"
edu_temp$caste[which(edu_temp$caste==3)]<-"2_obc"
edu_temp$caste[which(edu_temp$caste==9)]<-"1_others"

## hh monthly exp per capita (quintiles)


## grade
# edu_temp$edu_level_curr_attend <- factor(edu_temp$edu_level_curr_attend, 
#                                          levels = c("primary","upper primary/middle","secondary","higher secondary"), 
#                                          ordered = TRUE)

## distance to school
edu_temp$dist_inst[which(edu_temp$dist_inst==1)]<-"1_d" #<1 km
edu_temp$dist_inst[which(edu_temp$dist_inst==2)]<-"2_d"
edu_temp$dist_inst[which(edu_temp$dist_inst==3)]<-"3_d"
edu_temp$dist_inst[which(edu_temp$dist_inst==4)]<-"4_d"
edu_temp$dist_inst[which(edu_temp$dist_inst==5)]<-"5_d"

## type of school
edu_temp$type_of_inst[which(edu_temp$type_of_inst %in% c("pvt aided","pvt unaided"))]<-"1_pvt"
edu_temp$type_of_inst[which(edu_temp$type_of_inst %in% c("government"))]<-"2_govt"

## hh size
# edu_temp$hh_size_grp <- NA
# edu_temp$hh_size_grp[which(edu_temp$hh_size==1)]<-"1_hhsize"
# edu_temp$hh_size_grp[which(edu_temp$hh_size>1 & edu_temp$hh_size<4)]<-"2_hhsize"
# edu_temp$hh_size_grp[which(edu_temp$hh_size>3 & edu_temp$hh_size<6)]<-"3_hhsize"
# edu_temp$hh_size_grp[which(edu_temp$hh_size>5)]<-"4_hhsize"


## survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

### Rural: logistic regression models
model_r_2007 <- svyglm(cycle ~ factor(sex) + factor(age_grp) + factor(dist_inst) +
                         factor(caste) + factor(type_of_inst) + factor(hepc_q),
                       family=quasibinomial, subset(svyd, (year==2007 & sector=="rural"))) 

model_r_2017 <- svyglm(cycle ~ factor(sex) + factor(age_grp) + factor(dist_inst) +
                         factor(caste) + factor(type_of_inst) + factor(hepc_q),
                       family=quasibinomial, subset(svyd, (year==2017 & sector=="rural"))) 

res1 <- summ(model_r_2007, confint = getOption("summ-confint", TRUE), exp = TRUE, pvals = FALSE)
res2 <- summ(model_r_2017, confint = getOption("summ-confint", TRUE), exp = TRUE, pvals = FALSE)

res1 <- tidy(res1)
res2 <- tidy(res2)

# res1 <- res1[-c(1), ]
# res2 <- res2[-c(1), ]
# res3 <- res3[-c(1), ]
# res4 <- res4[-c(1), ]

res1$year<-2007
res2$year<-2017

res_r <- rbind(res1, res2)

##p-value
res_r$star <- NA
res_r$star[res_r$p.value<0.001] <- "***"
res_r$star[res_r$p.value>=0.001 & res_r$p.value<0.01] <- "**"
res_r$star[res_r$p.value>=0.01 & res_r$p.value<0.05] <- "*"
res_r$star[is.na(res_r$star)] <- " "

##estimate and 95% confidence interval
res_r$estimate <- format(round(res_r$estimate,digits=2),nsmall=2)
res_r$conf.low <- round(res_r$conf.low,digits=2)
res_r$conf.high <- round(res_r$conf.high,digits=2)

res_r$or <- paste0(res_r$estimate, " (", res_r$conf.low, ", ", res_r$conf.high, ")", res_r$star)

res_r <- res_r %>% select(term, year, or) %>% spread(year, or) %>% mutate(
  term =
    case_match(
      term,
      "(Intercept)" ~ "(Intercept)",
      "factor(sex)2_f" ~ "Gender Female",
      "factor(age_grp)2_age" ~ "Age 11-13 years",
      "factor(age_grp)3_age" ~ "Age 14-15 years",
      "factor(age_grp)4_age" ~ "Age 16-17 years",
      "factor(dist_inst)2_d" ~ "Distance to school 1-2 km",
      "factor(dist_inst)3_d" ~ "Distance to school 2-3 km",
      "factor(dist_inst)4_d" ~ "Distance to school 3-5 km",
      "factor(dist_inst)5_d" ~ "Distance to school 5+ km",
      "factor(caste)2_obc" ~ "Caste OBC",
      "factor(caste)3_sc" ~ "Caste SC",
      "factor(caste)4_st" ~ "Caste ST",
      "factor(type_of_inst)2_govt" ~ "Type of school Government",
      "factor(hepc_q)Q2" ~ "Household expenditure per person Q2",
      "factor(hepc_q)Q3" ~ "Household expenditure per person Q3",
      "factor(hepc_q)Q4" ~ "Household expenditure per person Q4",
      "factor(hepc_q)Q5" ~ "Household expenditure per person Q5"
    )
)

write.csv(res_r, "rural_regression.csv")

###Urban: logistic regression models

model_u_2007 <- svyglm(cycle ~ factor(sex) + factor(age_grp) + factor(dist_inst) +
                         factor(caste) + factor(type_of_inst) + factor(hepc_q),
                       family=quasibinomial, subset(svyd, (year==2007 & sector=="urban"))) 

model_u_2017 <- svyglm(cycle ~ factor(sex) + factor(age_grp) + factor(dist_inst) +
                         factor(caste) + factor(type_of_inst) + factor(hepc_q),
                       family=quasibinomial, subset(svyd, (year==2017 & sector=="urban"))) 

res3 <- summ(model_u_2007, confint = getOption("summ-confint", TRUE), exp = TRUE, pvals = FALSE)
res4 <- summ(model_u_2017, confint = getOption("summ-confint", TRUE), exp = TRUE, pvals = FALSE)

res3 <- tidy(res3)
res4 <- tidy(res4)

res3$year<-2007
res4$year<-2017

res_u <- rbind(res3, res4)

res_u$star <- NA
res_u$star[res_u$p.value<0.001] <- "***"
res_u$star[res_u$p.value>=0.001 & res_u$p.value<0.01] <- "**"
res_u$star[res_u$p.value>=0.01 & res_u$p.value<0.05] <- "*"
res_u$star[is.na(res_u$star)] <- " "

res_u$estimate <- format(round(res_u$estimate,digits=2),nsmall=2)
res_u$conf.low <- format(round(res_u$conf.low,digits=2),nsmall=2)
res_u$conf.high <- format(round(res_u$conf.high,digits=2),nsmall=2)

res_u$or <- paste0(res_u$estimate, " (", res_u$conf.low, ", ", res_u$conf.high, ")", res_u$star)

res_u <- res_u %>% select(term, year, or) %>% spread(year, or) %>% mutate(
  term =
    case_match(
      term,
      "(Intercept)" ~ "(Intercept)",
      "factor(sex)2_f" ~ "Gender Female",
      "factor(age_grp)2_age" ~ "Age 11-13 years",
      "factor(age_grp)3_age" ~ "Age 14-15 years",
      "factor(age_grp)4_age" ~ "Age 16-17 years",
      "factor(dist_inst)2_d" ~ "Distance to school 1-2 km",
      "factor(dist_inst)3_d" ~ "Distance to school 2-3 km",
      "factor(dist_inst)4_d" ~ "Distance to school 3-5 km",
      "factor(dist_inst)5_d" ~ "Distance to school 5+ km",
      "factor(caste)2_obc" ~ "Caste OBC",
      "factor(caste)3_sc" ~ "Caste SC",
      "factor(caste)4_st" ~ "Caste ST",
      "factor(type_of_inst)2_govt" ~ "Type of school Government",
      "factor(hepc_q)Q2" ~ "Household expenditure per person Q2",
      "factor(hepc_q)Q3" ~ "Household expenditure per person Q3",
      "factor(hepc_q)Q4" ~ "Household expenditure per person Q4",
      "factor(hepc_q)Q5" ~ "Household expenditure per person Q5"
    )
)

write.csv(res_u, "urban_regression.csv")



#### 17. Forest plots: Logistic regression between likelihood of cycling and user demographics, distance across years stratified by rural/urban ####

###data

edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1

## sex
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"1_m"
edu_temp$sex[edu_temp$sex==2]<-"2_f"

## age category
edu_temp$age_grp <- NA
edu_temp$age_grp[which(edu_temp$age>4 & edu_temp$age<11)]<-"1_age"
edu_temp$age_grp[which(edu_temp$age>10 & edu_temp$age<14)]<-"2_age"
edu_temp$age_grp[which(edu_temp$age>13 & edu_temp$age<16)]<-"3_age"
edu_temp$age_grp[which(edu_temp$age>15 & edu_temp$age<18)]<-"4_age"

## caste
colnames(edu_temp)[colnames(edu_temp) == "social_group"] <- "caste"

edu_temp$caste[which(edu_temp$caste==1)]<-"4_st"
edu_temp$caste[which(edu_temp$caste==2)]<-"3_sc"
edu_temp$caste[which(edu_temp$caste==3)]<-"2_obc"
edu_temp$caste[which(edu_temp$caste==9)]<-"1_others"

## grade
# edu_temp$edu_level_curr_attend <- factor(edu_temp$edu_level_curr_attend, 
#                                          levels = c("primary","upper primary/middle","secondary","higher secondary"), 
#                                          ordered = TRUE)

## distance to school
edu_temp$dist_inst[which(edu_temp$dist_inst==1)]<-"1_d" #<1 km
edu_temp$dist_inst[which(edu_temp$dist_inst==2)]<-"2_d"
edu_temp$dist_inst[which(edu_temp$dist_inst==3)]<-"3_d"
edu_temp$dist_inst[which(edu_temp$dist_inst==4)]<-"4_d"
edu_temp$dist_inst[which(edu_temp$dist_inst==5)]<-"5_d"

## type of school
edu_temp$type_of_inst[which(edu_temp$type_of_inst %in% c("pvt aided","pvt unaided"))]<-"1_pvt"
edu_temp$type_of_inst[which(edu_temp$type_of_inst %in% c("government"))]<-"2_govt"

## survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

#### Rural: logistic regression models
model_r_2007 <- svyglm(cycle ~ factor(sex) + factor(age_grp) + factor(dist_inst) +
                         factor(caste) + factor(type_of_inst) + factor(hepc_q),
                       family=quasibinomial, subset(svyd, (year==2007 & sector=="rural"))) 

model_r_2017 <- svyglm(cycle ~ factor(sex) + factor(age_grp) + factor(dist_inst) +
                         factor(caste) + factor(type_of_inst) + factor(hepc_q),
                       family=quasibinomial, subset(svyd, (year==2017 & sector=="rural"))) 

model_u_2007 <- svyglm(cycle ~ factor(sex) + factor(age_grp) + factor(dist_inst) +
                         factor(caste) + factor(type_of_inst) + factor(hepc_q),
                       family=quasibinomial, subset(svyd, (year==2007 & sector=="urban"))) 

model_u_2017 <- svyglm(cycle ~ factor(sex) + factor(age_grp) + factor(dist_inst) +
                         factor(caste) + factor(type_of_inst) + factor(hepc_q),
                       family=quasibinomial, subset(svyd, (year==2017 & sector=="urban"))) 

res1 <- summ(model_r_2007, confint = getOption("summ-confint", TRUE), exp = TRUE, pvals = FALSE)
res2 <- summ(model_r_2017, confint = getOption("summ-confint", TRUE), exp = TRUE, pvals = FALSE)
res3 <- summ(model_u_2007, confint = getOption("summ-confint", TRUE), exp = TRUE, pvals = FALSE)
res4 <- summ(model_u_2017, confint = getOption("summ-confint", TRUE), exp = TRUE, pvals = FALSE)

res1 <- tidy(res1)
res2 <- tidy(res2)
res3 <- tidy(res3)
res4 <- tidy(res4)

res1 <- res1[-c(1), ]
res2 <- res2[-c(1), ]
res3 <- res3[-c(1), ]
res4 <- res4[-c(1), ]

res1$year<-2007
res2$year<-2017
res3$year<-2007
res4$year<-2017

res_r <- rbind(res1, res2)
res_u <- rbind(res3, res4)

res_r <- subset(res_r, select = -c(std.error, statistic))
colnames(res_r)[colnames(res_r) == "estimate"] <- "odds_ratio"

res_r$year<-as.factor(res_r$year)
res_r <- res_r %>% mutate(
  term =
    case_match(
      term,
      "factor(sex)2_f" ~ "Gender Girls",
      "factor(age_grp)2_age" ~ "Age 11-13 years",
      "factor(age_grp)3_age" ~ "Age 14-15 years",
      "factor(age_grp)4_age" ~ "Age 16-17 years",
      "factor(dist_inst)2_d" ~ "Distance to school 1-2 km",
      "factor(dist_inst)3_d" ~ "Distance to school 2-3 km",
      "factor(dist_inst)4_d" ~ "Distance to school 3-5 km",
      "factor(dist_inst)5_d" ~ "Distance to school 5+ km",
      "factor(caste)2_obc" ~ "Caste OBC",
      "factor(caste)3_sc" ~ "Caste SC",
      "factor(caste)4_st" ~ "Caste ST",
      "factor(type_of_inst)2_govt" ~ "Government school",
      "factor(hepc_q)Q2" ~ "Household expenditure per person Q2",
      "factor(hepc_q)Q3" ~ "Household expenditure per person Q3",
      "factor(hepc_q)Q4" ~ "Household expenditure per person Q4",
      "factor(hepc_q)Q5" ~ "Household expenditure per person Q5"
    )
)

res_u <- subset(res_u, select = -c(std.error, statistic, p.value))
colnames(res_u)[colnames(res_u) == "estimate"] <- "odds_ratio"

res_u$year<-as.factor(res_u$year)
res_u <- res_u %>% mutate(
  term =
    case_match(
      term,
      "factor(sex)2_f" ~ "Gender Girls",
      "factor(age_grp)2_age" ~ "Age 11-13 years",
      "factor(age_grp)3_age" ~ "Age 14-15 years",
      "factor(age_grp)4_age" ~ "Age 16-17 years",
      "factor(dist_inst)2_d" ~ "Distance to school 1-2 km",
      "factor(dist_inst)3_d" ~ "Distance to school 2-3 km",
      "factor(dist_inst)4_d" ~ "Distance to school 3-5 km",
      "factor(dist_inst)5_d" ~ "Distance to school 5+ km",
      "factor(caste)2_obc" ~ "Caste OBC",
      "factor(caste)3_sc" ~ "Caste SC",
      "factor(caste)4_st" ~ "Caste ST",
      "factor(type_of_inst)2_govt" ~ "Government school",
      "factor(hepc_q)Q2" ~ "Household expenditure per person Q2",
      "factor(hepc_q)Q3" ~ "Household expenditure per person Q3",
      "factor(hepc_q)Q4" ~ "Household expenditure per person Q4",
      "factor(hepc_q)Q5" ~ "Household expenditure per person Q5"
    )
)


#a<-export_summs(res1,res2, error_style = c("ci"),error_pos = c("right"), model.names = c("Model 1","Model 2"))

##rural
dodger = position_dodge(width = 0.3)

ggplot(res_r, aes(y = odds_ratio, x = term, colour = year)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position=dodger,size = 0.7, linewidth=0.9) +
  geom_hline(yintercept = 1.0, linetype = "dashed", size = 1) +
  scale_y_log10(breaks = c(0.5, 1,2, 5, 10, 25, 60)) +
  labs(y = "Odds ratio", x = "") +
  coord_flip(ylim = c(0.3, 63)) +
  # annotate(geom = 'text', y =7, x = 18, 
  #          label ='Reference category:\nDistance: <1km\nAge: 5-10 years\nSex: Female\nSocial group: General\nCourse expenditure: <770 Rs.\nHH expenditure per month: <4650 Rs.', 
  #          size = 4.5, hjust = 0)+
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.border = element_rect(),
                     panel.background = element_blank(),
                     plot.title = element_text(size=32, colour = "black"),
                     axis.text.x = element_text(size=24, colour = "black"),
                     axis.text.y = element_text(size=24, colour = "black"), 
                     axis.title.x = element_text(size=28, colour = "black"),
                     axis.title.y = element_text(size=28, colour = "black"),
                     legend.title = element_blank(),
                     legend.text = element_text(size=30, colour = "black"))

ggsave("reg_r.png", height = 9, width = 15)

##urban
dodger = position_dodge(width = 0.3)

ggplot(res_u, aes(y = odds_ratio, x = term, colour = year)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), position=dodger,size = 0.7, linewidth=0.9) +
  geom_hline(yintercept = 1.0, linetype = "dotted", size = 1) +
  scale_y_log10(breaks = c(0.5, 1,2, 5, 10, 25, 60)) +
  labs(y = "Odds ratio", x = "") +
  coord_flip(ylim = c(0.3, 63)) +
  # annotate(geom = 'text', y =7, x = 18, 
  #          label ='Reference category:\nDistance: <1km\nAge: 5-10 years\nSex: Female\nSocial group: General\nCourse expenditure: <770 Rs.\nHH expenditure per month: <4650 Rs.', 
  #          size = 4.5, hjust = 0)+
  theme_bw() + theme(panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.border = element_rect(),
                     panel.background = element_blank(),
                     plot.title = element_text(size=32, colour = "black"),
                     axis.text.x = element_text(size=24, colour = "black"),
                     axis.text.y = element_text(size=24, colour = "black"), 
                     axis.title.x = element_text(size=28, colour = "black"),
                     axis.title.y = element_text(size=28, colour = "black"),
                     legend.title = element_blank(),
                     legend.text = element_text(size=26, colour = "black"))

ggsave("reg_u.png", height = 9, width = 15)


#### 18. Tables: cycling levels by income across years stratified by gender and rural/urban ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

edu_temp$sex <- as.factor(edu_temp$sex)

##survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

cycle_inc <- svyby(~ cycle, by = ~ hepc_q+sex+year+sector, svyd, svymean, vartype = c("se","ci"))
colnames(cycle_inc)[colnames(cycle_inc) == "cycle"] <- "mean"
cycle_inc$mean<-round(cycle_inc$mean*100, 1)
cycle_inc$ci_l<-round(cycle_inc$ci_l*100, 1)
cycle_inc$ci_u<-round(cycle_inc$ci_u*100, 1)
rownames(cycle_inc)<-NULL
cycle_inc$se<-NULL
cycle_inc$mean <- paste0(cycle_inc$mean, " ","(", cycle_inc$ci_l,",",cycle_inc$ci_u,")")
cycle_inc$ci_l<-NULL
cycle_inc$ci_u<-NULL

##Rural
cycle_inc_r <- cycle_inc %>% filter(sector=="rural")

cycle_inc_rm <- cycle_inc_r %>% filter(sex=="Male") %>% select(-c("sex","sector")) %>% spread(year, mean)
cycle_inc_rf <- cycle_inc_r %>% filter(sex=="Female") %>% select(-c("sex","sector")) %>% spread(year, mean)

cycle_inc_r <- cycle_inc_rf %>% left_join(cycle_inc_rm, by="hepc_q")

write.csv(cycle_inc_r, "cycle_inc_r.csv")

##Urban
cycle_inc_u <- cycle_inc %>% filter(sector=="urban")

cycle_inc_um <- cycle_inc_u %>% filter(sex=="Male") %>% select(-c("sex","sector")) %>% spread(year, mean)
cycle_inc_uf <- cycle_inc_u %>% filter(sex=="Female") %>% select(-c("sex","sector")) %>% spread(year, mean)

cycle_inc_u <- cycle_inc_uf %>% left_join(cycle_inc_um, by="hepc_q")

write.csv(cycle_inc_u, "cycle_inc_u.csv")


#### 19. Bar plots: cycle mode share by income quintile ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$mode_of_transport),]
edu_temp$cycle<-0
edu_temp$cycle[which(edu_temp$mode_of_transport==4)]<-1
edu_temp <- edu_temp[!is.na(edu_temp$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Boys"
edu_temp$sex[edu_temp$sex==2]<-"Girls"

edu_temp$sex <- as.factor(edu_temp$sex)

##survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

cycle_inc <- svyby(~ cycle, by = ~ hepc_q+sex+year+sector, svyd, svymean, vartype = c("se","ci"))
colnames(cycle_inc)[colnames(cycle_inc) == "cycle"] <- "mean"
cycle_inc$mean<-round(cycle_inc$mean*100, 1)
cycle_inc$ci_l<-round(cycle_inc$ci_l*100, 1)
cycle_inc$ci_u<-round(cycle_inc$ci_u*100, 1)
rownames(cycle_inc)<-NULL
cycle_inc$se<-NULL

cycle_inc_r_2007 <- cycle_inc %>% filter(year==2007) %>% filter(sector=="rural")
cycle_inc_r_2017 <- cycle_inc %>% filter(year==2017) %>% filter(sector=="rural")
cycle_inc_u_2007 <- cycle_inc %>% filter(year==2007) %>% filter(sector=="urban")
cycle_inc_u_2017 <- cycle_inc %>% filter(year==2017) %>% filter(sector=="urban")

##Rural: 2007
a <- ggplot(cycle_inc_r_2007, aes(x = hepc_q,y = mean, fill = sex)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_manual(values=c("#6666FF","#66FF99"))+
  #geom_text(aes(label = mean), size = 6, position = position_dodge(width = .9),hjust=1.0, vjust = -0.2, colour = "black")+
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u,group = factor(sex, level = c('Boys','Girls'))), 
                width=0.3, size=0.8,position=position_dodge(0.9,preserve='single'),color="black") +
  ggtitle("a) Rural: 2007") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name="Mode share of cycling (%)",expand = c(0,0), limits=c(0, 16.2), 
                     breaks = seq(0,16,by=2))+
  xlab("Monthly per capita expenditure (quintile)")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=32, colour = "black"),
        legend.title=element_blank(),
        legend.position = "none",
        legend.text = element_text(size=32, colour = "black"))

##Rural: 2017
b <- ggplot(cycle_inc_r_2017, aes(x = hepc_q,y = mean, fill = sex)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_manual(values=c("#6666FF","#66FF99"))+
  #geom_text(aes(label = mean), size = 6, position = position_dodge(width = .9),hjust=1.0, vjust = -0.2, colour = "black")+
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u,group = factor(sex, level = c('Boys','Girls'))), 
                width=0.3, size=0.8,position=position_dodge(0.9,preserve='single'),color="black") +
  ggtitle("b) Rural: 2017") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name="Mode share of cycling (%)",expand = c(0,0), limits=c(0, 16.2), 
                     breaks = seq(0,16,by=2))+
  xlab("Monthly per capita expenditure (quintile)")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=32, colour = "black"),
        legend.title=element_blank(),
        legend.position = "none",
        legend.text = element_text(size=32, colour = "black"))

##Urban: 2007
c <- ggplot(cycle_inc_u_2007, aes(x = hepc_q,y = mean, fill = sex)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_manual(values=c("#6666FF","#66FF99"))+
  #geom_text(aes(label = mean), size = 6, position = position_dodge(width = .9),hjust=1.0, vjust = -0.2, colour = "black")+
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u,group = factor(sex, level = c('Boys','Girls'))), 
                width=0.3, size=0.8,position=position_dodge(0.9,preserve='single'),color="black") +
  ggtitle("c) Urban: 2007") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name="Mode share of cycling (%)",expand = c(0,0), limits=c(0, 16.2), 
                     breaks = seq(0,16,by=2))+
  xlab("Monthly per capita expenditure (quintile)")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=32, colour = "black"),
        legend.title=element_blank(),
        legend.position = "none",
        legend.text = element_text(size=32, colour = "black"))

##Urban: 2017
d <- ggplot(cycle_inc_u_2017, aes(x = hepc_q,y = mean, fill = sex)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_manual(values=c("#6666FF","#66FF99"))+
  #geom_text(aes(label = mean), size = 6, position = position_dodge(width = .9),hjust=1.0, vjust = -0.2, colour = "black")+
  geom_errorbar(aes(ymin=ci_l, ymax=ci_u,group = factor(sex, level = c('Boys','Girls'))), 
                width=0.3, size=0.8,position=position_dodge(0.9,preserve='single'),color="black") +
  ggtitle("d) Urban: 2017") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name="Mode share of cycling (%)",expand = c(0,0), limits=c(0, 16.2), 
                     breaks = seq(0,16,by=2))+
  xlab("Monthly per capita expenditure (quintile)")+
  theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=24, colour = "black"),
        axis.text.y = element_text(size=24, colour = "black"), 
        axis.title.y = element_text(size=26, colour = "black"),
        axis.title.x = element_text(size=26, colour = "black"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=32, colour = "black"),
        legend.title=element_blank(),
        legend.position = "none",
        legend.text = element_text(size=32, colour = "black"))

ggarrange(a,b,c,d,nrow=2,ncol=2,common.legend = TRUE, legend = "bottom")
ggsave("cycle_inc_bar_plot.png", width=15, height = 12)


#### 20. Percentage distribution of distance to school by grade ####

##data
edu_temp <- school_5to17yrs[!is.na(school_5to17yrs$sex),]
edu_temp <- edu_temp %>% filter(!sex==3)
edu_temp$sex[edu_temp$sex==1]<-"Male"
edu_temp$sex[edu_temp$sex==2]<-"Female"

edu_temp$dist_inst[which(edu_temp$dist_inst==1)]<-"d<1 km"
edu_temp$dist_inst[which(edu_temp$dist_inst==2)]<-"1 km<=d<2 km"
edu_temp$dist_inst[which(edu_temp$dist_inst==3)]<-"2 km<=d<3 km"
edu_temp$dist_inst[which(edu_temp$dist_inst==4)]<-"3 km<=d<5 km"
edu_temp$dist_inst[which(edu_temp$dist_inst==5)]<-"d>=5 km"

edu_temp$dist_inst <- factor(edu_temp$dist_inst, 
                             levels = c("d<1 km","1 km<=d<2 km","2 km<=d<3 km","3 km<=d<5 km","d>=5 km"), 
                             ordered = TRUE)

edu_temp$edu_level_curr_attend <- factor(edu_temp$edu_level_curr_attend, 
                                         levels = c("primary","upper primary/middle","secondary","higher secondary"), 
                                         ordered = TRUE)

edu_temp <- edu_temp[!is.na(edu_temp$edu_level_curr_attend),]
edu_temp <- edu_temp[!is.na(edu_temp$dist_inst),]

##survey design
svyd <- svydesign(id=~1, weights=~hh_wgt, strata=~district_name+sector, data=edu_temp)

dist_grade <- svyby(~ dist_inst, by = ~ edu_level_curr_attend+year+sector, svyd, svymean)
dist_grade <- dist_grade[,1:8]
rownames(dist_grade)<-NULL
dist_grade$`dist_instd<1 km`<-round(dist_grade$`dist_instd<1 km`*100,1)
dist_grade$`dist_inst1 km<=d<2 km`<-round(dist_grade$`dist_inst1 km<=d<2 km`*100,1)
dist_grade$`dist_inst2 km<=d<3 km`<-round(dist_grade$`dist_inst2 km<=d<3 km`*100,1)
dist_grade$`dist_inst3 km<=d<5 km`<-round(dist_grade$`dist_inst3 km<=d<5 km`*100,1)
dist_grade$`dist_instd>=5 km`<-round(dist_grade$`dist_instd>=5 km`*100,1)

dist_grade <- dist_grade[!dist_grade$year==2014,]
write.csv(dist_grade,"dist_grade.csv")
