########################################################
# Title: 5 CSDS health visiting research ready datasets
# Created by: Amanda Clery
# Date: January 2024

# This script produces research ready datasets for each
# postnatal mandated health visiting contact to include
# only those children and contacts that occur in LAs
# and quarters with complete CSDS data compared to the
# publicly available health visiting metrics

# Corresponds to thesis sections 4.2.5 and 4.2.6

########################################################

#---- 0 Install packages ----------------------------------------

library(dplyr)
library(lubridate)
library(ggplot2)
library(sf)
library(tmap)

#---- 1 Open data -----------------------------------------------

setwd("file path to the data X:/ ...")

# open the prepared demographics dataset
load(file = "1_unique_u5s.Rdata")

# open the prepared contacts dataset
load(file = "2_unique contacts.Rdata")

# open the list of LA quarters with complete numerator data
load(file = "3_complete_las_numerator.Rdata")

# open the list of LA quarters with complete denominator data
load(file = "4_complete_las_denominator.Rdata")

# open publicly available 2021 LA boundary maps
map <- st_read("XXXX.shp")
map <- map[grepl("E",map$CTYUA21CD),c("CTYUA21CD","CTYUA21NM")]


#---- 2 Compare numerator and denominator data ------------------

# both numerator and denominator data needed to be complete for 
# creating coverage estimates for analysis.
# therefore needed to merge and check if any LA-quarters had 
# complete data for one but not the other

assess_tot <- full_join(assess,assess_denom, by=c("la","yrq"))

# exclude LA-quarters where numerator is complete but denominator is not
assess_tot$nbv_complete[assess_tot$nbv_complete==1 & assess_tot$nbv_complete_denom==0] <- NA
assess_tot$sixw_complete[assess_tot$sixw_complete==1 & assess_tot$sixw_complete_denom==0] <- NA
assess_tot$oney_complete[assess_tot$oney_complete==1 & assess_tot$oney_complete_denom==0] <- NA
assess_tot$twoy_complete[assess_tot$twoy_complete==1 & assess_tot$twoy_complete_denom==0] <- NA


#---- 3 Prepare data for mapping the final complete LAs  ---------

# ensure LAs match between map data and 2018-20 CSDS data
map$la <- map$CTYUA21CD
map$la <- ifelse(map$CTYUA21CD=="E06000061","E10000021",
                   ifelse(map$CTYUA21CD=="E06000062","E10000021",
                          ifelse(map$CTYUA21CD=="E06000053","E06000052",
                                 ifelse(map$CTYUA21CD=="E09000001","E09000012",map$la))))

map$la_name <- map$CTYUA21NM
map$la_name <- ifelse(map$CTYUA21CD=="E06000061","Northamptonshire",
                        ifelse(map$CTYUA21CD=="E06000062","Northamptonshire",
                               ifelse(map$CTYUA21CD=="E06000053","Cornwall and Isles of Scilly",
                                      ifelse(map$CTYUA21CD=="E06000052","Cornwall and Isles of Scilly",
                                             ifelse(map$CTYUA21CD=="E09000001","Hackney and City of London",
                                                    ifelse(map$CTYUA21CD=="E09000012","Hackney and City of London",map$la_name))))))

map <- map %>%
  group_by(la, la_name) %>%
  summarise(geometry=st_union(geometry)) %>%
  ungroup()


# count the number of quarters of complete data for each LA and contact
rrlist_nbv <- assess_tot %>%
  group_by(la) %>%
  mutate(nbv_yrq_count=sum(nbv_complete)) %>% 
  distinct(la,nbv_yrq_count)

rrlist_6w <- assess_tot %>%
  group_by(la) %>%
  mutate(sixw_yrq_count=sum(sixw_complete)) %>% 
  distinct(la,sixw_yrq_count)

rrlist_1y <- assess_tot %>%
  group_by(la) %>%
  mutate(oney_yrq_count=sum(oney_complete)) %>% 
  distinct(la,oney_yrq_count)

rrlist_2y <- assess_tot %>%
  group_by(la) %>%
  mutate(twoy_yrq_count=sum(twoy_complete)) %>% 
  distinct(la,twoy_yrq_count)

# join datasets together
las <- left_join(rrlist_nbv,rrlist_6w,by="la")
las <- left_join(las,rrlist_1y,by="la")
las <- left_join(las,rrlist_2y,by="la")

las <- las[order(las$la),]

# join with map data
map <- left_join(map,las,by="la")

# update missings to zero for plotting
map$nbv_yrq_count[is.na(map$nbv_yrq_count)]<- 0
map$sixw_yrq_count[is.na(map$sixw_yrq_count)]<- 0
map$oney_yrq_count[is.na(map$oney_yrq_count)]<- 0
map$twoy_yrq_count[is.na(map$twoy_yrq_count)]<- 0


#---- 4 Map final complete LAs  ---------------------------------

# create a map, shading each LA based on the number of quarters
# of data each contributes to the final set of complete LAs
# to be included in the research ready datasets

# new birth visit
tmap::tmap_save(tm_shape(map) +
                  tm_fill(col="nbv_yrq_count",
                          palette="Purples",
                          breaks = c(0,1,2,3,4,5,6,7,8,+Inf),
                          labels = c("None","1","2","3","4","5","6","7","8")) +
                  tm_layout(main.title = "Number of quarters LAs contribute to NBV data",
                            main.title.size = 1.2,
                            legend.title.color = "white",
                            frame = FALSE) +
                  tm_borders(),
                filename="5_nbv_completeness_map.png")

# 6-8-week review
tmap_save(tm_shape(map) +
            tm_fill(col="sixw_yrq_count",
                    palette="Oranges",
                    breaks = c(0,1,2,3,4,5,6,7,8,+Inf),
                    labels = c("None","1","2","3","4","5","6","7","8")) +
            tm_layout(main.title = "Number of quarters LAs contribute to 6-8w review data",
                      main.title.size = 1.2,
                      legend.title.color = "white",
                      frame = FALSE) +
            tm_borders(),
          filename = "5_6w_completeness_map.png")

# 1- year review
tmap_save(tm_shape(map) +
            tm_fill(col="oney_yrq_count",
                    palette="Blues",
                    breaks = c(0,1,2,3,4,5,6,7,8,+Inf),
                    labels = c("None","1","2","3","4","5","6","7","8")) +
            tm_layout(main.title = "Number of quarters LAs contribute to 1y review data",
                      main.title.size = 1.2,
                      legend.title.color = "white",
                      frame = FALSE) +
            tm_borders(),
          filename = "5_1y_completeness_map.png")

# 2-2.5-year review
tmap_save(tm_shape(map) +
            tm_fill(col="twoy_yrq_count",
                    palette="Greens",
                    breaks = c(0,1,2,3,4,5,6,7,8,+Inf),
                    labels = c("None","1","2","3","4","5","6","7","8")) +
            tm_layout(main.title = "Number of quarters LAs contribute to 2-2.5y data",
                      main.title.size = 1.2,
                      legend.title.color = "white",
                      frame = FALSE) +
            tm_borders(),
          filename = "5_2y_completeness_map.png")


#---- 5 Create research-ready datasets  -------------------------

# prepare data to merge with list of complete LA-quarters

contacts_u5_all_unique$yrq <- lubridate::quarter(contacts_u5_all_unique$Contact_Date, with_year=TRUE, fiscal_start=4)
contacts_u5_all_unique$yrq <- paste0(as.integer(contacts_u5_all_unique$yrq)-2001,as.integer(contacts_u5_all_unique$yrq)-2000,"q",round((contacts_u5_all_unique$yrq%%1)*10))

contacts_u5_all_unique <- left_join(contacts_u5_all_unique,assess_tot, by=c("la","yrq"))

# new birth visit
rr_nbv <- contacts_u5_all_unique[contacts_u5_all_unique$mandated_nbv==1 & contacts_u5_all_unique$nbv_complete==1 & !is.na(contacts_u5_all_unique$nbv_complete),]

# 6-8-week review
rr_6w <- contacts_u5_all_unique[contacts_u5_all_unique$mandated_6w==1 & contacts_u5_all_unique$sixw_complete==1 & !is.na(contacts_u5_all_unique$sixw_complete),]

# 1-year review
rr_1y <- contacts_u5_all_unique[contacts_u5_all_unique$mandated_1y==1 & contacts_u5_all_unique$oney_complete==1 & !is.na(contacts_u5_all_unique$oney_complete),]

# 2-2.5-year review
rr_2y <- contacts_u5_all_unique[contacts_u5_all_unique$mandated_2y==1 & contacts_u5_all_unique$twoy_complete==1 & !is.na(contacts_u5_all_unique$twoy_complete),]


#---- 6 Include children eligible but not received  --------------

# in step 5, the datasets only include those children who received
# the mandated contacts. for analysis, we also wanted to include
# children who should have received the contact but did not

# determine the expected quarter of the contact

# new birth visit
demo_u5_all$MonthYear_Turning30Days_dt <- as.Date(paste0(demo_u5_all$MonthYear_Turning30Days,"-15"),"%Y-%m-%d")
demo_u5_all$yrq_exp_nbv <- quarter(demo_u5_all$MonthYear_Turning30Days_dt, with_year=TRUE, fiscal_start=4)
demo_u5_all$yrq_exp_nbv <- paste0(as.integer(demo_u5_all$yrq_exp_nbv)-2001,as.integer(demo_u5_all$yrq_exp_nbv)-2000,"q",round((demo_u5_all$yrq_exp_nbv%%1)*10))

# 6-8-week review
demo_u5_all$MonthYear_Turning56Days_dt <- as.Date(paste0(demo_u5_all$MonthYear_Turning56Days,"-15"),"%Y-%m-%d")
demo_u5_all$yrq_exp_6w <- quarter(demo_u5_all$MonthYear_Turning56Days_dt, with_year=TRUE, fiscal_start=4)
demo_u5_all$yrq_exp_6w <- paste0(as.integer(demo_u5_all$yrq_exp_6w)-2001,as.integer(demo_u5_all$yrq_exp_6w)-2000,"q",round((demo_u5_all$yrq_exp_6w%%1)*10))

# 1-year review
demo_u5_all$MonthYear_Turning457Days_dt <- as.Date(paste0(demo_u5_all$MonthYear_Turning457Days,"-15"),"%Y-%m-%d")
demo_u5_all$yrq_exp_1y <- quarter(demo_u5_all$MonthYear_Turning457Days_dt, with_year=TRUE, fiscal_start=4)
demo_u5_all$yrq_exp_1y <- paste0(as.integer(demo_u5_all$yrq_exp_1y)-2001,as.integer(demo_u5_all$yrq_exp_1y)-2000,"q",round((demo_u5_all$yrq_exp_1y%%1)*10))

# 2-2.5-year review
demo_u5_all$MonthYear_Turning914Days_dt <- as.Date(paste0(demo_u5_all$MonthYear_Turning914Days,"-15"),"%Y-%m-%d")
demo_u5_all$yrq_exp_2y <- quarter(demo_u5_all$MonthYear_Turning914Days_dt, with_year=TRUE, fiscal_start=4)
demo_u5_all$yrq_exp_2y <- paste0(as.integer(demo_u5_all$yrq_exp_2y)-2001,as.integer(demo_u5_all$yrq_exp_2y)-2000,"q",round((demo_u5_all$yrq_exp_2y%%1)*10))

yrqs <- c("yrq_exp_nbv","yrq_exp_6w","yrq_exp_1y","yrq_exp_2y")

for (i in yrqs) {
  
  demo_u5_all[,i] <- as.numeric(demo_u5_all[,i])
}

# determine the expected LA

# prepare variables into date format
demo_u5_all$ReferralRequest_ReceivedDate_1 <- as.Date(demo_u5_all$ReferralRequest_ReceivedDate_1, format="%Y-%m-%d")
demo_u5_all$ReferralRequest_ReceivedDate_2 <- as.Date(demo_u5_all$ReferralRequest_ReceivedDate_2, format="%Y-%m-%d")
demo_u5_all$ReferralRequest_ReceivedDate_3 <- as.Date(demo_u5_all$ReferralRequest_ReceivedDate_3, format="%Y-%m-%d")
demo_u5_all$ReferralRequest_ReceivedDate_4 <- as.Date(demo_u5_all$ReferralRequest_ReceivedDate_4, format="%Y-%m-%d")
demo_u5_all$ReferralRequest_ReceivedDate_5 <- as.Date(demo_u5_all$ReferralRequest_ReceivedDate_5, format="%Y-%m-%d")
demo_u5_all$ReferralRequest_ReceivedDate_6 <- as.Date(demo_u5_all$ReferralRequest_ReceivedDate_6, format="%Y-%m-%d")

# new birth visit
demo_u5_all$la_exp_nbv <- ifelse(is.na(demo_u5_all$UTLA19CD_2),demo_u5_all$UTLA19CD_1,
                                 ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$UTLA19CD_3) & !is.na(demo_u5_all$ReferralRequest_ReceivedDate_2) & demo_u5_all$MonthYear_Turning30Days_dt<demo_u5_all$ReferralRequest_ReceivedDate_2,demo_u5_all$UTLA19CD_1,
                                        ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$ReferralRequest_ReceivedDate_2), demo_u5_all$UTLA19CD_1,
                                               ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning30Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_2, demo_u5_all$UTLA19CD_2,
                                                      ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning30Days_dt<demo_u5_all$ReferralRequest_ReceivedDate_2, demo_u5_all$UTLA19CD_1,
                                                             ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning30Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_2 & !is.na(demo_u5_all$ReferralRequest_ReceivedDate_3)& demo_u5_all$MonthYear_Turning30Days_dt<demo_u5_all$ReferralRequest_ReceivedDate_3,demo_u5_all$UTLA19CD_2,
                                                                    ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning30Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_2 & is.na(demo_u5_all$ReferralRequest_ReceivedDate_3),demo_u5_all$UTLA19CD_2,
                                                                           ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning30Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_3, demo_u5_all$UTLA19CD_3, NA))))))))

# 6-8-week review
demo_u5_all$la_exp_6w <- ifelse(is.na(demo_u5_all$UTLA19CD_2),demo_u5_all$UTLA19CD_1,
                                ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$UTLA19CD_3) & !is.na(demo_u5_all$ReferralRequest_ReceivedDate_2) & demo_u5_all$MonthYear_Turning56Days_dt<demo_u5_all$ReferralRequest_ReceivedDate_2,demo_u5_all$UTLA19CD_1,
                                       ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$ReferralRequest_ReceivedDate_2), demo_u5_all$UTLA19CD_1,
                                              ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning56Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_2, demo_u5_all$UTLA19CD_2,
                                                     ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning56Days_dt<demo_u5_all$ReferralRequest_ReceivedDate_2, demo_u5_all$UTLA19CD_1,
                                                            ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning56Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_2 & !is.na(demo_u5_all$ReferralRequest_ReceivedDate_3)& demo_u5_all$MonthYear_Turning56Days_dt<demo_u5_all$ReferralRequest_ReceivedDate_3,demo_u5_all$UTLA19CD_2,
                                                                   ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning56Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_2 & is.na(demo_u5_all$ReferralRequest_ReceivedDate_3),demo_u5_all$UTLA19CD_2,
                                                                          ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning56Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_3, demo_u5_all$UTLA19CD_3, NA))))))))

# 1-year review
demo_u5_all$la_exp_1y <- ifelse(is.na(demo_u5_all$UTLA19CD_2),demo_u5_all$UTLA19CD_1,
                                ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$UTLA19CD_3) & !is.na(demo_u5_all$ReferralRequest_ReceivedDate_2) & demo_u5_all$MonthYear_Turning457Days<demo_u5_all$ReferralRequest_ReceivedDate_2,demo_u5_all$UTLA19CD_1,
                                       ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$ReferralRequest_ReceivedDate_2), demo_u5_all$UTLA19CD_1,
                                              ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning457Days>=demo_u5_all$ReferralRequest_ReceivedDate_2, demo_u5_all$UTLA19CD_2,
                                                     ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning457Days<demo_u5_all$ReferralRequest_ReceivedDate_2, demo_u5_all$UTLA19CD_1,
                                                            ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning457Days>=demo_u5_all$ReferralRequest_ReceivedDate_2 & !is.na(demo_u5_all$ReferralRequest_ReceivedDate_3)& demo_u5_all$MonthYear_Turning457Days<demo_u5_all$ReferralRequest_ReceivedDate_3,demo_u5_all$UTLA19CD_2,
                                                                   ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning457Days>=demo_u5_all$ReferralRequest_ReceivedDate_2 & is.na(demo_u5_all$ReferralRequest_ReceivedDate_3),demo_u5_all$UTLA19CD_2,
                                                                          ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning457Days>=demo_u5_all$ReferralRequest_ReceivedDate_3, demo_u5_all$UTLA19CD_3, NA))))))))

# 2-2.5-year review
demo_u5_all$la_exp_2y <- ifelse(is.na(demo_u5_all$UTLA19CD_2),demo_u5_all$UTLA19CD_1,
                                ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$UTLA19CD_3) & !is.na(demo_u5_all$ReferralRequest_ReceivedDate_2) & demo_u5_all$MonthYear_Turning914Days_dt<demo_u5_all$ReferralRequest_ReceivedDate_2,demo_u5_all$UTLA19CD_1,
                                       ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$ReferralRequest_ReceivedDate_2), demo_u5_all$UTLA19CD_1,
                                              ifelse(!is.na(demo_u5_all$UTLA19CD_2) & is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning914Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_2, demo_u5_all$UTLA19CD_2,
                                                     ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning914Days_dt<demo_u5_all$ReferralRequest_ReceivedDate_2, demo_u5_all$UTLA19CD_1,
                                                            ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning914Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_2 & !is.na(demo_u5_all$ReferralRequest_ReceivedDate_3)& demo_u5_all$MonthYear_Turning914Days_dt<demo_u5_all$ReferralRequest_ReceivedDate_3,demo_u5_all$UTLA19CD_2,
                                                                   ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning914Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_2 & is.na(demo_u5_all$ReferralRequest_ReceivedDate_3),demo_u5_all$UTLA19CD_2,
                                                                          ifelse(!is.na(demo_u5_all$UTLA19CD_3) & demo_u5_all$MonthYear_Turning914Days_dt>=demo_u5_all$ReferralRequest_ReceivedDate_3, demo_u5_all$UTLA19CD_3, NA))))))))

# combine Isles of Scilly with Cornwall, and City of London with Hackney
# as these deliver the respective health visiting services
las <- c("la_exp_nbv","la_exp_6w","la_exp_1y","la_exp_2y")

for (i in las) {
  
  demo_u5_all[,i][demo_u5_all[,i]=="E06000053"] <- "E06000052"
  demo_u5_all[,i][demo_u5_all[,i]=="E09000001"] <- "E09000012"
  
}


# use research ready lists to extract children

assess_tot_nbv <- assess_tot[assess_tot$nbv_complete==1 & !is.na(assess_tot$nbv_complete==1),c("la","yrq")]
assess_tot_6w <- assess_tot[assess_tot$sixw_complete==1 & !is.na(assess_tot$sixw_complete==1),c("la","yrq")]
assess_tot_1y <- assess_tot[assess_tot$oney_complete==1 & !is.na(assess_tot$oney_complete==1),c("la","yrq")]
assess_tot_2y <- assess_tot[assess_tot$twoy_complete==1 & !is.na(assess_tot$twoy_complete==1),c("la","yrq")]

rr_nbv_denom <- right_join(demo_u5_all,assess_tot_nbv, by=c("la_exp_nbv"="la","yrq_exp_nbv"="yrq"))
rr_6w_denom <- right_join(demo_u5_all,assess_tot_6w, by=c("la_exp_6w"="la","yrq_exp_6w"="yrq"))
rr_1y_denom <- right_join(demo_u5_all,assess_tot_1y, by=c("la_exp_1y"="la","yrq_exp_1y"="yrq"))
rr_2y_denom <- right_join(demo_u5_all,assess_tot_2y, by=c("la_exp_2y"="la","yrq_exp_2y"="yrq"))


#---- 7 Create full dataset including numerator and denominator  --------------

# combine the numerator and denominator data. These do not perfectly match 
# because some children are present in the numerator but not the denominator
# and vice versa. Therefore, cleaned so that:
# 1 children who received the contact in a complete LA-quarter but expected 
# their contact in an incomplete LA-quarter were added to the denominator
# 2 children who received their contact in an incomplete LA-quarter but 
# expected their contact in a complete LA-quarter were removed from the 
# denominator
#
# completed this separately for each postnatal mandated contact

## new birth visit
rr_nbv_tot <- full_join(rr_nbv_num,rr_nbv_denom,by="Token_Person_ID")

# identify those in cases 1 and 2 above
nbv_num_only <- rr_nbv_tot$Token_Person_ID[is.na(rr_nbv_tot$e_dob)]
nbv_denom_only <- rr_nbv_tot$Token_Person_ID[is.na(rr_nbv_tot$mandated_nbv)]

# extract denominator data for those missing 
rr_nbv_denom_num_only <- demo_u5_all[demo_u5_all$Token_Person_ID %in% nbv_num_only,]
rr_nbv_denom_num_only <- left_join(rr_nbv_denom_num_only,assess_tot, by=c("la_exp_nbv"="la","yrq_exp_nbv"="yrq"))
rr_nbv_denom_tot <- rbind(rr_nbv_denom,rr_nbv_denom_num_only)

# extract numerator data for those missing
rr_nbv_num_denom_only <- contacts_u5_all_unique[contacts_u5_all_unique$Token_Person_ID %in% nbv_denom_only,]
rr_nbv_num_denom_only <- rr_nbv_num_denom_only[rr_nbv_num_denom_only$mandated_nbv==1,]
rr_nbv_num_tot <- rbind(rr_nbv_num,rr_nbv_num_denom_only)

# combine 
rr_nbv_tot <- full_join(rr_nbv_num_tot,rr_nbv_denom_tot,by="Token_Person_ID") 

rr_nbv_tot$num_only <- ifelse(rr_nbv_tot$Token_Person_ID %in% nbv_num_only,1,0)
rr_nbv_tot$denom_only <- ifelse(rr_nbv_tot$Token_Person_ID %in% nbv_denom_only,1,0)

# create final cohort and clean vars
rr_nbv_tot <- rr_nbv_tot[!(rr_nbv_tot$denom_only==1 & !is.na(rr_nbv_tot$mandated_nbv)),]
rm(rr_nbv,rr_nbv_denom,rr_nbv_denom_num_only,rr_nbv_denom_tot,rr_nbv_num,rr_nbv_num_denom_only,rr_nbv_num_tot,nbv_denom_only,nbv_num_only)

# combine LA info: for those with a contact, use that info, for those in denom only, use la_exp_nbv
rr_nbv_tot$la_for_analysis <- ifelse(!is.na(rr_nbv_tot$la),rr_nbv_tot$la,
                                     ifelse(is.na(rr_nbv_tot$la),rr_nbv_tot$la_exp_nbv,NA))

# for those with no mandated contact (denominator only) updated mandated to 0
rr_nbv_tot$mandated_nbv[is.na(rr_nbv_tot$mandated_nbv)] <- 0



## 6-8-week review

rr_6w_tot <- full_join(rr_6w_num,rr_6w_denom,by="Token_Person_ID")

# identify cases
sixw_num_only <- rr_6w_tot$Token_Person_ID[is.na(rr_6w_tot$e_dob)]
sixw_denom_only <- rr_6w_tot$Token_Person_ID[is.na(rr_6w_tot$mandated_6w)]

# extract denominator data for those missing 
rr_6w_denom_num_only <- demo_u5_all[demo_u5_all$Token_Person_ID %in% sixw_num_only,]
rr_6w_denom_num_only <- left_join(rr_6w_denom_num_only,assess_tot, by=c("la_exp_6w"="la","yrq_exp_6w"="yrq"))
rr_6w_denom_tot <- rbind(rr_6w_denom,rr_6w_denom_num_only)

# extract numerator data for those missing
rr_6w_num_denom_only <- contacts_u5_all_unique[contacts_u5_all_unique$Token_Person_ID %in% sixw_denom_only,]
rr_6w_num_denom_only <- rr_6w_num_denom_only[rr_6w_num_denom_only$mandated_6w==1,]
rr_6w_num_tot <- rbind(rr_6w_num,rr_6w_num_denom_only)

# combine 
rr_6w_tot <- full_join(rr_6w_num_tot,rr_6w_denom_tot,by="Token_Person_ID") 
rr_6w_tot$num_only <- ifelse(rr_6w_tot$Token_Person_ID %in% sixw_num_only,1,0)
rr_6w_tot$denom_only <- ifelse(rr_6w_tot$Token_Person_ID %in% sixw_denom_only,1,0)

# create final cohort
rr_6w_tot <- rr_6w_tot[!(rr_6w_tot$denom_only==1 & !is.na(rr_6w_tot$mandated_6w)),]
rm(rr_6w,rr_6w_denom,rr_6w_denom_num_only,rr_6w_denom_tot,rr_6w_num,rr_6w_num_denom_only,rr_6w_num_tot,sixw_denom_only,sixw_num_only)

# combine LA info: for those with a contact, use that info, for those in denom only, use la_exp_6w
rr_6w_tot$la_for_analysis <- ifelse(!is.na(rr_6w_tot$la),rr_6w_tot$la,
                                    ifelse(is.na(rr_6w_tot$la),rr_6w_tot$la_exp_6w,NA))

# for those with no mandated contact (denominator only) updated mandated to 0
rr_6w_tot$mandated_6w[is.na(rr_6w_tot$mandated_6w)] <- 0



## 1-year review

rr_1y_tot <- full_join(rr_1y_num,rr_1y_denom,by="Token_Person_ID") 

# identify cases
oney_num_only <- rr_1y_tot$Token_Person_ID[is.na(rr_1y_tot$e_dob)]
oney_denom_only <- rr_1y_tot$Token_Person_ID[is.na(rr_1y_tot$mandated_1y)]

# extract denominator data for those missing 

rr_1y_denom_num_only <- demo_u5_all[demo_u5_all$Token_Person_ID %in% oney_num_only,]
rr_1y_denom_num_only <- left_join(rr_1y_denom_num_only,assess_tot, by=c("la_exp_1y"="la","yrq_exp_1y"="yrq"))
rr_1y_denom_tot <- rbind(rr_1y_denom,rr_1y_denom_num_only)

# extract numerator data for those missing
rr_1y_num_denom_only <- contacts_u5_all_unique[contacts_u5_all_unique$Token_Person_ID %in% oney_denom_only,]
rr_1y_num_denom_only <- rr_1y_num_denom_only[rr_1y_num_denom_only$mandated_1y==1,]
rr_1y_num_tot <- rbind(rr_1y_num,rr_1y_num_denom_only)

# combine 
rr_1y_tot <- full_join(rr_1y_num_tot,rr_1y_denom_tot,by="Token_Person_ID") 
rr_1y_tot$num_only <- ifelse(rr_1y_tot$Token_Person_ID %in% oney_num_only,1,0)
rr_1y_tot$denom_only <- ifelse(rr_1y_tot$Token_Person_ID %in% oney_denom_only,1,0)

# create final cohort
rr_1y_tot <- rr_1y_tot[!(rr_1y_tot$denom_only==1 & !is.na(rr_1y_tot$mandated_1y)),]
rm(rr_1y,rr_1y_denom,rr_1y_denom_num_only,rr_1y_denom_tot,rr_1y_num,rr_1y_num_denom_only,rr_1y_num_tot,oney_denom_only,oney_num_only)

# combine LA info: for those with a contact, use that info, for those in denom only, use la_exp_1y
rr_1y_tot$la_for_analysis <- ifelse(!is.na(rr_1y_tot$la),rr_1y_tot$la,
                                    ifelse(is.na(rr_1y_tot$la),rr_1y_tot$la_exp_1y,NA))

# for those with no mandated contact (denominator only) updated mandated to 0
rr_1y_tot$mandated_1y[is.na(rr_1y_tot$mandated_1y)] <- 0



## 2-year review
rr_2y_tot <- full_join(rr_2y_num,rr_2y_denom,by="Token_Person_ID")

# identify cases
twoy_num_only <- rr_2y_tot$Token_Person_ID[is.na(rr_2y_tot$e_dob)]
twoy_denom_only <- rr_2y_tot$Token_Person_ID[is.na(rr_2y_tot$mandated_2y)]

# extract denominator data for those missing 

rr_2y_denom_num_only <- demo_u5_all[demo_u5_all$Token_Person_ID %in% twoy_num_only,]
rr_2y_denom_num_only <- left_join(rr_2y_denom_num_only,assess_tot, by=c("la_exp_2y"="la","yrq_exp_2y"="yrq"))
rr_2y_denom_tot <- rbind(rr_2y_denom,rr_2y_denom_num_only)

# extract numerator data for those missing
rr_2y_num_denom_only <- contacts_u5_all_unique[contacts_u5_all_unique$Token_Person_ID %in% twoy_denom_only,]
rr_2y_num_denom_only <- rr_2y_num_denom_only[rr_2y_num_denom_only$mandated_2y==1,]
rr_2y_num_tot <- rbind(rr_2y_num,rr_2y_num_denom_only)

# combine 
rr_2y_tot <- full_join(rr_2y_num_tot,rr_2y_denom_tot,by="Token_Person_ID") 
rr_2y_tot$num_only <- ifelse(rr_2y_tot$Token_Person_ID %in% twoy_num_only,1,0)
rr_2y_tot$denom_only <- ifelse(rr_2y_tot$Token_Person_ID %in% twoy_denom_only,1,0)

# create final cohort
rr_2y_tot <- rr_2y_tot[!(rr_2y_tot$denom_only==1 & !is.na(rr_2y_tot$mandated_2y)),]
rm(rr_2y,rr_2y_denom,rr_2y_denom_num_only,rr_2y_denom_tot,rr_2y_num,rr_2y_num_denom_only,rr_2y_num_tot,twoy_denom_only,twoy_num_only)

# combine LA info: for those with a contact, use that info, for those in denom only, use la_exp_2y 
rr_2y_tot$la_for_analysis <- ifelse(!is.na(rr_2y_tot$la),rr_2y_tot$la,
                                    ifelse(is.na(rr_2y_tot$la),rr_2y_tot$la_exp_2y,NA))

# for those with no mandated contact (denominator only) updated mandated to 0
rr_2y_tot$mandated_2y[is.na(rr_2y_tot$mandated_2y)] <- 0



#---- 8 Save research-ready datasets  -------------------------

save(rr_nbv_tot,file = "5_research-ready-nbv.Rdata")
save(rr_6w_tot,file = "5_research-ready-6w.Rdata")
save(rr_1y_tot,file = "5_research-ready-1y.Rdata")
save(rr_2y_tot,file = "5_research-ready-2y.Rdata")

