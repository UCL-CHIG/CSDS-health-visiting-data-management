########################################################
# Title: 3 CSDS health visiting completeness assessment
# Created by: Amanda Clery
# Date: January 2024

# This script prepares the CSDS data to count number
# of postnatal mandated health visiting contacts by
# LA and quarter between April 2018 and March 2020
# for children under 5 living in England.
#
# Then compares these counts to counts available
# publicly published by the Office for Health Improvement
# and Disparities (OHID, previously Public Health England)
# to identify complete CSDS data by LA and quarter

# Corresponds to thesis section 4.2.4

########################################################

#---- 0 Install packages ----------------------------------------

library(dplyr)
library(lubridate)
library(ggplot2)


#---- 1 Open data -----------------------------------------------

setwd("file path to the data X:/ ...")

# open the prepared contacts dataset
load(file = "2_unique contacts.Rdata")

# open aggregate PH metrics
# these are publicly available and have been prepared to include only
# the counts of each mandated contact and the children eligible
phmetrics <- read.csv(file="XXXXX.csv")


#---- 2 Assign an LA to each contact  -------------------------------

# based on demographics data, need to determine which LA the contact 
# took place in.
# this is straightforward for children who never move LA
# but for those children who do move LA, the LA assigned to the contact
# should be based on the timing of movement between LA, based on 
# referral dates

# firstly convert relevant dates into date format
contacts_u5_all_unique$ReferralRequest_ReceivedDate_1 <- as.Date(contacts_u5_all_unique$ReferralRequest_ReceivedDate_1, format="%Y-%m-%d")
contacts_u5_all_unique$ReferralRequest_ReceivedDate_2 <- as.Date(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2, format="%Y-%m-%d")
contacts_u5_all_unique$ReferralRequest_ReceivedDate_3 <- as.Date(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3, format="%Y-%m-%d")
contacts_u5_all_unique$ReferralRequest_ReceivedDate_4 <- as.Date(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4, format="%Y-%m-%d")
contacts_u5_all_unique$ReferralRequest_ReceivedDate_5 <- as.Date(contacts_u5_all_unique$ReferralRequest_ReceivedDate_5, format="%Y-%m-%d")
contacts_u5_all_unique$ReferralRequest_ReceivedDate_6 <- as.Date(contacts_u5_all_unique$ReferralRequest_ReceivedDate_6, format="%Y-%m-%d")


# generate new LA variable
contacts_u5_all_unique$la <- NA

# assign contacts to an LA
contacts_u5_all_unique$la <- ifelse(is.na(contacts_u5_all_unique$UTLA19CD_2),contacts_u5_all_unique$UTLA19CD_1,
                                    ifelse(is.na(contacts_u5_all_unique$UTLA19CD_3) & !is.na(contacts_u5_all_unique$UTLA19CD_2) &
                                             !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_2,contacts_u5_all_unique$UTLA19CD_2,
                                           ifelse(is.na(contacts_u5_all_unique$UTLA19CD_3) & !is.na(contacts_u5_all_unique$UTLA19CD_2) &
                                                    !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) & contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_2,contacts_u5_all_unique$UTLA19CD_1,
                                                  ifelse(is.na(contacts_u5_all_unique$UTLA19CD_3) & !is.na(contacts_u5_all_unique$UTLA19CD_2) & 
                                                           is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2),contacts_u5_all_unique$UTLA19CD_2,
                                                         ifelse(is.na(contacts_u5_all_unique$UTLA19CD_4) & !is.na(contacts_u5_all_unique$UTLA19CD_3) &
                                                                  !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_3,contacts_u5_all_unique$UTLA19CD_3,
                                                                ifelse(is.na(contacts_u5_all_unique$UTLA19CD_4) & !is.na(contacts_u5_all_unique$UTLA19CD_3) &
                                                                         !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) &
                                                                         contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_3 & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_2,contacts_u5_all_unique$UTLA19CD_2,
                                                                       ifelse(is.na(contacts_u5_all_unique$UTLA19CD_4) & !is.na(contacts_u5_all_unique$UTLA19CD_3) &
                                                                                !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) &
                                                                                contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_2,contacts_u5_all_unique$UTLA19CD_1,
                                                                              ifelse(is.na(contacts_u5_all_unique$UTLA19CD_4) & !is.na(contacts_u5_all_unique$UTLA19CD_3) &
                                                                                       !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) & is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) &
                                                                                       contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_3,contacts_u5_all_unique$UTLA19CD_2,
                                                                                     ifelse(is.na(contacts_u5_all_unique$UTLA19CD_4) & !is.na(contacts_u5_all_unique$UTLA19CD_3) &
                                                                                              is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) & is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2),contacts_u5_all_unique$UTLA19CD_3,
                                                                                            ifelse(is.na(contacts_u5_all_unique$UTLA19CD_4) & !is.na(contacts_u5_all_unique$UTLA19CD_3) &
                                                                                                     is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) &
                                                                                                     contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_2,contacts_u5_all_unique$UTLA19CD_1,
                                                                                                   ifelse(is.na(contacts_u5_all_unique$UTLA19CD_4) & !is.na(contacts_u5_all_unique$UTLA19CD_3) &
                                                                                                            is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) &
                                                                                                            contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_2,contacts_u5_all_unique$UTLA19CD_3,       
                                                                                                          ifelse(is.na(contacts_u5_all_unique$UTLA19CD_5) & !is.na(contacts_u5_all_unique$UTLA19CD_4) &
                                                                                                                   !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4) & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_4,contacts_u5_all_unique$UTLA19CD_4,
                                                                                                                 ifelse(is.na(contacts_u5_all_unique$UTLA19CD_5) & !is.na(contacts_u5_all_unique$UTLA19CD_4) &
                                                                                                                          !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) &
                                                                                                                          contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_4 & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_3,contacts_u5_all_unique$UTLA19CD_3,
                                                                                                                        ifelse(is.na(contacts_u5_all_unique$UTLA19CD_5) & !is.na(contacts_u5_all_unique$UTLA19CD_4) &
                                                                                                                                 !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) &
                                                                                                                                 contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_3 & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_2,contacts_u5_all_unique$UTLA19CD_2,
                                                                                                                               ifelse(is.na(contacts_u5_all_unique$UTLA19CD_5) & !is.na(contacts_u5_all_unique$UTLA19CD_4) &
                                                                                                                                        !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) & contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_2,contacts_u5_all_unique$UTLA19CD_1,
                                                                                                                                      ifelse(is.na(contacts_u5_all_unique$UTLA19CD_5) & !is.na(contacts_u5_all_unique$UTLA19CD_4) &
                                                                                                                                               !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4) & is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) &
                                                                                                                                               contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_4, contacts_u5_all_unique$UTLA19CD_3,
                                                                                                                                             ifelse(is.na(contacts_u5_all_unique$UTLA19CD_5) & !is.na(contacts_u5_all_unique$UTLA19CD_4) &
                                                                                                                                                      !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) & is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) &
                                                                                                                                                      contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_3, contacts_u5_all_unique$UTLA19CD_2,
                                                                                                                                                    ifelse(is.na(contacts_u5_all_unique$UTLA19CD_5) & !is.na(contacts_u5_all_unique$UTLA19CD_4) &
                                                                                                                                                             is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4) & is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3),contacts_u5_all_unique$UTLA19CD_4,
                                                                                                                                                           ifelse(is.na(contacts_u5_all_unique$UTLA19CD_5) & !is.na(contacts_u5_all_unique$UTLA19CD_4) &
                                                                                                                                                                    is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) &
                                                                                                                                                                    contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_3, contacts_u5_all_unique$UTLA19CD_4,
                                                                                                                                                                  ifelse(is.na(contacts_u5_all_unique$UTLA19CD_6) & !is.na(contacts_u5_all_unique$UTLA19CD_5) &
                                                                                                                                                                           !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_5) &
                                                                                                                                                                           contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_5,contacts_u5_all_unique$UTLA19CD_5,
                                                                                                                                                                         ifelse(is.na(contacts_u5_all_unique$UTLA19CD_6) & !is.na(contacts_u5_all_unique$UTLA19CD_5)&
                                                                                                                                                                                  !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_5) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4) &
                                                                                                                                                                                  contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_5 & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_4, contacts_u5_all_unique$UTLA19CD_4,
                                                                                                                                                                                ifelse(is.na(contacts_u5_all_unique$UTLA19CD_6) & !is.na(contacts_u5_all_unique$UTLA19CD_5)&
                                                                                                                                                                                         !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) &
                                                                                                                                                                                         contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_4 & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_3, contacts_u5_all_unique$UTLA19CD_3,
                                                                                                                                                                                       ifelse(is.na(contacts_u5_all_unique$UTLA19CD_6) & !is.na(contacts_u5_all_unique$UTLA19CD_5)&
                                                                                                                                                                                                !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_5) & is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) &
                                                                                                                                                                                                contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_5 & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_3, contacts_u5_all_unique$UTLA19CD_4,
                                                                                                                                                                                              ifelse(is.na(contacts_u5_all_unique$UTLA19CD_6) & !is.na(contacts_u5_all_unique$UTLA19CD_5)&
                                                                                                                                                                                                       !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) &
                                                                                                                                                                                                       contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_3 & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_2,contacts_u5_all_unique$UTLA19CD_2,
                                                                                                                                                                                                     ifelse(is.na(contacts_u5_all_unique$UTLA19CD_6) & !is.na(contacts_u5_all_unique$UTLA19CD_5)&
                                                                                                                                                                                                              !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) & contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_2, contacts_u5_all_unique$UTLA19CD_1,
                                                                                                                                                                                                            ifelse(is.na(contacts_u5_all_unique$UTLA19CD_6) & !is.na(contacts_u5_all_unique$UTLA19CD_5) &
                                                                                                                                                                                                                     !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4) &is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) &
                                                                                                                                                                                                                     contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_4, contacts_u5_all_unique$UTLA19CD_3,
                                                                                                                                                                                                                   ifelse(is.na(contacts_u5_all_unique$UTLA19CD_6) & !is.na(contacts_u5_all_unique$UTLA19CD_5) &
                                                                                                                                                                                                                            !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3) & is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2) &
                                                                                                                                                                                                                            contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_3, contacts_u5_all_unique$UTLA19CD_2,
                                                                                                                                                                                                                          ifelse(is.na(contacts_u5_all_unique$UTLA19CD_6) & !is.na(contacts_u5_all_unique$UTLA19CD_5) &
                                                                                                                                                                                                                                   is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_5) & !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4) &
                                                                                                                                                                                                                                   contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_4, contacts_u5_all_unique$UTLA19CD_5,
                                                                                                                                                                                                                                 ifelse(!is.na(contacts_u5_all_unique$UTLA19CD_6) & 
                                                                                                                                                                                                                                          !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_6)&
                                                                                                                                                                                                                                          contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_6, contacts_u5_all_unique$UTLA19CD_6,
                                                                                                                                                                                                                                        ifelse(!is.na(contacts_u5_all_unique$UTLA19CD_6) & 
                                                                                                                                                                                                                                                 !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_6)& !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_5) & 
                                                                                                                                                                                                                                                 contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_6 & 
                                                                                                                                                                                                                                                 contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_5, contacts_u5_all_unique$UTLA19CD_5,
                                                                                                                                                                                                                                               ifelse(!is.na(contacts_u5_all_unique$UTLA19CD_6) &
                                                                                                                                                                                                                                                        !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_5)& !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4)&
                                                                                                                                                                                                                                                        contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_5 &
                                                                                                                                                                                                                                                        contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_4, contacts_u5_all_unique$UTLA19CD_4,
                                                                                                                                                                                                                                                      ifelse(!is.na(contacts_u5_all_unique$UTLA19CD_6) &
                                                                                                                                                                                                                                                               !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4)& !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3)&
                                                                                                                                                                                                                                                               contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_4 &
                                                                                                                                                                                                                                                               contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_3, contacts_u5_all_unique$UTLA19CD_3,
                                                                                                                                                                                                                                                             ifelse(!is.na(contacts_u5_all_unique$UTLA19CD_6) &
                                                                                                                                                                                                                                                                      !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3)& !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2)&
                                                                                                                                                                                                                                                                      contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_3 &
                                                                                                                                                                                                                                                                      contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_2, contacts_u5_all_unique$UTLA19CD_2,
                                                                                                                                                                                                                                                                    ifelse(!is.na(contacts_u5_all_unique$UTLA19CD_6) &
                                                                                                                                                                                                                                                                             !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2)&
                                                                                                                                                                                                                                                                             contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_2,contacts_u5_all_unique$UTLA19CD_1,
                                                                                                                                                                                                                                                                           ifelse(!is.na(contacts_u5_all_unique$UTLA19CD_6) &
                                                                                                                                                                                                                                                                                    !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_5)& !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3)& is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_4)&
                                                                                                                                                                                                                                                                                    contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_5 & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_3, contacts_u5_all_unique$UTLA19CD_4, 
                                                                                                                                                                                                                                                                                  ifelse(!is.na(contacts_u5_all_unique$UTLA19CD_6) &
                                                                                                                                                                                                                                                                                           !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_3)& !is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_1)& is.na(contacts_u5_all_unique$ReferralRequest_ReceivedDate_2)&
                                                                                                                                                                                                                                                                                           contacts_u5_all_unique$Contact_Date<contacts_u5_all_unique$ReferralRequest_ReceivedDate_3 & contacts_u5_all_unique$Contact_Date>=contacts_u5_all_unique$ReferralRequest_ReceivedDate_1, contacts_u5_all_unique$UTLA19CD_2, 
                                                                                                                                                                                                                                                                                         NA))))))))))))))))))))))))))))))))))))

# health visiting is combined for LAs Cornwall and Isles of Scilly
# and for Hackney and City of London

# combine these LAs
contacts_u5_all_unique$la[contacts_u5_all_unique$la=="E06000053"] <- "E06000052"
contacts_u5_all_unique$la[contacts_u5_all_unique$la=="E09000001"] <- "E09000012"

contacts_u5_all_unique$la <- as.factor(contacts_u5_all_unique$la)

# rename Cornwall and Hackney codes
las <- contacts_u5_all_unique %>%
  ungroup() %>%
  select(UTLA19CD_1,UTLA19NM_1) %>%
  distinct(UTLA19CD_1, .keep_all = TRUE)

las <- las[!(las$UTLA19CD_1=="E06000053" |las$UTLA19CD_1=="E09000001"),]
las <- las[order(las$UTLA19CD_1),]
las$UTLA19NM_1[las$UTLA19CD_1=="E06000052"] <- "Cornwall and Isles of Scilly"
las$UTLA19NM_1[las$UTLA19CD_1=="E09000012"] <- "Hackney and City of London"

# add all LA names to each contact in the dataset the dataset
colnames(las)[colnames(las)=="UTLA19NM_1"] <- "la_name"
colnames(las)[colnames(las)=="UTLA19CD_1"] <- "la"
las$la <- as.factor(las$la)

contacts_u5_all_unique <- left_join(contacts_u5_all_unique,las, by="la")


#---- 3 Assign the quarter to each contact  -------------------------------

contacts_u5_all_unique$quarter <- lubridate::quarter(contacts_u5_all_unique$Contact_Date, with_year=TRUE, fiscal_start=4)
contacts_u5_all_unique$quarter <- paste0(as.integer(contacts_u5_all_unique$quarter)-2001,as.integer(contacts_u5_all_unique$quarter)-2000,"q",round((contacts_u5_all_unique$quarter%%1)*10))
freq(contacts_u5_all_unique$quarter)


#---- 4 Create datasets summarising quarters -------------------------------

# for each LA and for each quarter, count the number of mandated
# contacts, which are added into separate data sets

quarters <- unique(contacts_u5_all_unique$quarter)

for (i in quarters) {
  assign(paste0("csds_",i), data.frame(la=sort(unique(contacts_u5_all_unique$la)),
                                       yrq=i,
                                       nbv_csds=NA, sixw_csds=NA, oney_csds=NA, twoy_csds=NA))
}

quarterdf <- list(csds_1819q1,csds_1819q2,csds_1819q3,csds_1819q4,
                  csds_1920q1,csds_1920q2,csds_1920q3,csds_1920q4)

names(quarterdf) <- c("csds_1819q1","csds_1819q2","csds_1819q3","csds_1819q4",
                      "csds_1920q1","csds_1920q2","csds_1920q3","csds_1920q4")


for(i in 1:length(quarterdf)) {
  quarterdf[[i]]$nbv_csds <- contacts_u5_all_unique %>%
    filter(quarter==substring(names(quarterdf)[i],6,11) & mandated_nbv==1) %>%
    group_by(la, .drop=FALSE) %>%
    tally() %>%
    pull(n)

  quarterdf[[i]]$sixw_csds <- contacts_u5_all_unique %>%
    filter(quarter==substring(names(quarterdf)[i],6,11) & mandated_6w==1) %>%
    group_by(la, .drop=FALSE) %>%
    tally() %>%
    pull(n)
  
  quarterdf[[i]]$oney_csds <- contacts_u5_all_unique %>%
    filter(quarter==substring(names(quarterdf)[i],6,11) & mandated_1y==1) %>%
    group_by(la, .drop=FALSE) %>%
    tally() %>%
    pull(n)
  
quarterdf[[i]]$twoy_csds <- contacts_u5_all_unique %>%
    filter(quarter==substring(names(quarterdf)[i],6,11) & mandated_2y==1) %>%
    group_by(la, .drop=FALSE) %>%
    tally() %>%
    pull(n)
  
  assign(names(quarterdf)[i], as.data.frame(quarterdf[[i]]))
}


#---- 5 Save each quarter dataset --------------------------------------

for (i in 1:length(quarterdf)) {
  write.csv(quarterdf[[i]],file = paste0("3_ref_",names(quarterdf)[i],".csv"),row.names = FALSE)
}

# merge to a single long dataset 
csds <- rbind(csds_1819q1,csds_1819q2,csds_1819q3,csds_1819q4,
              csds_1920q1,csds_1920q2,csds_1920q3,csds_1920q4)


#---- 6 Compare CSDS with PH metrics ------------------------------------

# merge CSDS counts with the PHE aggregate metrics
assess <- merge(csds, phmetrics, by =c ("la","yrq"))

# divide count from csds by count from phe metrics
# for each mandated contact
assess$nbv_perc <- assess$nbv_csds / assess$nbv_num *100
assess$sixw_perc <- assess$sixw_csds / assess$sixw_num *100
assess$oney_perc <- assess$oney_csds / assess$oney_num *100
assess$twoy_perc <- assess$twoy_csds / assess$twoy_num *100

# following this we conducted a manual visual assessment and updated
# those with inconsistent PHE aggregate values to missing


#---- 7 Plot the agreement between CSDS and metrics ----------------------

# create categories for bar graph
assess$nbv_perc_cat <- cut(assess$nbv_perc,breaks = seq(0,170,by=5),labels = FALSE, include.lowest = TRUE, right = FALSE)
assess$nbv_perc_cat <- (assess$nbv_perc_cat-1)*5

assess$sixw_perc_cat <- cut(assess$sixw_perc,breaks = seq(0,175,by=5),labels = FALSE, include.lowest = TRUE, right = FALSE)
assess$sixw_perc_cat <- (assess$sixw_perc_cat-1)*5

assess$oney_perc_cat <- cut(assess$oney_perc,breaks = seq(0,225,by=5),labels = FALSE, include.lowest = TRUE, right = FALSE)
assess$oney_perc_cat <- (assess$oney_perc_cat-1)*5

assess$twoy_perc_cat <- cut(assess$twoy_perc,breaks = seq(0,375,by=5),labels = FALSE, include.lowest = TRUE, right = FALSE)
assess$twoy_perc_cat <- (assess$twoy_perc_cat-1)*5

# assign an output .png file to the plot
agg_png("3_data quality graphs.png",width=30,height =20,units="cm", res=400,scaling = 1)


# create four separate plots and arrange 2 by 2
ggarrange(
  
  ggplot(assess,aes(x=nbv_perc_cat))+
    geom_bar(width=5,position = position_nudge(x=2.5),fill="#756BB1")+
    scale_x_continuous(limits = c(0,200),breaks = seq(0,200,by=20))+
    scale_y_continuous(limits=c(0,400),breaks = seq(0,400,by=50))+
    geom_vline(xintercept = c(85,115), linetype="dashed", size=1,colour="gray40")+
    ylab("Number of LA-quarters")+
    xlab("")+
    annotate("text",x=72,y=400,label="85%",size=5,colour="gray40")+
    annotate("text",x=130,y=400,label="115%",size=5,colour="gray40")+
    theme(panel.background = element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(colour="black",size=14),
          axis.text.x = element_text(colour="black",size=14),
          axis.title.y = element_text(size=18,vjust=3)),
  
  ggplot(assess,aes(x=sixw_perc_cat))+
    geom_bar(width=5,position = position_nudge(x=2.5),fill="#FD8D3C")+
    scale_x_continuous(limits = c(0,200),breaks = seq(0,200,by=20))+
    scale_y_continuous(limits=c(0,400),breaks = seq(0,400,by=50))+
    geom_vline(xintercept = c(85,115), linetype="dashed", size=1,colour="gray40")+
    ylab("")+
    xlab("")+
    annotate("text",x=72,y=400,label="85%",size=5,colour="gray40")+
    annotate("text",x=130,y=400,label="115%",size=5,colour="gray40")+
    theme(panel.background = element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(colour="black",size=14),
          axis.text.x = element_text(colour="black",size=14)),
  
  ggplot(assess,aes(x=oney_perc_cat))+
    geom_bar(width=5,position = position_nudge(x=2.5), fill="#3182BD")+
    scale_x_continuous(limits = c(0,200),breaks = seq(0,200,by=20))+
    scale_y_continuous(limits=c(0,400),breaks = seq(0,400,by=50))+
    geom_vline(xintercept = c(85,115), linetype="dashed", size=1,colour="gray40")+
    ylab("Number of LA-quarters")+
    xlab("% agreement with OHID HV metrics")+
    geom_label(x=190,y=50,label="*2 outliers\nremoved",colour="gray40",lineheight=0.7,size=4.5,label.size = NA)+
    theme(panel.background = element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(colour="black",size=14),
          axis.text.x = element_text(colour="black",size=14),
          axis.title.y = element_text(size=18,vjust=3),
          axis.title.x = element_text(size=18,vjust=-2)),
  
  ggplot(assess,aes(x=twoy_perc_cat))+
    geom_bar(width=5,position = position_nudge(x=2.5),fill="#31A354")+
    scale_x_continuous(limits = c(0,200),breaks = seq(0,200,by=20))+
    scale_y_continuous(limits=c(0,400),breaks = seq(0,400,by=50))+
    geom_vline(xintercept = c(85,115), linetype="dashed", size=1,colour="gray40")+
    ylab("")+
    xlab("% agreement with OHID HV metrics")+
    geom_label(x=190,y=50,label="*8 outliers\nremoved",colour="gray40",lineheight=0.7,size=4.5,label.size = NA)+
    theme(panel.background = element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(colour="black",size=14),
          axis.text.x = element_text(colour="black",size=14),
          axis.title.x = element_text(size=18,vjust=-2)),
  
  labels=c("A","B","C","D"),font.label = list(size=20), vjust = -0.4, hjust=-2,
  ncol=2,nrow=2)+
  theme(plot.margin = margin(25,0,10,10,unit="pt"))

dev.off()


#---- 8 Assign thresholds for completeness ------------------------------

# using the above plots and discussion of previous literature,
# we created cut offs to identify those LA-quarters that had complete
# CSDS data (i.e. high agreement between CSDS and PHE metrics)

assess$nbv_complete <- ifelse(assess$nbv_perc>=85 & assess$nbv_perc<=115,1,0)
assess$sixw_complete <- ifelse(assess$sixw_perc>=85 & assess$sixw_perc<=115,1,0)
assess$oney_complete <- ifelse(assess$oney_perc>=85 & assess$oney_perc<=115,1,0)
assess$twoy_complete <- ifelse(assess$twoy_perc>=85 & assess$twoy_perc<=115,1,0)


#---- 9 save list of LA quarters that are complete ----------------------

assess <- assess[,c("la","yrq","nbv_complete","sixw_complete","oney_complete","twoy_complete")]
save(assess,file = "3_complete_las_numerator.Rdata")


