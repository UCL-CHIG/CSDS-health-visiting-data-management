########################################################
# Title: 2 Preparing CSDS health visiting contacts
# Created by: Amanda Clery
# Date: January 2024

# This script prepares the raw CSDS data to identify
# health visiting contacts that took place between 
# April 2018 and March 2020 for children under 5 
# living in England 

# Corresponds to thesis sections 4.2.2 & 4.2.3

########################################################

#---- 0 Install packages ----------------------------------------

library(dplyr)


#---- 1 Open and convert raw data -------------------------------

setwd("file path to the data X:/ ...")

# open the care activities datasets for each financial year
contact18 <- read.csv(file="XXXXX.csv")
contact19 <- read.csv(file="XXXXX.csv")

# and resave as .Rdata files
save(contact18,file="XXXXX.Rdata")
save(contact19,file="XXXXX.Rdata")

# open the prepared demographics dataset
load(file = "1_unique_u5s.Rdata")


#---- 2 Keep activities only for children under 5 -----------------

# merge all years of data together
contacts_all <- rbind(contact18,contact19)

# keep activities corresponding to under 5s
contacts_u5_all <- inner_join(contacts_all,demo_u5_all,by="Token_Person_ID")


#---- 3 Keep health visiting activities only  ---------------------

# keep only if team type = HV or activity type = HV
contacts_u5_all <- contacts_u5_all[contacts_u5_all$TeamType==16 | contacts_u5_all$Activity_Type==8 | contacts_u5_all$Activity_Type==9 | contacts_u5_all$Activity_Type==10 | contacts_u5_all$Activity_Type==11 | contacts_u5_all$Activity_Type==12,]

# exclude if team type and activity type are both missing
contacts_u5_all <- contacts_u5_all[!(is.na(contacts_u5_all$TeamType) & is.na(contacts_u5_all$Activity_Type)),]


#---- 4 Keep attended activities only  ----------------------------

# generate flag for those that did not attend
contacts_u5_all$dna_flag <- ifelse(contacts_u5_all$AttendOrNot==5 | contacts_u5_all$AttendOrNot==6,0,
                                ifelse(contacts_u5_all$AttendOrNot==0 | contacts_u5_all$AttendOrNot==2 | contacts_u5_all$AttendOrNot==3 | contacts_u5_all$AttendOrNot==4 | contacts_u5_all$AttendOrNot==7,1,NA))

# if attendance variable is missing, keep only if the activity was 5 mins or longer
contacts_u5_all$dna_flag[is.na(contacts_u5_all$dna_flag) &!is.na(contacts_u5_all$CareContact_Duration) & contacts_u5_all$CareContact_Duration>=5] <- 0
contacts_u5_all$dna_flag[is.na(contacts_u5_all$dna_flag) &!is.na(contacts_u5_all$CareContact_Duration) & contacts_u5_all$CareContact_Duration<5] <- 1
contacts_u5_all$dna_flag[is.na(contacts_u5_all$dna_flag) & is.na(contacts_u5_all$CareContact_Duration)] <- 1

# exclude those that did not attend
contacts_u5_all <- contacts_u5_all[contacts_u5_all$dna_flag==0 & !is.na(contacts_u5_all$dna_flag),]


#---- 5 Keep one record per contact  -----------------------------

# before keeping one record must ensure the mandated flag is kept
# flag  mandated contacts
contacts_u5_all$mandated_flag <- ifelse(contacts_u5_all$Activity_Type==8 | contacts_u5_all$Activity_Type==9 | contacts_u5_all$Activity_Type==10 | contacts_u5_all$Activity_Type==11,1,0)
contacts_u5_all$mandated_flag[is.na(contacts_u5_all$Activity_Type) | contacts_u5_all$Activity_Type==""] <- NA

# there are some contact dates that have multiple contact IDs
# assume only one contact per day
# order by care contact date, keeping mandated flags first
contacts_u5_all_unique$Contact_Date <- as.Date(contacts_u5_all_unique$Contact_Date, format="%Y-%m-%d")
contacts_u5_all <- contacts_u5_all[order(contacts_u5_all$Token_Person_ID,contacts_u5_all$Contact_Date, -contacts_u5_all$mandated_flag),]

# for each child keep only 1 contact per day
contacts_u5_all_unique <- contacts_u5_all %>%
  distinct(Token_Person_ID,Contact_Date, .keep_all = TRUE) 


#---- 6 Identify which contacts are mandated based on activity type ---------------------

# clean up activity type variable
contacts_u5_all_unique$Activity_Type <- sub("^0+","",contacts_u5_all_unique$Activity_Type)
contacts_u5_all_unique$Activity_Type[contacts_u5_all_unique$Activity_Type==""] <- NA
contacts_u5_all_unique$Activity_Type <- as.numeric(contacts_u5_all_unique$Activity_Type)
contacts_u5_all_unique$Activity_Type[contacts_u5_all_unique$Activity_Type>12 & contacts_u5_all_unique$Activity_Type<97 | contacts_u5_all_unique$Activity_Type==99] <- NA

# new birth visit
contacts_u5_all_unique$mandated_nbv_pc <- ifelse(contacts_u5_all_unique$Activity_Type==8,1,0) 

# 6-8-week review
contacts_u5_all_unique$mandated_6w_pc <- ifelse(contacts_u5_all_unique$Activity_Type==9,1,0) 

# 1-year review
contacts_u5_all_unique$mandated_1y_pc <- ifelse(contacts_u5_all_unique$Activity_Type==10,1,0) 

# 2-2.5-year review
contacts_u5_all_unique$mandated_2y_pc <- ifelse(contacts_u5_all_unique$Activity_Type==11,1,0) 


#---- 7 Combine with contacts that are mandated based on age at contact ------------------

# because of the low numbers of mandated contacts identified in step 7, we chose to 
# use a date-derived approach to identify further mandated contacts based on their plausibility
# for children who had any health visiting contact at the age when they would be expected
# to have a mandated contact

# flag contacts that are already coded as a mandated contact
contacts_u5_all_unique$any_practitioner_flag <- with(contacts_u5_all_unique,
                                                     ifelse(Activity_Type==8 | Activity_Type==9 | Activity_Type==10 | Activity_Type==11,1,0))
contacts_u5_all_unique$any_practitioner_flag[is.na(contacts_u5_all_unique$Activity_Type)] <- 0

# new birth visit
# flag contacts that occur at the age when the child is expected to have NBV
contacts_u5_all_unique$ContactBetween8_30Days_Flag <- with(contacts_u5_all_unique,
                                                           ifelse(ContactBetween8_14Days_Flag=="Yes" | ContactBetween15_30Days_Flag=="Yes","Yes","No"))

# create new variable for the new birth visit combining the activity type and date approach
contacts_u5_all_unique$mandated_nbv <- NA
contacts_u5_all_unique$mandated_nbv[contacts_u5_all_unique$mandated_nbv_pc==1] <- 1
contacts_u5_all_unique$mandated_nbv[contacts_u5_all_unique$ContactBetween8_30Days_Flag=="Yes" & contacts_u5_all_unique$any_practitioner_flag==0] <- 1

# 6-8-week review
contacts_u5_all_unique$mandated_6w <- NA
contacts_u5_all_unique$mandated_6w[contacts_u5_all_unique$mandated_6w_pc==1] <- 1
contacts_u5_all_unique$mandated_6w[contacts_u5_all_unique$ContactBetween42_63Days_Flag=="Yes" & contacts_u5_all_unique$any_practitioner_flag==0] <- 1

# 1-year review
contacts_u5_all_unique$mandated_1y <- NA
contacts_u5_all_unique$mandated_1y[contacts_u5_all_unique$mandated_1y_pc==1] <- 1
contacts_u5_all_unique$mandated_1y[contacts_u5_all_unique$ContactBetween270_457Days_Flag=="Yes" & contacts_u5_all_unique$any_practitioner_flag==0] <- 1

# 2-2.5-year review
contacts_u5_all_unique$mandated_2y <- NA
contacts_u5_all_unique$mandated_2y[contacts_u5_all_unique$mandated_2y_pc==1] <- 1
contacts_u5_all_unique$mandated_2y[contacts_u5_all_unique$ContactBetween691_914Days_Flag=="Yes" & contacts_u5_all_unique$any_practitioner_flag==0] <- 1


#---- 8 Allow only one mandated contact of each type per child ------------------------

# some children have multiple of the same mandated contact recorded
# this is likely to be recording error because in practice children only get one of 
# each mandated contact

# new birth visit
# identify those children with >1 mandated flag
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID) %>%
  mutate(n_mandated_nbv=sum(mandated_nbv,na.rm = TRUE))

# identify those which have the practitioner code
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID) %>%
  mutate(activity8=case_when(n_mandated_nbv>1 ~ any(mandated_nbv_pc==1, na.rm = TRUE)))

# remove over counts by prioritising activity type code
contacts_u5_all_unique$mandated_nbv[contacts_u5_all_unique$activity8==TRUE & (contacts_u5_all_unique$Activity_Type!=8 | is.na(contacts_u5_all_unique$Activity_Type))] <- NA

# remove over counts by prioritising the earliest contact
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID,mandated_nbv) %>%
  mutate(first_nbv=case_when(n_mandated_nbv>1 ~ min(Contact_Date)))

contacts_u5_all_unique$mandated_nbv[contacts_u5_all_unique$mandated_nbv==1 & contacts_u5_all_unique$Contact_Date!=contacts_u5_all_unique$first_nbv] <- NA

# 6-8-week review
# identify those children with >1 mandated flag
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID) %>%
  mutate(n_mandated_6w=sum(mandated_6w,na.rm = TRUE))

# identify those which have the practitioner code
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID) %>%
  mutate(activity9=case_when(n_mandated_6w>1 ~ any(mandated_6w_pc==1)))

# remove over counts by prioritising activity type code
contacts_u5_all_unique$mandated_6w[contacts_u5_all_unique$activity9==TRUE & (contacts_u5_all_unique$Activity_Type!=9 | is.na(contacts_u5_all_unique$Activity_Type))] <- NA

# remove over counts by prioritising the earliest contact
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID,mandated_6w) %>%
  mutate(first_6w=case_when(n_mandated_6w>1 ~ min(Contact_Date)))

contacts_u5_all_unique$mandated_6w[contacts_u5_all_unique$mandated_6w==1 & contacts_u5_all_unique$Contact_Date!=contacts_u5_all_unique$first_6w] <- NA

# 1-year review
# identify those children with >1 mandated flag
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID) %>%
  mutate(n_mandated_1y=sum(mandated_1y,na.rm = TRUE))

# identify those which have the practitioner code
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID) %>%
  mutate(activity10=case_when(n_mandated_1y>1 ~ any(mandated_1y_pc==1, na.rm = TRUE)))

# remove over counts by prioritising activity type code
contacts_u5_all_unique$mandated_1y[contacts_u5_all_unique$activity10==TRUE & (contacts_u5_all_unique$Activity_Type!=10 | is.na(contacts_u5_all_unique$Activity_Type))] <- NA

# remove over counts by prioritising the earliest contact
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID,mandated_1y) %>%
  mutate(first_1y=case_when(n_mandated_1y>1 ~ min(Contact_Date)))

contacts_u5_all_unique$mandated_1y[contacts_u5_all_unique$mandated_1y==1 & contacts_u5_all_unique$Contact_Date!=contacts_u5_all_unique$first_1y] <- NA

# 2-2.5-year review
# identify those children with >1 mandated flag
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID) %>%
  mutate(n_mandated_2y=sum(mandated_2y,na.rm = TRUE))

# identify those which have the practitioner code
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID) %>%
  mutate(activity11=case_when(n_mandated_2y>1 ~ any(mandated_2y_pc==1, na.rm = TRUE)))

# remove over counts by prioritising activity type code
contacts_u5_all_unique$mandated_2y[contacts_u5_all_unique$activity11==TRUE & (contacts_u5_all_unique$Activity_Type!=11 | is.na(contacts_u5_all_unique$Activity_Type))] <- NA

# remove over counts by prioritising the earliest contact
contacts_u5_all_unique <- contacts_u5_all_unique %>%
  group_by(Token_Person_ID,mandated_2y) %>%
  mutate(first_2y=case_when(n_mandated_2y>1 ~ min(Contact_Date)))

contacts_u5_all_unique$mandated_2y[contacts_u5_all_unique$mandated_2y==1 & contacts_u5_all_unique$Contact_Date!=contacts_u5_all_unique$first_2y] <- NA


#---- 9 Save all health visiting contacts dataset  ------------------

save(contacts_u5_all_unique,file = "2_unique contacts.Rdata")
