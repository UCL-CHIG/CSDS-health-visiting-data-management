########################################################
# Title: 1 Preparing CSDS child cohort
# Created by: Amanda Clery
# Date: January 2024

# This script prepares the raw CSDS data to identify
# children under 5 living in England between April 2018 
# and March 2020

# Corresponds to thesis section 4.2.1

########################################################

#---- 0 Install packages ----------------------------------------

library(dplyr)
library(data.table)
library(reshape2)


#---- 1 Open and convert raw data -------------------------------

setwd("file path to the data X:/ ...")

# open the demographics and referrals datasets for each financial year
demo18 <- read.csv(file="XXXXX.csv")
demo19 <- read.csv(file="XXXXX.csv")

# and resave as .Rdata files
save(demo18,file = "XXXXX.Rdata")
save(demo19,file = "XXXXX.Rdata")

# open publicly available local authority look up
la_codes <- read.csv(file="Lower_Tier_Local_Authority_to_Upper_Tier_Local_Authority__April_2019__Lookup_in_England_and_Wales.csv")
la_codes <- la_codes[,c(2,4,5)]

#---- 2 Create cohort of under 5s -------------------------------

# merge all years of data together
demo_all <- rbind(demo18,demo19)

# keep only those who are under 5 at extract end date
demo_u5_all <- demo_all[demo_all$AgeBand_Extract_EndDate=="0-4"]

# remove missing token ids
demo_u5_all <- demo_u5_all[demo_u5_all$Token_Person_ID!="",]

# order data so that children with multiple rows of data appear
# in order of when the referral was received
demo_u5_all <- demo_u5_all[order(demo_u5_all$Token_Person_ID,demo_u5_all$ReferralRequest_ReceivedDate),]


#---- 3 Include only children living in England -----------------

# create a variable grouping the local authority codes into country
demo_u5_all$country <- ifelse(grepl("E",demo_u5_all$LocalAuthority), "England",
                              ifelse(grepl("S",demo_u5_all$LocalAuthority),"Scotland",
                                     ifelse(grepl("N",demo_u5_all$LocalAuthority),"Northern Ireland",
                                            ifelse(grepl("W",demo_u5_all$LocalAuthority),"Wales",
                                                   ifelse(grepl("L",demo_u5_all$LocalAuthority),"Channel Islands",
                                                          ifelse(grepl("M",demo_u5_all$LocalAuthority),"Isle of Man",NA))))))

# keep only records in England
demo_u5_all <- demo_u5_all[demo_u5_all$country=="England" & !is.na(demo_u5_all$country),]

# update inactive local authorities for time period of interest for analysis
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E06000028"] <- "E06000058"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E06000029"] <- "E06000058"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000048"] <- "E06000058"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000049"] <- "E06000059"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000050"] <- "E06000059"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000051"] <- "E06000059"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000052"] <- "E06000059"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000053"] <- "E06000059"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000190"] <- "E07000246"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000191"] <- "E07000246"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000201"] <- "E07000245"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000204"] <- "E07000245"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000205"] <- "E07000244"
demo_u5_all$LocalAuthority[demo_u5_all$LocalAuthority=="E07000206"] <- "E07000244"

# merge on local authority code look up to identify the 151 upper tier LA codes
# that correspond to the 317 LocalAuthority values
demo_u5_all <- left_join(demo_u5_all,la_codes,by=c("LocalAuthority"="LTLA19CD"))


#---- 4 Clean and generate demographic variables of interest ----------------------

# estimated date of birth (using month and year turning 366 days)
# use 15th of the month as estimated day
demo_u5_all$MonthYear_Turning366Days_dt <- as.Date(paste0(demo_u5_all$MonthYear_Turning366Days,"-15"),"%Y-%m-%d")
demo_u5_all$e_dob <- demo_u5_all$MonthYear_Turning366Days_dt-366

# gender
demo_u5_all$gender <- demo_u5_all$Gender
demo_u5_all$gender[demo_u5_all$gender==""| demo_u5_all$gender=="0"| demo_u5_all$gender=="3"|demo_u5_all$gender=="C"|demo_u5_all$gender=="U"|demo_u5_all$gender=="X"] <- NA
demo_u5_all$gender[demo_u5_all$gender=="F"] <- "2"
demo_u5_all$gender[demo_u5_all$gender=="M"] <- "1"
demo_u5_all$gender[demo_u5_all$gender=="1"] <- "Male"
demo_u5_all$gender[demo_u5_all$gender=="2"] <- "Female"
demo_u5_all$gender[demo_u5_all$gender=="9"] <- "Indeterminate"

# ethnicity
demo_u5_all$ethcat <- demo_u5_all$EthnicCategory
demo_u5_all$ethcat[demo_u5_all$ethcat==""|demo_u5_all$ethcat=="99"|demo_u5_all$ethcat=="2"|demo_u5_all$ethcat=="3"|demo_u5_all$ethcat=="6"|demo_u5_all$ethcat=="8"|demo_u5_all$ethcat=="9"|demo_u5_all$ethcat=="V"|demo_u5_all$ethcat=="V8"|demo_u5_all$ethcat=="Z"] <- NA
demo_u5_all$ethcat[grepl("^C",demo_u5_all$ethcat,ignore.case = FALSE)] <- "C"
demo_u5_all$ethcat[grepl("^G",demo_u5_all$ethcat,ignore.case = FALSE)] <- "G"
demo_u5_all$ethcat[grepl("^L",demo_u5_all$ethcat,ignore.case = FALSE)] <- "L"
demo_u5_all$ethcat[grepl("^P",demo_u5_all$ethcat,ignore.case = FALSE)] <- "P"
demo_u5_all$ethcat[grepl("^S",demo_u5_all$ethcat,ignore.case = FALSE)] <- "S"

# ethnicity - grouped into the 5 census umbrella categories
demo_u5_all$ethcat5 <- demo_u5_all$ethcat
demo_u5_all$ethcat5[demo_u5_all$ethcat5=="A"|demo_u5_all$ethcat5=="B"|demo_u5_all$ethcat5=="C"] <- "White"
demo_u5_all$ethcat5[demo_u5_all$ethcat5=="D"|demo_u5_all$ethcat5=="E"|demo_u5_all$ethcat5=="F"|demo_u5_all$ethcat5=="G"] <- "Mixed"
demo_u5_all$ethcat5[demo_u5_all$ethcat5=="H"|demo_u5_all$ethcat5=="J"|demo_u5_all$ethcat5=="K"|demo_u5_all$ethcat5=="L"] <- "Asian"
demo_u5_all$ethcat5[demo_u5_all$ethcat5=="M"|demo_u5_all$ethcat5=="N"|demo_u5_all$ethcat5=="P"] <- "Black"
demo_u5_all$ethcat5[demo_u5_all$ethcat5=="R"|demo_u5_all$ethcat5=="S"] <- "Other"


#---- 5 Manage records which have duplicates per child -------------------

# ultimately want to keep one row per child
# before keeping only one record per child, need to ensure 
# that the data of interest is present and consistent across
# rows for the same child

# if data is present on one row, but missing on the other, replace
demo_u5_all <- demo_u5_all %>%
  group_by(Token_Person_ID) %>%
  fill(e_dob:ethcat5, .direction = "updown")

# if date of birth did not match keep the most common
demo_u5_all <- demo_u5_all %>%
  group_by(Token_Person_ID) %>%
  mutate(commondob = as.Date(names(which.max(table(e_dob))),"%Y-%m-%d")) %>%
  mutate(mismatch = ifelse(length(unique(e_dob[!is.na(e_dob)]))>1,1,0)) %>%
  mutate(e_dob = ifelse(mismatch==1,commondob,e_dob))

# if other variables did not match keep the earliest recorded
# ie closest to birth

# identify the earliest record
demo_u5_all$dups <- ave(1:length(demo_u5_all$Token_Person_ID),demo_u5_all$Token_Person_ID,FUN = seq_along)

# replace all values with the earliest record for each variable
demo_u5_all <- demo_u5_all %>%
  group_by(Token_Person_ID) %>%
  mutate(earlygender = gender[dups==1]) %>%
  mutate(mismatch = ifelse(length(unique(gender[!is.na(gender)]))>1,1,0)) %>%
  mutate(gender = ifelse(mismatch==1,earlygender,gender))

demo_u5_all <- demo_u5_all %>%
  group_by(Token_Person_ID) %>%
  mutate(earlyethcat = ethcat[dups==1]) %>%
  mutate(mismatch = ifelse(length(unique(ethcat[!is.na(ethcat)]))>1,1,0)) %>%
  mutate(ethcat = ifelse(mismatch==1,earlyethcat,ethcat))

demo_u5_all <- demo_u5_all %>%
  group_by(Token_Person_ID) %>%
  mutate(earlyethcat5 = ethcat5[dups==1]) %>%
  mutate(mismatch = ifelse(length(unique(ethcat5[!is.na(ethcat5)]))>1,1,0)) %>%
  mutate(ethcat5 = ifelse(mismatch==1,earlyethcat5,ethcat5))

demo_u5_all <- demo_u5_all %>%
  group_by(Token_Person_ID) %>%
  mutate(earlylsoa = LSOA[dups==1]) %>%
  mutate(mismatch = ifelse(length(unique(LSOA[!is.na(LSOA)]))>1,1,0)) %>%
  mutate(LSOA = ifelse(mismatch==1,earlylsoa,LSOA))


#---- 6 Keep only one demographic record per child -------------------
# now that the variables of interest match, keep one record for each child
# for analysis, also interested in children who move LA
# therefore, need to manage children separately who do and do not move LA

# keep one row per child
demo_u5_all_unique <- demo_u5_all %>% 
  distinct(Token_Person_ID, .keep_all = TRUE)

# keep one row per child and distinct LA
demo_u5_all <- demo_u5_all %>% 
  distinct(Token_Person_ID,UTLA19CD, .keep_all = TRUE)

# count the number of different LAs
demo_u5_all$la_count <- ave(1:length(demo_u5_all$Token_Person_ID),demo_u5_all$Token_Person_ID,FUN = seq_along)

# count the total number of LAs
demo_u5_all$la_tot <- ave(1:length(demo_u5_all$Token_Person_ID),demo_u5_all$Token_Person_ID,FUN = length)

# keep one row per child, but retain the LA data in wide format
demo_u5_all_wide <- dcast(melt(demo_u5_all,id.vars = c("Token_Person_ID","la_count")), Token_Person_ID~variable+la_count)
demo_u5_all_wide <- reshape(demo_u5_all,idvar = "Token_Person_ID",timevar = "la_count", direction = "wide")

# merge the LA data back onto the unique one row per child dataset
demo_u5_all <- merge(demo_u5_all_unique,demo_u5_all_wide,by="Token_Person_ID")


#---- 7 Save demographics data: one record per child -------------------

save(demo_u5_all,file = "1_unique_u5s.Rdata")
