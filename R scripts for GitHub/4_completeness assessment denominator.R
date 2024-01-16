########################################################
# Title: 4 CSDS health visiting completeness assessment
# Created by: Amanda Clery
# Date: January 2024

# This script prepares the CSDS data to count number
# of children expecting their mandated health visiting 
# contacts by LA and quarter between April 2018 and 
# March 2020 for children under 5 living in England.
#
# Then compares these counts to counts available
# publicly published by the Office for Health Improvement
# and Disparities (OHID, previously Public Health England)
# to identify complete CSDS data by LA and quarter for
# the denominator of eligible children

# Corresponds to thesis section 4.2.5

########################################################

#---- 0 Install packages ----------------------------------------

library(dplyr)
library(lubridate)
library(ggplot2)


#---- 1 Open data -----------------------------------------------

setwd("file path to the data X:/ ...")

# open the prepared demographics dataset
load(file = "1_unique_u5s.Rdata")

# open aggregate PH metrics
# these are publicly available and have been prepared to include 
# the counts of each mandated contact and the children eligible
phmetrics <- read.csv(file="XXXXX.csv")


#---- 2 Assign an expected quarter to each child  ---------------

# using existing month-year flags in the CSDS, flag which quarter
# the child is expected to have each mandated contact based on
# when they turn each age

# new birth visit
demo_u5_all$MonthYear_Turning30Days_dt <- as.Date(paste0(demo_u5_all$MonthYear_Turning30Days,"-15"),"%Y-%m-%d")
demo_u5_all$quarter_exp_nbv <- quarter(demo_u5_all$MonthYear_Turning30Days_dt, with_year=TRUE, fiscal_start=4)
demo_u5_all$quarter_exp_nbv <- paste0(as.integer(demo_u5_all$quarter_exp_nbv)-2001,as.integer(demo_u5_all$quarter_exp_nbv)-2000,"q",round((demo_u5_all$quarter_exp_nbv%%1)*10))

# 6-8-week review
demo_u5_all$MonthYear_Turning56Days_dt <- as.Date(paste0(demo_u5_all$MonthYear_Turning56Days,"-15"),"%Y-%m-%d")
demo_u5_all$quarter_exp_6w <- quarter(demo_u5_all$MonthYear_Turning56Days_dt, with_year=TRUE, fiscal_start=4)
demo_u5_all$quarter_exp_6w <- paste0(as.integer(demo_u5_all$quarter_exp_6w)-2001,as.integer(demo_u5_all$quarter_exp_6w)-2000,"q",round((demo_u5_all$quarter_exp_6w%%1)*10))

# 1-year review
demo_u5_all$MonthYear_Turning457Days_dt <- as.Date(paste0(demo_u5_all$MonthYear_Turning457Days,"-15"),"%Y-%m-%d")
demo_u5_all$quarter_exp_1y <- quarter(demo_u5_all$MonthYear_Turning457Days_dt, with_year=TRUE, fiscal_start=4)
demo_u5_all$quarter_exp_1y <- paste0(as.integer(demo_u5_all$quarter_exp_1y)-2001,as.integer(demo_u5_all$quarter_exp_1y)-2000,"q",round((demo_u5_all$quarter_exp_1y%%1)*10))

# 2-2.5-year review
demo_u5_all$MonthYear_Turning914Days_dt <- as.Date(paste0(demo_u5_all$MonthYear_Turning914Days,"-15"),"%Y-%m-%d")
demo_u5_all$quarter_exp_2y <- quarter(demo_u5_all$MonthYear_Turning914Days_dt, with_year=TRUE, fiscal_start=4)
demo_u5_all$quarter_exp_2y <- paste0(as.integer(demo_u5_all$quarter_exp_2y)-2001,as.integer(demo_u5_all$quarter_exp_2y)-2000,"q",round((demo_u5_all$quarter_exp_2y%%1)*10))


#---- 3 Assign an expected LA to each child  --------------------

# based on demographics data, need to determine which LA the child
# is expected to receive each contact in.
# this is straightforward for children who never move LA
# but for those children who do move LA, the LA assigned should be 
# based on the timing of movement between LA, based on referral dates

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


#---- 4 Create datasets summarising quarters -------------------------------

# for each LA and for each quarter, count the number of children expected
# to have each mandated contact, and populate separate data sets

# prepare datasets
quarters <- c("1819q1","1819q2","1819q3","1819q4","1920q1","1920q2","1920q3","1920q4")

for (i in quarters) {
  assign(paste0("csds_denom_",i), data.frame(la=sort(unique(demo_u5_all$la_exp_nbv)),
                                       yrq=i,
                                       nbv_csds_denom=NA, sixw_csds_denom=NA, oney_csds_denom=NA, twoy_csds_denom=NA))
}

quarterdf <- list(csds_denom_1819q1,csds_denom_1819q2,csds_denom_1819q3,csds_denom_1819q4,
                  csds_denom_1920q1,csds_denom_1920q2,csds_denom_1920q3,csds_denom_1920q4)

names(quarterdf) <- c("csds_denom_1819q1","csds_denom_1819q2","csds_denom_1819q3","csds_denom_1819q4",
                      "csds_denom_1920q1","csds_denom_1920q2","csds_denom_1920q3","csds_denom_1920q4")

# populate datasets

for(i in 1:length(quarterdf)) {
  quarterdf[[i]]$nbv_csds_denom <- demo_u5_all %>%
    filter(quarter_exp_nbv==substring(names(quarterdf)[i],12,17)) %>%
    group_by(la_exp_nbv, .drop=FALSE) %>%
    arrange(la_exp_nbv) %>%
    tally() %>%
    pull(n)
  
  quarterdf[[i]]$sixw_csds_denom <- demo_u5_all %>%
    filter(quarter_exp_6w==substring(names(quarterdf)[i],12,17)) %>%
    group_by(la_exp_6w, .drop=FALSE) %>%
    arrange(la_exp_6w) %>%
    tally() %>%
    pull(n)
  
  quarterdf[[i]]$oney_csds_denom <- demo_u5_all %>%
    filter(quarter_exp_1y==substring(names(quarterdf)[i],12,17)) %>%
    group_by(la_exp_1y, .drop=FALSE) %>%
    arrange(la_exp_1y) %>%
    tally() %>%
    pull(n) 
  
  quarterdf[[i]]$twoy_csds_denom <- demo_u5_all %>%
    filter(quarter_exp_2y==substring(names(quarterdf)[i],12,17)) %>%
    group_by(la_exp_2y, .drop=FALSE) %>%
    arrange(la_exp_2y) %>%
    tally() %>%
    pull(n) 
  
  assign(names(quarterdf)[i], as.data.frame(quarterdf[[i]]))
  
}


#---- 5 Save each quarter dataset --------------------------------------

for (i in 1:length(quarterdf)) {
  write.csv(quarterdf[[i]],file = paste0("4_ref_",names(quarterdf)[i],".csv"),row.names = FALSE)
}

# merge to a single long dataset 
csds_denom <- rbind(csds_denom_1819q1,csds_denom_1819q2,csds_denom_1819q3,csds_denom_1819q4,
                    csds_denom_1920q1,csds_denom_1920q2,csds_denom_1920q3,csds_denom_1920q4)


#---- 6 Compare CSDS with PH metrics ------------------------------------

# merge CSDS counts with the PHE aggregate metrics
assess_denom <- merge(csds_denom, phmetrics, by =c ("la","yrq"))

# divide count from csds by count from phe metrics
# for each mandated contact
assess_denom$nbv_perc_denom <- assess_denom$nbv_csds_denom / assess_denom$nbv_denom *100
assess_denom$sixw_perc_denom <- assess_denom$sixw_csds_denom / assess_denom$sixw_denom *100
assess_denom$oney_perc_denom <- assess_denom$oney_csds_denom / assess_denom$oney_denom *100
assess_denom$twoy_perc_denom <- assess_denom$twoy_csds_denom / assess_denom$twoy_denom *100

# following this we conducted a manual visual assessment and updated
# those with inconsistent PHE aggregate values to missing


#---- 7 Plot the agreement between CSDS and metrics ----------------------

# create categories for bar graph
assess_denom$nbv_perc_denom_cat <- cut(assess_denom$nbv_perc_denom,breaks = seq(0,340,by=5),labels = FALSE, include.lowest = TRUE, right = FALSE)
assess_denom$nbv_perc_denom_cat <- (assess_denom$nbv_perc_denom_cat-1)*5

assess_denom$sixw_perc_denom_cat <- cut(assess_denom$sixw_perc_denom,breaks = seq(0,315,by=5),labels = FALSE, include.lowest = TRUE, right = FALSE)
assess_denom$sixw_perc_denom_cat <- (assess_denom$sixw_perc_denom_cat-1)*5

assess_denom$oney_perc_denom_cat <- cut(assess_denom$oney_perc_denom,breaks = seq(0,325,by=5),labels = FALSE, include.lowest = TRUE, right = FALSE)
assess_denom$oney_perc_denom_cat <- (assess_denom$oney_perc_denom_cat-1)*5

assess_denom$twoy_perc_denom_cat <- cut(assess_denom$twoy_perc_denom,breaks = seq(0,310,by=5),labels = FALSE, include.lowest = TRUE, right = FALSE)
assess_denom$twoy_perc_denom_cat <- (assess_denom$twoy_perc_denom_cat-1)*5


# assign an output .png file to the plot
agg_png("4_data quality graphs_denom.png",width=30,height =20,units="cm", res=400,scaling = 1)


# create four separate plots and arrange 2 by 2
ggarrange(
  
  ggplot(assess_denom,aes(x=nbv_perc_denom_cat))+
    geom_bar(width=5,position = position_nudge(x=2.5),fill="#756BB1")+
    scale_x_continuous(limits = c(0,200),breaks = seq(0,200,by=20))+
    scale_y_continuous(limits=c(0,550),breaks = seq(0,550,by=50))+
    geom_vline(xintercept = c(85,115), linetype="dashed", size=1,colour="gray40")+
    ylab("Number of LA-quarters")+
    xlab("")+
    geom_label(x=190,y=50,label="*2 outliers\nremoved",colour="gray40",lineheight=0.7,size=4.5,label.size = NA)+
    annotate("text",x=72,y=550,label="85%",size=5,colour="gray40")+
    annotate("text",x=130,y=550,label="115%",size=5,colour="gray40")+
    theme(panel.background = element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(colour="black",size=14),
          axis.text.x = element_text(colour="black",size=14),
          axis.title.y = element_text(size=18,vjust=3)),
  
  ggplot(assess_denom,aes(x=sixw_perc_denom_cat))+
    geom_bar(width=5,position = position_nudge(x=2.5),fill="#FD8D3C")+
    scale_x_continuous(limits = c(0,200),breaks = seq(0,200,by=20))+
    scale_y_continuous(limits=c(0,550),breaks = seq(0,550,by=50))+
    geom_vline(xintercept = c(85,115), linetype="dashed", size=1,colour="gray40")+
    ylab("")+
    xlab("")+
    geom_label(x=190,y=50,label="*2 outliers\nremoved",colour="gray40",lineheight=0.7,size=4.5,label.size = NA)+
    annotate("text",x=72,y=550,label="85%",size=5,colour="gray40")+
    annotate("text",x=130,y=550,label="115%",size=5,colour="gray40")+
    theme(panel.background = element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(colour="black",size=14),
          axis.text.x = element_text(colour="black",size=14)),
  
  ggplot(assess_denom,aes(x=oney_perc_denom_cat))+
    geom_bar(width=5,position = position_nudge(x=2.5), fill="#3182BD")+
    scale_x_continuous(limits = c(0,200),breaks = seq(0,200,by=20))+
    scale_y_continuous(limits=c(0,550),breaks = seq(0,550,by=50))+
    geom_vline(xintercept = c(85,115), linetype="dashed", size=1,colour="gray40")+
    ylab("Number of LA-quarters")+
    xlab("% agreement with OHID HV metrics")+
    geom_label(x=190,y=50,label="*3 outliers\nremoved",colour="gray40",lineheight=0.7,size=4.5,label.size = NA)+
    theme(panel.background = element_blank(),
          axis.line=element_line(colour = "black"),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(colour="black",size=14),
          axis.text.x = element_text(colour="black",size=14),
          axis.title.y = element_text(size=18,vjust=3),
          axis.title.x = element_text(size=18,vjust=-2)),
  
  ggplot(assess_denom,aes(x=twoy_perc_denom_cat))+
    geom_bar(width=5,position = position_nudge(x=2.5),fill="#31A354")+
    scale_x_continuous(limits = c(0,200),breaks = seq(0,200,by=20))+
    scale_y_continuous(limits=c(0,550),breaks = seq(0,550,by=50))+
    geom_vline(xintercept = c(85,115), linetype="dashed", size=1,colour="gray40")+
    ylab("")+
    xlab("% agreement with OHID HV metrics")+
    geom_label(x=190,y=50,label="*4 outliers\nremoved",colour="gray40",lineheight=0.7,size=4.5,label.size = NA)+
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

assess_denom$nbv_complete_denom <- ifelse(assess_denom$nbv_perc_denom>=85 & assess_denom$nbv_perc_denom<=115,1,0)
assess_denom$sixw_complete_denom <- ifelse(assess_denom$sixw_perc_denom>=85 & assess_denom$sixw_perc_denom<=115,1,0)
assess_denom$oney_complete_denom <- ifelse(assess_denom$oney_perc_denom>=85 & assess_denom$oney_perc_denom<=115,1,0)
assess_denom$twoy_complete_denom <- ifelse(assess_denom$twoy_perc_denom>=85 & assess_denom$twoy_perc_denom<=115,1,0)


#---- 9 save list of LA quarters that are complete ----------------------

assess_denom <- assess_denom[,c("la","yrq","nbv_complete_denom","sixw_complete_denom","oney_complete_denom","twoy_complete_denom")]
save(assess_denom,file = "4_complete_las_denominator.Rdata")


