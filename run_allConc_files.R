###########
# This file is to create a list of samples to choose from for ddRadSeq analysis

# 0 ################################################################################
### Prepare working environment

### clear working environment
rm(list = ls())

### load libraries
library(gdata)
# options() line below prevents R from stalling while loading sqldf library
options(gsubfn.engine = "R")
library(sqldf)
# required for allele frequency calculations
library(dplyr)
# required to produce plots
library(ggplot2)


# 1 ################################################################################
### set working directory
setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")

# 3 ################################################################################
### Count number of mosquitoes isolated per house (i.e. location_code)
# load isolation log
# make sure that csv file has the date column in the MM-DD-YYYY format

isolLog <- read.csv("IsolationLog_IQT-Mosq.csv", header = T, sep = ",")

kdr <- read.csv("kdrData_all.csv", header = T, sep = ",")

conc <- read.csv("IQT2014Concentrations_2017-08-23.csv", header = T, sep = ",")

# 3 ################################################################################
allConc <- sqldf("SELECT conc.*
                 , kdr.Location_Code
                 , kdr.Zone1016_1
                 , kdr.Date
                 FROM conc
                 LEFT JOIN kdr
                 ON conc.mosquito_id = kdr.mosquito_id
                 ")
head(allConc)  
  
# Convert current date to sqldf readable form
allConc$newDate <- as.character(as.Date(as.character(allConc$Date), format = "%m/%d/%Y"))

# Select based on months
allConc_pre <- sqldf("Select * from allConc where newDate between '2014-01-01' and '2014-04-30'")
allConc_post <- sqldf("Select * from allConc where newDate between '2014-05-01' and '2014-10-31'")

# Subset based on zone
allConc_pre_buff <- sqldf("Select * from allConc_pre where Zone1016_1 = 'buffer'")
allConc_pre_trt <- sqldf("Select * from allConc_pre where Zone1016_1 = 'treatment'")
allConc_post_buff <- sqldf("Select * from allConc_post where Zone1016_1 = 'buffer'")
allConc_post_trt <- sqldf("Select * from allConc_post where Zone1016_1 = 'treatment'")

# Order by concentration
allConc_pre_buff <- allConc_pre_buff[with(allConc_pre_buff, order(-conc)), ]
allConc_pre_trt <- allConc_pre_trt[with(allConc_pre_trt, order(-conc)), ]
allConc_post_buff <- allConc_post_buff[with(allConc_post_buff, order(-conc)), ]
allConc_post_trt <- allConc_post_trt[with(allConc_post_trt, order(-conc)), ]

# Select top 30 samples
allConc_pre_buff_final <- allConc_pre_buff[1:50,]
allConc_pre_trt_final <- allConc_pre_trt[1:50,]
allConc_post_buff_final <-  allConc_post_buff[1:50,]
allConc_post_trt_final <- allConc_post_trt[1:50,]

# Save as .csv files
write.csv(allConc_pre_buff_final, file = "allConc_pre_buff.csv", row.names = F)
write.csv(allConc_pre_trt_final, file = "allConc_pre_trt.csv", row.names = F)
write.csv(allConc_post_buff_final, file = "allConc_post_buff.csv", row.names = F)
write.csv(allConc_post_trt_final, file = "allConc_post_trt.csv", row.names = F)



