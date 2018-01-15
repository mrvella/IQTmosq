################################################################################
### This script counts the number of mosquitoes isolated per house (i.e. location_code)

### Prepare working environment
### clear working environment
rm(list = ls())

### set working directory
setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")

### load isolation log
# make sure that csv file has the date column in the MM-DD-YYYY format
isolLog <- read.csv("IsolationLog_IQT-Mosq.csv", header = T, sep = ",")

Loc_count <- sqldf("select Location_Code, 
                  count (Location_Code) as occurances 
                  from isolLog 
                  group by Location_Code
                  order by occurances desc")
head(Loc_count)