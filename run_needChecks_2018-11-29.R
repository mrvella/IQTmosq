##########
# This script is to find susceptible individuals in 2012
##########

### clear working environment
rm(list = ls())

### Load libraries
library(sqldf)


##################### 
### Load datasets
setwd("~/Dropbox/GouldLab/Project_Mosquito/QGIS_Files/Routput")
# masterdec.csv is the master list of houses with GPS codes from Amy's group
masterdec <- read.csv("masterdec.csv", header = T, sep = ",")
# masterdec_ngs.csv is the list of ngs samples used in lib5
ngs <- read.csv("masterdec_ngs.csv", header = T, sep = ",")

setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")
# kdrData_reduced_[date].csv is the file that contains all samples with kdr genotypes and known location codes
kdr <- read.csv("kdrData_reduced_2018-11-09.csv", header = T, sep = ",")


##################### 
### Prep dataframes for merging
# Create new column with only first 6 characters from Location_Code
kdr$LOC_CODE <- substr(kdr$Location_Code, 1, 6)

##################### 
### Merge dataframes
# Merge masterdec and kdr dataframes together (aka - inner join)
masterdec_kdr <- merge(masterdec, kdr, by = c("LOC_CODE", "X", "Y"))

# Reduce dataset for easier viewing 
reduced <- masterdec_kdr[c("mosquito_id","newDate","haplotype", "LOC_CODE","X","Y")]
as.data.frame(reduced)
head(reduced)


#####################
### Subset data
errors <- sqldf("Select * from reduced where haplotype like '%error%'")
nas <- sqldf("Select * from reduced where haplotype like '%NA%'")


#####################
### write.csv file to use for checking on individuals
setwd("../Data_Mosquito/QualityControl")
# write.csv files
write.csv(errors, "errors_2018-11-29.csv", row.names = F)
write.csv(nas, "nas_2018-11-29.csv", row.names = F)
