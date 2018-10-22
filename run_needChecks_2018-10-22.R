##########
# This script is to find susceptible individuals in 2012
##########

### Load libraries
library(sqldf)

### clear working environment
rm(list = ls())

##################### 
### Load datasets
setwd("~/Dropbox/GouldLab/Project_Mosquito/QGIS_Files/Routput")
# masterdec.csv is the master list of houses with GPS codes from Amy's group
masterdec <- read.csv("masterdec.csv", header = T, sep = ",")
# masterdec_ngs.csv is the list of ngs samples used in lib5
ngs <- read.csv("masterdec_ngs.csv", header = T, sep = ",")

setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")
# kdrData_reduced_[date].csv is the file that contains all samples with kdr genotypes and known location codes
kdr <- read.csv("kdrData_reduced_2018-10-18.csv", header = T, sep = ",")


##################### 
### Prep dataframes for merging
# Create new column with only first 6 characters from Location_Code
kdr$LOC_CODE <- substr(kdr$Location_Code, 1, 6)

##################### 
### Merge dataframes
# Merge masterdec and kdr dataframes together (aka - inner join)
masterdec_kdr <- merge(masterdec, kdr, by = "LOC_CODE")

# Reduce dataset for easier viewing 
reduced <- masterdec_kdr[c("LOC_CODE","X","Y","mosquito_id","newDate","haplotype")]
as.data.frame(reduced)

##################### 
### Subset data
# subset based on year
collected2012 <- sqldf("Select * from reduced where newDate between '2012-01-01' and '2012-12-31'")
# subset any sample with an S allele at the 1534 locus
S1534 <- collected2012[grep("\\w\\wS\\w", collected2012$haplotype),]


#####################
### Prep dfs and write.csv file to use for checking on individuals
# reorder S1534
S1534 <- S1534[c(4,1:3,5:6)]
# subset ngs
ngs <- ngs[c(1,3:7)]
# rbind S1534 and ngs
bound <- rbind(S1534, ngs)
# write.csv file 
write.csv(bound, "needChecks_2018-10-22.csv", row.names = F)
