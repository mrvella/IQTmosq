### Melt Curve Analysis
### File Started: 31 May 2017
### Last Updated: 21 Sep 2017

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


# 2 ################################################################################
### Loop to concatenate MeltCurve data files
source("R_Scripts/loop_MeltCurve_catFiles.R")
# If you recieve the following error -- 
# "Error in file(file, "rt") : cannot open the connection' 
# -- make sure all analysis files are available and named properly in MeltCurve folder"

### set working directory back to Database folder
setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")

# 3 ################################################################################
### load isolation log
# Note: make sure that IsolationLog_IQT-Mosq.csv file has the date column in the MM-DD-YYYY format
isolLog <- read.csv("IsolationLog_IQT-Mosq.csv", header = T, sep = ",")
kdrData <- isolLog
# Convert current date to readable form for R
kdrData$newDate <- as.character(as.Date(as.character(kdrData$Date), format = "%m/%d/%Y"))
# Note: If year starts with 00 after conversion, then go back and save IsolationLog_IQT-Mosq.csv
# with Date column in the correct format. Weird formating issues happen with .csv files


# 4 ################################################################################
### Merge melt curve data with isolation log to create df "kdrData"

# Load merged data for each locus and replicate
merged1016_rep1 <- read.csv("MeltCurve_1016_rep1.csv", header = T, sep = ",")
merged1016_rep2 <- read.csv("MeltCurve_1016_rep2.csv", header = T, sep = ",")
merged1534_rep1 <- read.csv("MeltCurve_1534_rep1.csv", header = T, sep = ",")
merged1534_rep2 <- read.csv("MeltCurve_1534_rep2.csv", header = T, sep = ",")

# Rename column Genotype in each df
colnames(merged1016_rep1)[4] <- "V1016I_rep1"
colnames(merged1016_rep2)[4] <- "V1016I_rep2"
colnames(merged1534_rep1)[4] <- "F1534C_rep1"
colnames(merged1534_rep2)[4] <- "F1534C_rep2"

# Left join merged data to kdrData
kdrData <- merge(x = kdrData, y = merged1016_rep1[, c("mosquito_id", "V1016I_rep1")]
                 , by = "mosquito_id", all.x = TRUE)
kdrData <- merge(x = kdrData, y = merged1016_rep2[, c("mosquito_id", "V1016I_rep2")]
                 , by = "mosquito_id", all.x = TRUE)
kdrData <- merge(x = kdrData, y = merged1534_rep1[, c("mosquito_id", "F1534C_rep1")]
                 , by = "mosquito_id", all.x = TRUE)
kdrData <- merge(x = kdrData, y = merged1534_rep2[, c("mosquito_id", "F1534C_rep2")]
                 , by = "mosquito_id", all.x = TRUE)

# 5 ################################################################################
### Add columns with verified replicated genotype or put error
kdrData$V1016I <- ifelse(kdrData$V1016I_rep1 == kdrData$V1016I_rep2
                       , as.character(kdrData$V1016I_rep1)
                       , "error")
kdrData$F1534C <- ifelse(kdrData$F1534C_rep1 == kdrData$F1534C_rep2
                         , as.character(kdrData$F1534C_rep1)
                         , "error")
head(kdrData)

# 6 ################################################################################
### Add column for Zone
# Load file containing zone information
exptZone <- read.csv("Location_Zone.csv", header = T, sep = ",")
# Rename location code to match title in kdrData
colnames(exptZone)[1] <- "Location_Code"

# Left join Zone information to kdrDate
kdrData <- merge(x = kdrData, y = exptZone
                          , by = "Location_Code"
                          , all.x = TRUE)


# 7 ################################################################################
### Create and save files with missing project_code
# Those files with no Zone (i.e. project code) specified
noZone <- kdrData[is.na(kdrData$project_code),]
#write.csv(noZone, "noZone.csv", row.names = FALSE)

# Those files with no Zone and known location codes
noZoneNoUnknowns <- noZone[noZone$Location_Code != "unknown",]
write.csv(noZoneNoUnknowns, "noZoneNoUnknowns.csv", row.names = FALSE)

###################################################################################
### This is the key code for this script
###################################################################################
# Remove unknown location codes from kdrData and save file
kdrData <- kdrData[kdrData$Location_Code != "unknown", ]
write.csv(kdrData
          ,"~/Dropbox/GouldLab/Project_Mosquito/Database/kdrData_all_reduced.csv"
          , row.names = F)


# 8 ################################################################################
### Convert melt curve output to readable genotype and haplotype
# Load function to convert melt curve output
source("R_Scripts/function_convertMergedMeltCurve.R")
# Run function for each column
kdrData$V1016I_converted <- convert1016(kdrData)
kdrData$F1534C_converted <- convert1534(kdrData)
kdrData$haplotype <- paste0(kdrData$V1016I_converted, kdrData$F1534C_converted)

### Save file
# Write to file
write.csv(kdrData
          ,"~/Dropbox/GouldLab/Project_Mosquito/Database/kdrData.csv"
          , row.names = F)


# 9 ################################################################################
### Parse data by year and/or month to use for allele frequency analysis
# Keep only those rows with project code

# Select based on year
mosq2000 <- sqldf("Select * from kdrData where newDate between '2000-01-01' and '2000-12-31'")
mosq2001 <- sqldf("Select * from kdrData where newDate between '2001-01-01' and '2001-12-31'")
mosq2002 <- sqldf("Select * from kdrData where newDate between '2002-01-01' and '2002-12-31'")
mosq2003 <- sqldf("Select * from kdrData where newDate between '2003-01-01' and '2003-12-31'")
mosq2004 <- sqldf("Select * from kdrData where newDate between '2004-01-01' and '2004-12-31'")
mosq2005 <- sqldf("Select * from kdrData where newDate between '2005-01-01' and '2005-12-31'")
mosq2006 <- sqldf("Select * from kdrData where newDate between '2006-01-01' and '2006-12-31'")
mosq2007 <- sqldf("Select * from kdrData where newDate between '2007-01-01' and '2007-12-31'")
mosq2008 <- sqldf("Select * from kdrData where newDate between '2008-01-01' and '2008-12-31'")
mosq2009 <- sqldf("Select * from kdrData where newDate between '2009-01-01' and '2009-12-31'")
mosq2010 <- sqldf("Select * from kdrData where newDate between '2010-01-01' and '2010-12-31'")
mosq2011 <- sqldf("Select * from kdrData where newDate between '2011-01-01' and '2011-12-31'")
mosq2012 <- sqldf("Select * from kdrData where newDate between '2012-01-01' and '2012-12-31'")
mosq2013 <- sqldf("Select * from kdrData where newDate between '2013-01-01' and '2013-12-31'")
mosq2014 <- sqldf("Select * from kdrData where newDate between '2014-01-01' and '2014-12-31'")
mosq2015 <- sqldf("Select * from kdrData where newDate between '2015-01-01' and '2015-12-31'")
mosq2016 <- sqldf("Select * from kdrData where newDate between '2016-01-01' and '2016-12-31'")

# Select based on month from all 2014 data
jan2014 <- sqldf("Select * from mosq2014 where newDate between '2014-01-01' and '2014-01-31'")
feb2014 <- sqldf("Select * from mosq2014 where newDate between '2014-02-01' and '2014-02-31'")
mar2014 <- sqldf("Select * from mosq2014 where newDate between '2014-03-01' and '2014-03-31'")
apr2014 <- sqldf("Select * from mosq2014 where newDate between '2014-04-01' and '2014-04-31'")
may2014 <- sqldf("Select * from mosq2014 where newDate between '2014-05-01' and '2014-05-31'")
jun2014 <- sqldf("Select * from mosq2014 where newDate between '2014-06-01' and '2014-06-31'")
jul2014 <- sqldf("Select * from mosq2014 where newDate between '2014-07-01' and '2014-07-31'")
aug2014 <- sqldf("Select * from mosq2014 where newDate between '2014-08-01' and '2014-08-31'")
sep2014 <- sqldf("Select * from mosq2014 where newDate between '2014-09-01' and '2014-09-31'")
oct2014 <- sqldf("Select * from mosq2014 where newDate between '2014-10-01' and '2014-10-31'")

# Select all from treatment zone
trt <- sqldf("Select * from kdrData where project_code is 'treatment'")
# Select based on 2014 month from treatment zone
jan2014t <- sqldf("Select * from trt where newDate between '2014-01-01' and '2014-01-31'")
feb2014t <- sqldf("Select * from trt where newDate between '2014-02-01' and '2014-02-31'")
mar2014t <- sqldf("Select * from trt where newDate between '2014-03-01' and '2014-03-31'")
apr2014t <- sqldf("Select * from trt where newDate between '2014-04-01' and '2014-04-31'")
may2014t <- sqldf("Select * from trt where newDate between '2014-05-01' and '2014-05-31'")
jun2014t <- sqldf("Select * from trt where newDate between '2014-06-01' and '2014-06-31'")
jul2014t <- sqldf("Select * from trt where newDate between '2014-07-01' and '2014-07-31'")
aug2014t <- sqldf("Select * from trt where newDate between '2014-08-01' and '2014-08-31'")
sep2014t <- sqldf("Select * from trt where newDate between '2014-09-01' and '2014-09-31'")
oct2014t <- sqldf("Select * from trt where newDate between '2014-10-01' and '2014-10-31'")

# Select all from buffer zone
buff <- sqldf("Select * from kdrData where project_code is 'buffer'")
# Select based on 2014 month from buffer zone
jan2014b <- sqldf("Select * from buff where newDate between '2014-01-01' and '2014-01-31'")
feb2014b <- sqldf("Select * from buff where newDate between '2014-02-01' and '2014-02-31'")
mar2014b <- sqldf("Select * from buff where newDate between '2014-03-01' and '2014-03-31'")
apr2014b <- sqldf("Select * from buff where newDate between '2014-04-01' and '2014-04-31'")
may2014b <- sqldf("Select * from buff where newDate between '2014-05-01' and '2014-05-31'")
jun2014b <- sqldf("Select * from buff where newDate between '2014-06-01' and '2014-06-31'")
jul2014b <- sqldf("Select * from buff where newDate between '2014-07-01' and '2014-07-31'")
aug2014b <- sqldf("Select * from buff where newDate between '2014-08-01' and '2014-08-31'")
sep2014b <- sqldf("Select * from buff where newDate between '2014-09-01' and '2014-09-31'")
oct2014b <- sqldf("Select * from buff where newDate between '2014-10-01' and '2014-10-31'")


# 10 ################################################################################
### Function to create dataframe of genotype counts, allele frequency of R, and +/- 95% confidence interval
### Run function across all years
### Create a dataframe with output from function

# Load functions
source("R_Scripts/function_mc.1016.R")
source("R_Scripts/function_mc.1534.R")
source("R_Scripts/function_mc.haps.R")


# 11 ################################################################################
### Run functions across all years and create dataframe with output
# Use mosqYears if figure out how to loop the function properly
# mosqYears <- c(mosq2000, mosq2001, mosq2002, mosq2003, mosq2004, mosq2005
#                , mosq2006, mosq2007, mosq2008, mosq2009, mosq2010, mosq2011
#                , mosq2012, mosq2013, mosq2014, mosq2015, mosq2016)

### Run Functions
# For 1016 locus at all years
m2000 = mc.1016(mosq2000)
m2001 = mc.1016(mosq2001)
m2002 = mc.1016(mosq2002)
m2003 = mc.1016(mosq2003)
m2004 = mc.1016(mosq2004)
m2005 = mc.1016(mosq2005)
m2006 = mc.1016(mosq2006)
m2007 = mc.1016(mosq2007)
m2008 = mc.1016(mosq2008)
m2009 = mc.1016(mosq2009)
m2010 = mc.1016(mosq2010)
m2011 = mc.1016(mosq2011)
m2012 = mc.1016(mosq2012)
m2013 = mc.1016(mosq2013)
m2014 = mc.1016(mosq2014)
m2015 = mc.1016(mosq2015)
m2016 = mc.1016(mosq2016)

# For 1534 locus at all years
s2000 = mc.1534(mosq2000)
s2001 = mc.1534(mosq2001)
s2002 = mc.1534(mosq2002)
s2003 = mc.1534(mosq2003)
s2004 = mc.1534(mosq2004)
s2005 = mc.1534(mosq2005)
s2006 = mc.1534(mosq2006)
s2007 = mc.1534(mosq2007)
s2008 = mc.1534(mosq2008)
s2009 = mc.1534(mosq2009)
s2010 = mc.1534(mosq2010)
s2011 = mc.1534(mosq2011)
s2012 = mc.1534(mosq2012)
s2013 = mc.1534(mosq2013)
s2014 = mc.1534(mosq2014)
s2015 = mc.1534(mosq2015)
s2016 = mc.1534(mosq2016)

# For haplotypes at all years
h2000 = mc.haps(mosq2000)
h2001 = mc.haps(mosq2001)
h2002 = mc.haps(mosq2002)
h2003 = mc.haps(mosq2003)
h2004 = mc.haps(mosq2004)
h2005 = mc.haps(mosq2005)
h2006 = mc.haps(mosq2006)
h2007 = mc.haps(mosq2007)
h2008 = mc.haps(mosq2008)
h2009 = mc.haps(mosq2009)
h2010 = mc.haps(mosq2010)
h2011 = mc.haps(mosq2011)
h2012 = mc.haps(mosq2012)
h2013 = mc.haps(mosq2013)
h2014 = mc.haps(mosq2014)
h2015 = mc.haps(mosq2015)
h2016 = mc.haps(mosq2016)

# For 1016 locus at all months in 2014
mJan <- mc.1016(jan2014)
mFeb <- mc.1016(feb2014)
mMar <- mc.1016(mar2014)
mApr <- mc.1016(apr2014)
mMay <- mc.1016(may2014)
mJun <- mc.1016(jun2014)
mJul <- mc.1016(jul2014)
mAug <- mc.1016(aug2014)
mSep <- mc.1016(sep2014)
mOct <- mc.1016(oct2014)

# For 1534 locus at all months in 2014
sJan <- mc.1534(jan2014)
sFeb <- mc.1534(feb2014)
sMar <- mc.1534(mar2014)
sApr <- mc.1534(apr2014)
sMay <- mc.1534(may2014)
sJun <- mc.1534(jun2014)
sJul <- mc.1534(jul2014)
sAug <- mc.1534(aug2014)
sSep <- mc.1534(sep2014)
sOct <- mc.1534(oct2014)

# For 1016 locus at all months in 2014 - treatment zone
tJan <- mc.1016(jan2014t)
tFeb <- mc.1016(feb2014t)
tMar <- mc.1016(mar2014t)
tApr <- mc.1016(apr2014t)
tMay <- mc.1016(may2014t)
tJun <- mc.1016(jun2014t)
tJul <- mc.1016(jul2014t)
tAug <- mc.1016(aug2014t)
tSep <- mc.1016(sep2014t)
tOct <- mc.1016(oct2014t)

# For 1016 locus at all months in 2014 - buffer zone
bJan <- mc.1016(jan2014b)
bFeb <- mc.1016(feb2014b)
bMar <- mc.1016(mar2014b)
bApr <- mc.1016(apr2014b)
bMay <- mc.1016(may2014b)
bJun <- mc.1016(jun2014b)
bJul <- mc.1016(jul2014b)
bAug <- mc.1016(aug2014b)
bSep <- mc.1016(sep2014b)
bOct <- mc.1016(oct2014b)


### Create dataframes
# Create list of years included in dataframe
year <- c(2000:2016)
# Create list of months included in dataframe - as numeric
month <- c(1:10)
# Create list of months included in dataframe - as objects
#month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October")

# For 1016 locus
df1016 <- rbind(m2000, m2001, m2002, m2003, m2004, m2005, m2006
               , m2007, m2008, m2009, m2010, m2011, m2012, m2013
               , m2014, m2015, m2016)
# Add year ID to rows in df rename
mc.1016.yr <- cbind(year, df1016)

# For 1534 locus
df1534 <- rbind(s2000, s2001, s2002, s2003, s2004, s2005, s2006
               , s2007, s2008, s2009, s2010, s2011, s2012, s2013
               , s2014, s2015, s2016)
# Add year ID to rows in df rename
mc.1534.yr <- cbind(year, df1534)

# For haplotypes
dfHaps <- rbind(h2000, h2001, h2002, h2003, h2004, h2005, h2006
                , h2007, h2008, h2009, h2010, h2011, h2012, h2013
                , h2014, h2015, h2016)
# Add year ID to rows in df rename
mc.haps.yr <- cbind(dfHaps, year)

# For 1016 locus at all months in 2014
df14.1016 <- rbind(mJan, mFeb, mMar, mApr, mMay, mJun, mJul, mAug, mSep, mOct)
# Add year ID to rows in df rename
mc.1016.mo <- cbind(month, df14.1016)

# For 1534 locus at all months in 2014
df14.1534 <- rbind(sJan, sFeb, sMar, sApr, sMay, sJun, sJul, sAug, sSep, sOct)
# Add year ID to rows in df rename
mc.1534.mo <- cbind(month, df14.1534)

# For 1016 locus at all months in 2014 - treatment zone
dftrt <- rbind(tJan, tFeb, tMar, tApr, tMay, tJun, tJul, tAug, tSep, tOct)
# Add year ID to rows in df rename
mc.1016.t <- cbind(month, dftrt)

# Create data frame from all runs of function
dfbuff <- rbind(bJan, bFeb, bMar, bApr, bMay, bJun, bJul, bAug, bSep, bOct)
# Add year ID to rows in df rename
mc.1016.b <- cbind(month, dfbuff)

# ### To view dataframes
# mc.1016.yr
# mc.1534.yr
# mc.haps.yr
# mc.1016.mo
# mc.1534.mo
# mc.1016.t
# mc.1016.b

### To save dataframes required for plots, selection coefficient, and other analyses
write.csv(mc.1016.yr, file = "/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/mc.1016.yr.csv", row.names = F)
write.csv(mc.1534.yr, file = "/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/mc.1534.yr.csv", row.names = F)
write.csv(mc.haps.yr, file = "/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/mc.haps.yr_reduced.csv", row.names = F)
write.csv(mc.1016.mo, file = "/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/mc.1016.mo.csv", row.names = F)
write.csv(mc.1534.mo, file = "/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/mc.1534.mo.csv", row.names = F)
write.csv(mc.1016.t, file = "/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/mc.1016.t.csv", row.names = F)
write.csv(mc.1016.b, file = "/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/mc.1016.b.csv", row.names = F)

# # 12 ################################################################################
# ### Plot Frequency of Resistance Alleles at two loci across years
# # Plot based off of dataframes mc.1016.yr and mc.1534.yr
# 
# source("R_Scripts/plot_kdrYears.R")
# kdrYears
# 
# # Write plot to pdf
# pdf(file = paste("figures/kdrYears/kdrYears_", Sys.Date(), ".pdf", sep = ""), 11, 8.5)
# print(kdrYears)
# dev.off()

  
# # 13 ################################################################################
# ### Plot Frequency of Resistance Allele at 1016 locus across year 2014
# # Plot based off of dataframe mc.1016.mo
# 
# source("R_Scripts/plot_kdrMonths.R")
# kdrMonths
# 
# # Write plot to pdf
# pdf(file = paste("figures/kdrMonths/kdrMonths_", Sys.Date(), ".pdf", sep = ""), 11, 8.5)
# print(kdrMonths)
# dev.off()


# 14 ################################################################################
### Plot Frequency of Resistance Allele at 1016 locus by zone
# Plot based off of dataframes mc.1016.t and mc.1016.b

source("R_Scripts/plot_kdrZones.R")
kdrZones

# Write plot to pdf
pdf(file = paste("figures/kdrZones/kdrZones_", Sys.Date(), ".pdf", sep = ""), 11, 8.5)
print(kdrZones)
dev.off





####################################################################################
####################################################################################
####################################################################################
# SCRATCH ##########################################################################
# b <- c(mosq2000, mosq2001)
# for (i in b){
#   print(head(i))
# }

### Find Rows in kdrData_all.csv where zone columns do not match
# This ignores NAs in the dataset

# no_match_zone_1016 <- sqldf("SELECT * 
#                             FROM kdrData_all
#                             WHERE Zone1016_1 != Zone1016_2
#                             ")
# 
# no_match_zone_1534 <- sqldf("SELECT * 
#                             FROM kdrData_all
#                             WHERE Zone1534_1 != Zone1534_2
#                             ")
# x ################################################################################
### Find rows where one replicate is equal to "error"
#not working - needs work

# errorFind <- sqldf("Select *
#                    From kdrData_all
#                    Where k.Rep1_1016 = error
#                    ")


# x ################################################################################
### Keep only rows where two columns match per locus
#not working - needs work

# match1016 <- sqldf ("SELECT *
#                     FROM kdrData_all as k
#                     WHERE k.'Rep1_1016' == k.'Rep2_1016'
#                     ")
# 
# match1534 <- sqldf ("SELECT *
#                     From kdrData_all as k
#                     WHERE k.'Rep1_1534' == k.'Rep2_1534'
#                     ")


# x ################################################################################
### Keep only those rows where two columns match
# tstZone <- isolGenosBoth[isolGenosBoth$Zone == isolGenosBoth$Zone1016,]
# head(tstZone)
# 
# tstZone2 <- match(isolGenosBoth$Zone,isolGenosBoth$Zone1016)
# 
# dim(isolLog)












