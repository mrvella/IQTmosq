##################### 
# This file is to create a subset of the masterdec that 
# contains all of the houses from where we have samples
# Started 30 Aug 2017
##################### 

### clear working environment
rm(list = ls())

### set working directory to import kdr file
setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")

# kdrData_reduced_[date].csv is the file that contains all samples with kdr genotypes and known location codes
kdr <- read.csv("kdrData_reduced_2018-03-12.csv", header = T, sep = ",")

### change working directory to QGIS 
setwd("~/Dropbox/GouldLab/Project_Mosquito/QGIS_Files/Routput")

### Load files
# masterdec.csv is the master list of houses from Amy's group
masterdec <- read.csv("masterdec.csv", header = T, sep = ",")

# Merge the two dataframes together (aka - inner join)
masterdec_kdr <- merge(masterdec, kdr, by.x = "LOC_CODE", by.y = "Location_Code")

# Remove rows with NA and errors in haplotype column
keep <- c("SSSS", "RRRR", "SSRR", "SSSR", "SRRR", "SRSS", "SRSR", "RRSS")
masterdec_kdr_all <- masterdec_kdr[masterdec_kdr$haplotype %in% keep,]

# Create file to import into QGIS
# write.csv(masterdec_kdr_all, "masterdec_kdr_2018-03-12.csv", row.names = F)
write.csv(masterdec_kdr_all, paste0("masterdec_kdr", Sys.Date(), ".csv"), row.names = F)


##################################################################################
# Subset masterdec_kdr_all$newDate on year
masterdec_2000 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2000,]
masterdec_2001 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2001,]
masterdec_2002 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2002,]
masterdec_2003 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2003,]
masterdec_2004 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2004,]
masterdec_2005 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2005,]
masterdec_2006 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2006,]
masterdec_2007 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2007,]
masterdec_2008 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2008,]
masterdec_2009 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2009,]
masterdec_2010 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2010,]
masterdec_2011 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2011,]
masterdec_2012 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2012,]
masterdec_2013 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2013,]
masterdec_2014 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2014,]
masterdec_2015 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2015,]
masterdec_2016 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2016,]
masterdec_2017 <- masterdec_kdr_all[format.Date(masterdec_kdr_all$newDate, "%Y")==2017,]

# Create files for each masterdec_[year] data frame
write.csv(masterdec_2000, "masterdec_2000.csv", row.names = F)
write.csv(masterdec_2001, "masterdec_2001.csv", row.names = F)
write.csv(masterdec_2002, "masterdec_2002.csv", row.names = F)
write.csv(masterdec_2003, "masterdec_2003.csv", row.names = F)
write.csv(masterdec_2004, "masterdec_2004.csv", row.names = F)
write.csv(masterdec_2005, "masterdec_2005.csv", row.names = F)
write.csv(masterdec_2006, "masterdec_2006.csv", row.names = F)
write.csv(masterdec_2007, "masterdec_2007.csv", row.names = F)
write.csv(masterdec_2008, "masterdec_2008.csv", row.names = F)
write.csv(masterdec_2009, "masterdec_2009.csv", row.names = F)
write.csv(masterdec_2010, "masterdec_2010.csv", row.names = F)
write.csv(masterdec_2011, "masterdec_2011.csv", row.names = F)
write.csv(masterdec_2012, "masterdec_2012.csv", row.names = F)
write.csv(masterdec_2013, "masterdec_2013.csv", row.names = F)
write.csv(masterdec_2014, "masterdec_2014.csv", row.names = F)
write.csv(masterdec_2015, "masterdec_2015.csv", row.names = F)
write.csv(masterdec_2016, "masterdec_2016.csv", row.names = F)
write.csv(masterdec_2017, "masterdec_2017.csv", row.names = F)



