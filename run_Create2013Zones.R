# Prepare working environment ---------------------------------------------
# set working directory
setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")

# clear working environment
rm(list = ls())

# load libraries
library(genetics)
library(sqldf)


# load data ---------------------------------------------
# pltmap <- read.csv("~/Dropbox/GouldLab/Project_Mosquito/NGS_libraries/jfbaltz_lib7_Aae_RadSeq_2013/2013_findGroup.csv")
# pltmap <- read.csv("~/Dropbox/GouldLab/Project_Mosquito/QGIS_Files/Routput/2013samps_withConcsGPS.csv")
pltmap <- read.csv("~/Dropbox/GouldLab/Project_Mosquito/NGS_libraries/jfbaltz_lib7_Aae_RadSeq_2013/lib7_samples_2013.csv")
head(pltmap)

loc_zone <- read.csv("MeltCurve_AnalysisProgramFiles/MeltCurve_410/Location_Zone.csv")
head(loc_zone)

# Create new column with only first 6 characters from Location_Code
pltmap$LOC_CODE <- substr(pltmap$location_code, 1, 6)

# Left join loc_zone onto df
df <- merge(x = pltmap, y = loc_zone, by.x = "LOC_CODE", by.y = "location_code", all.x = TRUE)

# write.csv(df, "~/Dropbox/GouldLab/Project_Mosquito/NGS_libraries/jfbaltz_lib7_Aae_RadSeq_2013/2013zones.csv", row.names = FALSE)
# write.csv(df, "temp2013zones.csv")
write.csv(df, "~/Dropbox/GouldLab/Project_Mosquito/NGS_libraries/jfbaltz_lib7_Aae_RadSeq_2013/2013zones_2019-02-03.csv", row.names = FALSE)