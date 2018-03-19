###
# This file is to search for specific tubes in the MathModelInvetory-2.csv file

################################################################################
### Prepare working environment
# clear working environment
rm(list = ls())

################################################################################
# set working directory
setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")

################################################################################
### Load masterfile with 2014 data
df <- read.csv("MathModelInvetory-2.csv", header = T, sep = ",")

################################################################################
### Convert current date to readable form
df$newDate <- as.character(as.Date(as.character(df$date), format = "%m/%d/%Y"))

################################################################################
# To look up unknown isolated samples in mathmodelinventory
x <- df[df$newDate >= as.Date("2013-03-31") & df$newDate <= as.Date("2013-03-01"),]
# OR
y <- sqldf("Select * from df where newDate between '2013-03-01' and '2013-03-31'")

# To subset based on partial location code
subset(df, grepl("PTA152", df$Location_code))






