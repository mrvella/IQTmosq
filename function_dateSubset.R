### Function to select samples based on date criteria
# Author: Jennifer Baltzegar - jen_baltzegar@ncsu.edu
# Started: 14 Sep 2017
# Last Edited: 14 Sep 2017


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
### The function called dateSubset
dateSubset <- function(startDate, endDate){
  df2 <- df[df$newDate >= startDate & df$newDate <= endDate,]
  n <- sum(df2$intra, na.rm = T)
  return(n)
}

################################################################################
### Run function for each year of collections
collected2013 <- dateSubset(as.Date("2013-01-01"), as.Date("2013-12-31"))
collected2014 <- dateSubset(as.Date("2014-01-01"), as.Date("2014-12-31"))

### Print output of function for each year
collected2013
collected2014
