# Selection Coefficient Analysis
# Started: 7/31/17
# Updated: 8/1/17

### clear working environment
#rm(list = ls())

### Set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

### Source required functions
#function for selection coefficient assuming dominance of p and selection for p
source("R_Scripts/IQTmosq/function_Dom.Sel.Coef.for.p.R")

#function for selection coefficient assuming dominance of p and selection for q
source("R_Scripts/IQTmosq/function_Dom.Sel.Coef.for.q.R")

#function for selection coefficint assuming no dominance of p and selection for p
source("R_Scripts/IQTmosq/function_NoDom.Sel.Coef.r")

# set number of generations per year
g = 12

# Note for this analysis I am assuming that S-allele = p, R-allele = q

################
### For 1534
################

# Open csv file
mc.1534.yr <- read.csv("mc.1534.yr_reduced.csv")

# remove rows with na
# As of 3/19/18 this is no longer necessary because all years (2000 - 2017) have data
# mc.1534.yr <- mc.1534.yr[complete.cases(mc.1534.yr), ]

# Copy and rename freqR column as q.1
mc.1534.yr$q.1 <- mc.1534.yr$freqR

# add column to df for q2
# note, result for last row of q.2 should = NA
for(i in 1:(length(mc.1534.yr$q.1 - 1))){
  mc.1534.yr$q.2[i] <- mc.1534.yr$q.1[i+1]
}
# View df to verify changes
mc.1534.yr

# Run functions
# This is the functions to calculate selection coefficient assuming dominance of p and selection for p
mc.1534.yr$Dom.Sel.Coef.for.p <- Dom.Sel.Coef.for.p(mc.1534.yr$q.1, mc.1534.yr$q.2, g)
# This is the function to calculate selection coefficient assuming dominance of p and selection for q
mc.1534.yr$Dom.Sel.Coef.for.q <- Dom.Sel.Coef.for.q(mc.1534.yr$q.1, mc.1534.yr$q.2, g)
# This is the function to calculate selection coefficient assuming no dominance of either allele and selection for p
mc.1534.yr$NoDom.Sel.Coef <- NoDom.Sel.Coef(mc.1534.yr$q.1, mc.1534.yr$q.2, g)
# View df to verify changes
mc.1534.yr

# Write csv file to save raw data
write.csv(mc.1534.yr, "mc.1534.yr_withSelCoeff.csv", row.names = F)


################
### For 1016
################

# Open csv file
mc.1016.yr <- read.csv("mc.1016.yr_reduced.csv")

# remove rows with na
# As of 3/19/18 this is no longer necessary because all years (2000 - 2017) have data
# mc.1016.yr <- mc.1016.yr[complete.cases(mc.1016.yr), ]

# Rename freqR column as q.1
mc.1016.yr$q.1 <- mc.1016.yr$freqR

# add column to df for q.2
# note, result for last row of q.2 should = NA
for(i in 1:(length(mc.1016.yr$q.1 - 1))){
  mc.1016.yr$q.2[i] <- mc.1016.yr$q.1[i+1]
}
# View df to verify changes
mc.1016.yr

# Run functions
# This is the functions to calculate selection coefficient assuming dominance of p and selection for p
mc.1016.yr$Dom.Sel.Coef.for.p <- Dom.Sel.Coef.for.p(mc.1016.yr$q.1, mc.1016.yr$q.2, g)
# This is the function to calculate selection coefficient assuming dominance of p and selection for q
mc.1016.yr$Dom.Sel.Coef.for.q <- Dom.Sel.Coef.for.q(mc.1016.yr$q.1, mc.1016.yr$q.2, g)
# This is the function to calculate selection coefficient assuming no dominance of either allele and selection for p
mc.1016.yr$NoDom.Sel.Coef <- NoDom.Sel.Coef(mc.1016.yr$q.1, mc.1016.yr$q.2, g)
# View df to verify changes
mc.1016.yr

# Write csv file to save raw data
write.csv(mc.1016.yr, "mc.1016.yr_withSelCoeff.csv", row.names = F)
