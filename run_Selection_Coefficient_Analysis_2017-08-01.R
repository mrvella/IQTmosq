# Selection Foefficient Analysis
# Started: 7/31/17
# Updated: 8/1/17

### clear working environment
#rm(list = ls())

### Set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

### Source required functions
#function for selection coefficient assuming dominance of p and selection for p
source("R_Scripts/function_Dom.Sel.Coef.for.p.R")

#function for selection coefficient assuming dominance of p and selection for q
source("R_Scripts/function_Dom.Sel.Coef.for.q.R")

#function for selection coefficint assuming no dominance of p and selection for p
source("R_Scripts/function_NoDom.Sel.Coef.r")

# set number of generations per year
g = 12 

# Note for this analysis I am assuming that S-allele = p, R-allele = q

################
### For 1534
################

# Open csv file
mc.1534.yr <- read.csv("mc.1534.yr_reduced.csv")

# remove rows with na
mc.1534.yr <- mc.1534.yr[complete.cases(mc.1534.yr), ]

# Rename freqR column as q.1
mc.1534.yr$q.1 <- mc.1534.yr$freqR

# add column to df for q2
# note, result for last row of q.2 should = NA
for(i in 1:(length(mc.1534.yr$q.1 - 1))){
  mc.1534.yr$q.2[i] <- mc.1534.yr$q.1[i+1]
}

# View df to verify changes
mc.1534.yr

# Run functions
#mc.1534.yr$Dom.Sel.Coef.for.p <- Dom.Sel.Coef.for.p(mc.1534.yr$q.1, mc.1534.yr$q.2, g)
mc.1534.yr$Dom.Sel.Coef.for.q <- Dom.Sel.Coef.for.q(mc.1534.yr$q.1, mc.1534.yr$q.2, g)
mc.1534.yr$NoDom.Sel.Coef <- NoDom.Sel.Coef(mc.1534.yr$q.1, mc.1534.yr$q.2, g)
mc.1534.yr

################
### For 1016
################

# Open csv file
mc.1016.yr <- read.csv("mc.1016.yr_reduced.csv")

# remove rows with na
mc.1016.yr <- mc.1016.yr[complete.cases(mc.1016.yr), ]

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
#mc.1016.yr$Dom.Sel.Coef.for.p <- Dom.Sel.Coef.for.p(mc.1016.yr$q.1, mc.1016.yr$q.2, g)
mc.1016.yr$Dom.Sel.Coef.for.q <- Dom.Sel.Coef.for.q(mc.1016.yr$q.1, mc.1016.yr$q.2, g)
mc.1016.yr$NoDom.Sel.Coef <- NoDom.Sel.Coef(mc.1016.yr$q.1, mc.1016.yr$q.2, g)
mc.1016.yr

# Write csv file to save raw data
write.csv(mc.1016.yr, "mc.1016.yr_withSelCoeff.csv", row.names = F)
