# Selection Coefficient Analysis without 2014 experimental data
# Started: 7/31/17

# ### clear working environment
# rm(list = ls())

### Set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

### Source required functions
# function for selection coefficient assuming partial dominance of p and selection for p
source("R_Scripts/IQTmosq/function_Part.Dom.Sel.Coef.for.p.R")

# function for selection coefficient assuming partial dominance of q and selection for q
source("R_Scripts/IQTmosq/function_Part.Dom.Sel.Coef.for.q.R")

# function for selection coefficient assuming dominance of p and selection for p
source("R_Scripts/IQTmosq/function_Dom.Sel.Coef.for.p.R")

# function for selection coefficient assuming dominance of p and selection for q
source("R_Scripts/IQTmosq/function_Dom.Sel.Coef.for.q.R")

# function for selection coefficint assuming no dominance of p and selection for p
source("R_Scripts/IQTmosq/function_NoDom.Sel.Coef.r")

# set number of generations per year
g = 5

# set amount of dominance (h) assumed for allele in Partial dominance calculations (default = 0.5)
h = 0.5

# Note for this analysis I am assuming that S-allele = p, R-allele = q

################
### For 1016
################

# Open csv file
mc.1016.t <- read.csv("mc.1016.t_reduced.csv")
mc.1016.b <- read.csv("mc.1016.b_reduced.csv")

# # keep only the first and last rows
# mc.1016.t <- mc.1016.t[c(1, nrow(mc.1016.t)),] 
# mc.1016.b <- mc.1016.b[c(1, nrow(mc.1016.b)),] 

# keep only the first and 5th
mc.1016.t <- mc.1016.t[c(1, g),] 
mc.1016.b <- mc.1016.b[c(1, g),] 

# Copy and rename freqR column as q.1
mc.1016.t$q.1 <- mc.1016.t$freqR
mc.1016.b$q.1 <- mc.1016.b$freqR

# add column to df for q2
# note, result for last row of q.2 should = NA
for(i in 1:(length(mc.1016.t$q.1 - 1))){
  mc.1016.t$q.2[i] <- mc.1016.t$q.1[i+1]
}

for(i in 1:(length(mc.1016.b$q.1 - 1))){
  mc.1016.b$q.2[i] <- mc.1016.b$q.1[i+1]
}
# View df to verify changes
# mc.1534.yr
mc.1016.t
mc.1016.b

# Run functions
# # This is the function to calculate selection coefficient assuming partial dominance of p and selection for p
# mc.1016.t$Part.Dom.Sel.Coef.for.p <- Part.Dom.Sel.Coef.for.p(mc.1016.t$q.1, mc.1016.t$q.2, g, h)
# mc.1016.b$Part.Dom.Sel.Coef.for.p <- Part.Dom.Sel.Coef.for.p(mc.1016.b$q.1, mc.1016.b$q.2, g, h)
# This is the function to calculate selection coefficient assuming partial dominance of q and selection for q
mc.1016.t$Part.Dom.Sel.Coef.for.q <- Part.Dom.Sel.Coef.for.q(mc.1016.t$q.1, mc.1016.t$q.2, g, h) 
mc.1016.b$Part.Dom.Sel.Coef.for.q <- Part.Dom.Sel.Coef.for.q(mc.1016.b$q.1, mc.1016.b$q.2, g, h) 
# # This is the function to calculate selection coefficient assuming dominance of p and selection for p
# mc.1016.t$Dom.Sel.Coef.for.p <- Dom.Sel.Coef.for.p(mc.1016.t$q.1, mc.1016.t$q.2, g)
# mc.1016.b$Dom.Sel.Coef.for.p <- Dom.Sel.Coef.for.p(mc.1016.b$q.1, mc.1016.b$q.2, g)
# This is the function to calculate selection coefficient assuming dominance of p and selection for q
mc.1016.t$Dom.Sel.Coef.for.q <- Dom.Sel.Coef.for.q(mc.1016.t$q.1, mc.1016.t$q.2, g)
mc.1016.b$Dom.Sel.Coef.for.q <- Dom.Sel.Coef.for.q(mc.1016.b$q.1, mc.1016.b$q.2, g)
# # This is the function to calculate selection coefficient assuming no dominance of either allele and selection for p
# mc.1016.t$NoDom.Sel.Coef <- NoDom.Sel.Coef(mc.1016.t$q.1, mc.1016.t$q.2, g)
# mc.1016.b$NoDom.Sel.Coef <- NoDom.Sel.Coef(mc.1016.b$q.1, mc.1016.b$q.2, g)
# View df to verify changes
mc.1016.t
mc.1016.b

# Write csv file to save raw data
write.csv(mc.1016.t, "mc.1016.t14_withSelCoeff.csv", row.names = F)
write.csv(mc.1016.b, "mc.1016.b14_withSelCoeff.csv", row.names = F)
