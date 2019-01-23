# This script performs a "Repeated G-Test" on 2014 V1016I data
# Started 2 Aug 2017

# Prepare working environment
rm(list = ls())
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")
# Load required libraries
# RVAideMemoire manual at https://cran.r-project.org/web/packages/RVAideMemoire/index.html
library(RVAideMemoire)
library(dplyr)

# Load mc.1016.t and mc.1016.b data
mc.1016.t <- read.csv("mc.1016.t.csv")
mc.1016.b <- read.csv("mc.1016.b.csv")

# 3/19/18 - Subset mc.1016.t to remove March data because sample size is too low, currently n = 4
mc.1016.t <- mc.1016.t[-3,]
mc.1016.b <- mc.1016.b[-3,]

# Add column for treatment type
# for spray region
list1 <- 1:9
treatment <- rep("spray",length(list1))
mc.1016.t <- cbind(mc.1016.t, treatment)
mc.1016.t

# for buffer region
treatment <- rep("buffer", length(list1))
mc.1016.b <- cbind(mc.1016.b, treatment)
mc.1016.b

# Combine the datasets
mc.1016.2014 <- rbind(mc.1016.b, mc.1016.t)
mc.1016.2014

# Remove rows with NA
mc.1016.2014 <- mc.1016.2014[complete.cases(mc.1016.2014), ]

# Calculate number of R and S alleles from genotype counts
mc.1016.2014$R <- mc.1016.2014$SR + (2*mc.1016.2014$RR)
mc.1016.2014$S <- mc.1016.2014$SR + (2*mc.1016.2014$SS)
mc.1016.2014

############################################################
# Notes about choosing expected proportions for G.tests
# Options:
# 1 - equal proportions - 0.5 : 0.5 ratio of R:S alleles
# 2 - initial proportions - 0.765377 : 0.234623 ratio of R:S alleles (as of 2/9/18)
# 3 - 2014 mean proportions - 0.7612376 : 0.2387624 ratio of R:S alleles (as of 8/2/17)

# Choice:
# Go with option #2 because we care about how the frequencies 
# are changing over time relative to the beginning of the sampling period.
# This choice is in line three of the Fun.xx functions as ", p=c(meanFreqR, meanFreqS)"
# Code to automatically update initial proportions, these are also the expected proportions for 
# the pooled G-test
meanFreqR = (mc.1016.2014$freqR[1] + mc.1016.2014$freqR[9])/2
meanFreqS = 1 - meanFreqR

############################################################
# Individual G-tests
# Ho: Numbers within each expt fit expectations
# Ex: There is a specified proportion (i.e. 1:1 or 3:1) of R:S alleles within each month*trt group
# Use Bonferroni correction for this test

# Functions to calculate individual G's, df's, and p-values
Fun.G = function (Q){             
  G.test(x=c(Q["R"], Q["S"])
         # , p=c(0.765377, 0.234623)
         , p=c(meanFreqR, meanFreqS)
  )$statistic                    
}

Fun.df = function (Q){
  G.test(x=c(Q["R"], Q["S"])
         # , p=c(0.765377, 0.234623)
         , p=c(meanFreqR, meanFreqS)
  )$parameter
}

Fun.p = function (Q){
  G.test(x=c(Q["R"], Q["S"])
         # , p=c(0.765377, 0.234623)
        , p=c(meanFreqR, meanFreqS)
  )$p.value
}


# Calculate proportion of R allele
mc.1016.2014 =
  mutate(mc.1016.2014,
         Prop.R = R / (R + S),                         
         G =       apply(mc.1016.2014[c("R", "S")], 1, Fun.G),
         df =      apply(mc.1016.2014[c("R", "S")], 1, Fun.df),
         p.Value = apply(mc.1016.2014[c("R", "S")], 1, Fun.p)
  )
# View data
mc.1016.2014

############################################################
# Heterogeneity G-test
# Ho: Relative proportions are equal across different experiments
# Ex: The proportion of R alleles is the same in different month*trt groups

# Create a data matrix to run G-test for heterogeneity
Data.matrix = as.matrix(mc.1016.2014[c("R", "S")])      
Data.matrix                                     

# Heterogeneity
G.test(Data.matrix) # Report this value                

############################################################
# Pooled G-test
# Ho: Pooled data fit expectations
# Ex: The number of R and S alles summed across month*trt group is equal to the expected proportions

# Set up data for pooled G-test
Total.R = sum(mc.1016.2014$R)                           
Total.S = sum(mc.1016.2014$S)                           

observed = c(Total.R, Total.S)
expected = c(meanFreqR, meanFreqS)

G.test(x=observed, p=expected)

############################################################
# Total G-test
# Ho: Data from individual experiments fit expectations

# Set up data for total G-test
Total.G  = sum(mc.1016.2014$G)                          
Total.df = sum(mc.1016.2014$df)

# Run 
Total.G                                       
Total.df

pchisq(Total.G,
       df= Total.df,
       lower.tail=FALSE)






