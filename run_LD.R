# Prepare working environment ---------------------------------------------
# set working directory
setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")

# clear working environment
rm(list = ls())

# load libraries
library(genetics)


# load data ---------------------------------------------
kdr <- read.csv("~/Dropbox/GouldLab/Project_Mosquito/Database/kdrData_reduced.csv")
head(kdr)


# create functions ---------------------------------------------
# change converted columns into format readable for 'genetics' package
LDconvert1016 <- function(kdr){
  x <- ifelse(kdr$V1016I_converted == "RR", "R/R"
              , ifelse(kdr$V1016I_converted == "SR", "S/R"
                       , ifelse(kdr$V1016I_converted == "SS", "S/S"
                                , "NA")))
  return(x)
}

LDconvert1534 <- function(kdr){
  x <- ifelse(kdr$F1534C_converted == "RR", "R/R"
              , ifelse(kdr$F1534C_converted == "SR", "S/R"
                       , ifelse(kdr$F1534C_converted == "SS", "S/S"
                                , "NA")))
  return(x)
}

# Create new genotype columns in readable form for 'genetics' package ---------------------------------------------
kdr$V1016I_LD <- LDconvert1016(kdr)
kdr$F1534C_LD <- LDconvert1534(kdr)

head(kdr)

# Calculate pairwise LD between SNPs ---------------------------------------------
# ID genotypes for each locus
v1016 <- genotype(kdr$V1016I_LD)
f1534 <- genotype(kdr$F1534C_LD)

# Run LD function
LD(v1016,f1534)

  