# This script will create the input file (genepop format) to run NeEstimator

### Set up the working space ----------------------------
# clear working environment
rm(list = ls())

# Set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

### Create the input file ----------------------------
# Load data 
kdrData <- read.csv("kdrData_reduced.csv")
# Add "," to end of every mosquito_id - required for genepop
kdrData$mosquito_id <- paste0(kdrData$mosquito_id, ",")

# Reduce to mosquito id and genotypes
kdrData <- kdrData[, c("mosquito_id", "newDate", "V1016I_converted", "F1534C_converted")]

# Create columns with two digit numbers to represent S and R alleles
kdrData$V1016I_genepop <- ifelse(kdrData$V1016I_converted == "SS", "0101",
                                ifelse(kdrData$V1016I_converted == "SR", "0102",
                                       ifelse(kdrData$V1016I_converted == "RR", "0202",
                                              "0000")))
kdrData$V1016I_genepop[is.na(kdrData$V1016I_genepop)] <- "0000"

kdrData$F1534C_genepop <- ifelse(kdrData$F1534C_converted == "SS", "0101",
                                ifelse(kdrData$F1534C_converted == "SR", "0102",
                                       ifelse(kdrData$F1534C_converted == "RR", "0202",
                                              "0000")))
kdrData$F1534C_genepop[is.na(kdrData$F1534C_genepop)] <- "0000"


# Subset kdrDate based on year
kdrData_byYear <- split(kdrData, format(as.Date(kdrData$newDate), "%Y"))
kdrData_2000 <- kdrData_byYear$`2000`[, c("mosquito_id", "V1016I_genepop", "F1534C_genepop")]
kdrData_2014 <- kdrData_byYear$`2014`[, c("mosquito_id", "V1016I_genepop", "F1534C_genepop")]

# Create header lines for genepop file
line1 <- "This is kdr genotype data for Aedes aegypti from Iquitos, Peru"
line2 <- c("V1016I_genepop,", "F1534C_genepop,")
line3 <- "pop"

# Remove column and row names for exporting
library(MASS)
names(kdrData_2000) <- NULL
names(kdrData_2014) <- NULL

# Write txt files
sink("./genepop_2000.txt")
cat(line1)
cat("\n")
cat(line2)
cat("\n")
cat(line3)
cat("\n")
write.matrix(kdrData_2000)
sink()

sink("./genepop_2014.txt")
cat(line1)
cat("\n")
cat(line2)
cat("\n")
cat(line3)
cat("\n")
write.matrix(kdrData_2014)
sink()

sink("./genepop_temporal.txt")
cat(line1)
cat("\n")
cat(line2)
cat("\n")
cat(line3)
cat("\n")
write.matrix(kdrData_2000)
cat(line3)
cat("\n")
write.matrix(kdrData_2014)
sink()
