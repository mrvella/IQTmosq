# Create matrix of haplotype probabilities
# Create graph of Frequency of kdr Haplotypes
# Started: 8/1/17

# This matrix assumes that RS does not exist in the population
# Locus 1016 is always written first

### Prepare workspace ------------------------------------------------------------
# Clear working environment
# rm(list = ls())
# Set working directory
setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")

# Load required libraries
library(reshape2)

# Load mc.haps file
mid <- read.csv("mc.haps.mid.csv")


### Create matrix of haplotype probabilities ------------------------------------------------------------
genos <- c("SSSS", "SSSR", "SSRR", "SRSS", "SRSR", "SRRR", "RRSS", "RRSR", "RRRR")
haps <- c("SS", "SR", "RS", "RR")
HapProbs <- matrix(data = c(1,0,0,0,0.5,0.5,0,0,0,1,0,0,1,0,0,0,0.33,0.33,0,0.33,0,0.5,0,0.5,0,0,0,0,0,0,0,1,0,0,0,1)
       , nrow = 9, ncol = 4, byrow = T, dimnames = list(genos, haps))
# HapProbs

### Impute haplotype numbers ### mid ### ----------------------------------
# 2000
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[1,i] * HapProbs[i,]
  yr <- mid$year[1]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2000 <- colSums(df[, c(1:4)])
# Rename df
haps2000 <- df


# 2001
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[2,i] * HapProbs[i,]
  yr <- mid$year[2]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2001 <- colSums(df[, c(1:4)])
# Rename df
haps2001 <- df


# 2002
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[3,i] * HapProbs[i,]
  yr <- mid$year[3]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2002 <- colSums(df[, c(1:4)])
# Rename df
haps2002 <- df

# 2003
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[4,i] * HapProbs[i,]
  yr <- mid$year[4]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2003 <- colSums(df[, c(1:4)])
# Rename df
haps2003 <- df

# 2004
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[5,i] * HapProbs[i,]
  yr <- mid$year[5]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2004 <- colSums(df[, c(1:4)])
# Rename df
haps2004 <- df


# 2005
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[6,i] * HapProbs[i,]
  yr <- mid$year[6]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2005 <- colSums(df[, c(1:4)])
# Rename df
haps2005 <- df

# 2006
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[7,i] * HapProbs[i,]
  yr <- mid$year[7]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2006 <- colSums(df[, c(1:4)])
# Rename df
haps2006 <- df


# 2007
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[8,i] * HapProbs[i,]
  yr <- mid$year[8]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2007 <- colSums(df[, c(1:4)])
# Rename df
haps2007 <- df


# 2008
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[9,i] * HapProbs[i,]
  yr <- mid$year[9]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2008 <- colSums(df[, c(1:4)])
# Rename df
haps2008 <- df


# 2009
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[10,i] * HapProbs[i,]
  yr <- mid$year[10]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2009 <- colSums(df[, c(1:4)])
# Rename df
haps2009 <- df

# 2010
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[11,i] * HapProbs[i,]
  yr <- mid$year[11]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2010 <- colSums(df[, c(1:4)])
# Rename df
haps2010 <- df


# 2011
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[12,i] * HapProbs[i,]
  yr <- mid$year[12]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2011 <- colSums(df[, c(1:4)])
# Rename df
haps2011 <- df

# 2012
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[13,i] * HapProbs[i,]
  yr <- mid$year[13]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2012 <- colSums(df[, c(1:4)])
# Rename df
haps2012 <- df

# 2013
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[14,i] * HapProbs[i,]
  yr <- mid$year[14]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2013 <- colSums(df[, c(1:4)])
# Rename df
haps2013 <- df

# 2014
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[15,i] * HapProbs[i,]
  yr <- mid$year[15]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2014 <- colSums(df[, c(1:4)])
# Rename df
haps2014 <- df


# 2015
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[16,i] * HapProbs[i,]
  yr <- mid$year[16]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2015 <- colSums(df[, c(1:4)])
# Rename df
haps2015 <- df


# 2016
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[17,i] * HapProbs[i,]
  yr <- mid$year[17]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2016 <- colSums(df[, c(1:4)])
# Rename df
haps2016 <- df

# 2017
df <- data.frame()
# For each year, multiply the genotype number by the haplotype probability
for(i in 1:9){
  df_New <- mid[18,i] * HapProbs[i,]
  yr <- mid$year[18]
  df.yr <- c(df_New, yr)
  df <- rbind(df, df.yr)
  names(df) <- c("SS", "SR", "RS", "RR", "Year")
}
# df
# Create vector of haplotype sums for year
sum2017 <- colSums(df[, c(1:4)])
# Rename df
haps2017 <- df


### Create table of sums for each haplotype per year ------------------------------------------------------------
sumAll <- rbind(sum2000, sum2001, sum2002, sum2003, sum2004, sum2005, sum2006, sum2007
                , sum2008, sum2009, sum2010, sum2011, sum2012, sum2013, sum2014, sum2015, sum2016, sum2017)

row.names(sumAll) <- (c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008"
                        , "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"))
# sumAll


### Get the frequency of haps for each year ------------------------------------------------------------
freq2000 <- sum2000/mid$n[1]
freq2001 <- sum2001/mid$n[2]
freq2002 <- sum2002/mid$n[3]
freq2003 <- sum2003/mid$n[4]
freq2004 <- sum2004/mid$n[5]
freq2005 <- sum2005/mid$n[6]
freq2006 <- sum2006/mid$n[7]
freq2007 <- sum2007/mid$n[8]
freq2008 <- sum2008/mid$n[9]
freq2009 <- sum2009/mid$n[10]
freq2010 <- sum2010/mid$n[11]
freq2011 <- sum2011/mid$n[12]
freq2012 <- sum2012/mid$n[13]
freq2013 <- sum2013/mid$n[14]
freq2014 <- sum2014/mid$n[15]
freq2015 <- sum2015/mid$n[16]
freq2016 <- sum2016/mid$n[17]
freq2017 <- sum2017/mid$n[18]

### Create table for frequency of haps for each year ------------------------------------------------------------
freqAll <- rbind(freq2000, freq2001, freq2002, freq2003, freq2004, freq2005, freq2006, freq2007
                , freq2008, freq2009, freq2010, freq2011, freq2012, freq2013, freq2014, freq2015, freq2016, freq2017)

row.names(freqAll) <- (c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008"
                        , "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017"))
freqAll <- as.data.frame(freqAll)
year <- 2000:2017
freqAll <- cbind(freqAll, year)
# freqAll


# Save df to .csv file
write.csv(freqAll, "freqAll_mid.csv", row.names = FALSE)

### Calculate 95% Confidence Interval (not for ribbon CIs) ------------------------------------------------------------
## for column 1: SS
df <- data.frame()
for(i in 1:18){
  ci = 1.96 * (sqrt((freqAll[i,1]*(1-freqAll[i,1]))/(2*mid$n[i])))
  df = rbind(df, ci)
}
colnames(df) <- "CI_95"
CI_95.SS <- df
# CI_95.SS

## for column 2: SR
df <- data.frame()
for(i in 1:18){
  ci = 1.96 * (sqrt((freqAll[i,2]*(1-freqAll[i,2]))/(2*mid$n[i])))
  df = rbind(df, ci)
}
colnames(df) <- "CI_95"
CI_95.SR <- df
# CI_95.SR

## for column 3: RS
df <- data.frame()
for(i in 1:18){
  ci = 1.96 * (sqrt((freqAll[i,3]*(1-freqAll[i,3]))/(2*mid$n[i])))
  df = rbind(df, ci)
}
colnames(df) <- "CI_95"
CI_95.RS <- df
# CI_95.RS

## for column 4: RR
df <- data.frame()
for(i in 1:18){
  ci = 1.96 * (sqrt((freqAll[i,4]*(1-freqAll[i,4]))/(2*mid$n[i])))
  df = rbind(df, ci)
}
colnames(df) <- "CI_95"
CI_95.RR <- df
# CI_95.RR

CI_95 <- rbind(CI_95.SS, CI_95.SR, CI_95.RS, CI_95.RR)



### Converting from wide to long format ------------------------------------------------------------
freqAll_long <- melt(freqAll, id.vars="year", variable.name = "Haplotype", value.name = "Frequency")
freqAll_long <- cbind(freqAll_long, CI_95)
write.csv(freqAll_long, "freqAll_long_mid.csv", row.names = FALSE)

# Subset data based on haplotype
freqSS <- freqAll_long[freqAll_long$Haplotype=="SS", ]
freqSR <- freqAll_long[freqAll_long$Haplotype=="SR", ]
freqRS <- freqAll_long[freqAll_long$Haplotype=="RS", ]
freqRR <- freqAll_long[freqAll_long$Haplotype=="RR", ]

### Plot graph ------------------------------------------------------------
# create vector containing n for each year
samp.size <- as.numeric(mid$n)
source("./R_Scripts/IQTmosq/neighborhoodAnalysis/plot_kdrNeighborhoods.R")

# Add subtitle
kdrHaps <- kdrHaps +
  labs(subtitle = "Central")

# View plot
kdrHaps

# Write plot to png
ggsave(filename = paste0("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/figures/kdrHaps/kdrNeighborhoods_bars/mid_bars_", Sys.Date(), ".png"), width = 11, height = 8, dpi = 600, units = "in", device='png')



