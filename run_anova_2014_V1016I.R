# This script performs anova on 2014 V1016I data

# Prepare working environment
rm(list = ls())
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")
# options() line below prevents R from stalling while loading sqldf library
options(gsubfn.engine = "R")
library(sqldf)

# Load mosq2014.csv data - originally save from "R_Scripts/run_MeltCurve_analysis_reduced.R"
mosq2014 <- read.csv("~/Dropbox/GouldLab/Project_Mosquito/Database/mosq2014.csv")
head(mosq2014)

# Count R alleles from 1016 locus
mosq2014 <- sqldf("select *,
                  CASE
                  WHEN Rep1_1016 = 'SS' THEN '0'
                  WHEN Rep1_1016 = 'SR' THEN '1'
                  WHEN Rep1_1016 = 'RR' THEN '2'
                  ELSE 'error'
                  END as 'countR1016'
                  FROM mosq2014
                  ")
head(mosq2014)

mosq2014 <- sqldf("select *,
                  CASE
                  WHEN newDate between '2014-01-01' and '2014-01-31' THEN 'Jan'
                  WHEN newDate between '2014-02-01' and '2014-02-31' THEN 'Feb'
                  WHEN newDate between '2014-04-01' and '2014-04-31' THEN 'Apr'
                  WHEN newDate between '2014-05-01' and '2014-05-31' THEN 'May'
                  WHEN newDate between '2014-06-01' and '2014-06-31' THEN 'Jun'
                  WHEN newDate between '2014-07-01' and '2014-07-31' THEN 'Jul'
                  WHEN newDate between '2014-08-01' and '2014-08-31' THEN 'Aug'
                  WHEN newDate between '2014-10-01' and '2014-10-31' THEN 'Oct'
                  ELSE 'error'
                  END as 'month'
                  FROM mosq2014
                  ")
head(mosq2014)

# Remove rows containing errors from mosq2014$countR1016
mosq2014 <- mosq2014[!(mosq2014$countR1016=="error"),]
unique(mosq2014$countR1016)
# Remove rows containing "other" or "undefined" location codes
mosq2014 <- mosq2014[!(mosq2014$Zone1016_1=="other"),]
mosq2014 <- mosq2014[!(mosq2014$Zone1016_1=="undefined"),]
unique(mosq2014$Zone1016_1)

# Verify changes
head(mosq2014)
tail(mosq2014)

attach(mosq2014)
anova(lm(countR1016 ~ Zone1016_1))
anova(lm(countR1016 ~ month))
anova(lm(countR1016 ~ Zone1016_1 * month))





freq <- sum(as.numeric(mosq2014$countR1016)) / (2*1713)
freq








