### Selection Coefficient calculations
### File Started: 7/20/2017
### Last Update: 7/27/2017

# This code assumes the following dataframes from MeltCurve_analysis.R
# mc.1016.yr
# mc.1534.yr
# mc.1016.mo
# mc.1534.mo
# mc.1016.t
# mc.1016.b
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")
source("R_Scripts/run_MeltCurve_analysis.R")


########################################################################################
# Subset to get genotype counts
# locus 1534
RRfreq <- mc.1534.yr$RR/mc.1534.yr$n
SRfreq <- mc.1534.yr$SR/mc.1534.yr$n
SSfreq <- mc.1534.yr$SS/mc.1534.yr$n
# Create dataframe with genotype frequencies
x1534 <- cbind(mc.1534.yr, RRfreq, SRfreq, SSfreq)
# Remove rows with NaNs
x1534 <- x1534[complete.cases(x1534), ]
# View dataframe
x1534

# locus 1016
RRfreq <- mc.1016.yr$RR/mc.1534.yr$n
SRfreq <- mc.1016.yr$SR/mc.1534.yr$n
SSfreq <- mc.1016.yr$SS/mc.1534.yr$n
# Create dataframe with genotype frequencies
x1016 <- cbind(mc.1016.yr, RRfreq, SRfreq, SSfreq)
# Remove rows with NaNs
x1016 <- x1016[complete.cases(x1016), ]
# View dataframe
x1016



########################################################################################
# Source function to calculate selection coefficient
source("R_Scripts/function_selcof.R")

# Run function on both loci
s1534 <- selcof(x1534); s1534
s1016 <- selcof(x1016); s1016

# Save output as xlsx file
library(xlsx)
write.xlsx(s1534, "s1534.xlsx")
write.xlsx(s1016, "s1016.xlsx")

########################################################################################
# # Rough plot of results
# plot(s1016$Year, s1016$`Selection Coefficient`)
# plot(s1534$Year, s1534$`Selection Coefficient`)

########################################################################################
# Calculate Selection Coeff. for before and after selection event
# Subset dataframes
# Locus 1534
keeprow1 <- c(1, 11)
y1534 <- x1534[keeprow1, ]
# Calculate s
sTot1534 <- selcof(y1534); sTot1534

# Locus 1016
keeprow2 <- c(1, 15)
y1016 <- x1016[keeprow2, ]
# Calculate s
sTot1016 <- selcof(y1016); sTot1016

########################################################################################
# Plot results
kdrYears <- kdrYears +
  # Need to comment out labs() in run_MeltCurve_analysis.R before running this code
  labs(x = "Year", y = "Frequency", title = "Selection Coefficient of Resistance Allele at Two Loci") +
  # Add labels for 1534 S values
  annotate("text", x=2000.5, y=.1, label = round(s1534$`Selection Coefficient`[1], digits=2)
           , color = "dark green", fontface = 7) +
  annotate("text", x=2001.0, y=.3, label = round(s1534$`Selection Coefficient`[2], digits=2)
           , color = "dark green", fontface = 7) +
  annotate("text", x=2002.5, y=.8, label = round(s1534$`Selection Coefficient`[3], digits=2)
           , color = "dark green", fontface = 7) +
  annotate("text", x=2003.5, y=.9, label = round(s1534$`Selection Coefficient`[4], digits=2)
           , color = "dark green", fontface = 7) +
  annotate("text", x=2004.5, y=.95, label = round(s1534$`Selection Coefficient`[5], digits=2)
           , color = "dark green", fontface = 7) +
  annotate("text", x=2007.5, y=1, label = round(s1534$`Selection Coefficient`[8], digits=2)
           , color = "dark green", fontface = 7) +
  annotate("text", x=2008.5, y=.95, label = round(s1534$`Selection Coefficient`[9], digits=2)
           , color = "dark green", fontface = 7) +
  annotate("text", x=2009.5, y=1, label = round(s1534$`Selection Coefficient`[10], digits=2)
           , color = "dark green", fontface = 7) +
  # Add labels for 1016 S values
  annotate("text", x=2010.5, y=.2, label = round(s1016$`Selection Coefficient`[11], digits=2)
           , color = "blue", fontface = 7) +
  annotate("text", x=2011.0, y=.3, label = round(s1016$`Selection Coefficient`[12], digits=2)
           , color = "blue", fontface = 7) +
  annotate("text", x=2012.5, y=.6, label = round(s1016$`Selection Coefficient`[13], digits=2)
           , color = "blue", fontface = 7) +
  annotate("text", x=2015.0, y=.75, label = round(s1016$`Selection Coefficient`[14], digits=2)
           , color = "blue", fontface = 7) 
kdrYears

















