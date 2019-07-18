# Get genotypes at 410 for some samples that wouldn't run on jungkoo's script

# Prepare working environment ---------------------------------------------
# set working directory
setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")

# clear working environment
rm(list = ls())

############################################
# MeltCurve_410_rep1
############################################

# Load data --------------------------------
pltmap <- read.csv("../Data_Mosquito/MeltCurve/MeltCurve_results/MeltCurve_410_rep1/MeltCurve_5/Analysis/PlateMap.csv")
raw_mc <- read.csv("../Data_Mosquito/MeltCurve/MeltCurve_results/MeltCurve_410_rep1/MeltCurve_5/Analysis/output_Raw_MeltingCurve.csv", header = FALSE)
loc_zone <- read.csv("MeltCurve_AnalysisProgramFiles/MeltCurve_410/Location_Zone.csv")

# Manipulate Data into correct format ----------------------
# Left join raw_mc onto pltmap
df <- merge(x = pltmap, y = raw_mc, by.x = "meltcurve_well_id", by.y = "V1" , all.x = TRUE)

# Create new column with only first 6 characters from Location_Code
df$LOC_CODE <- substr(df$Location_Code, 1, 6)

# Left join loc_zone onto df
df <- merge(x = df, y = loc_zone, by.x = "LOC_CODE", by.y = "location_code", all.x = TRUE)

# Remove LOC_CODE column
df <- df[,2:6]

# Rename columns
colnames(df)[4:5] <- c("Genotype", "Zone")

# Save new dataframe -----------------------
write.csv(df
          , "../Data_Mosquito/MeltCurve/MeltCurve_results/MeltCurve_410_rep1/MeltCurve_5/Analysis/PlateMap_Genotype_Zone.csv"
          , row.names = FALSE)

############################################
# MeltCurve_410_rep2
############################################

# Load data --------------------------------
pltmap <- read.csv("../Data_Mosquito/MeltCurve/MeltCurve_results/MeltCurve_410_rep2/MeltCurve_5/Analysis/PlateMap.csv")
raw_mc <- read.csv("../Data_Mosquito/MeltCurve/MeltCurve_results/MeltCurve_410_rep2/MeltCurve_5/Analysis/output_Raw_MeltingCurve.csv", header = FALSE)
loc_zone <- read.csv("MeltCurve_AnalysisProgramFiles/MeltCurve_410/Location_Zone.csv")

# Manipulate Data into correct format ----------------------
# Left join raw_mc onto pltmap
df <- merge(x = pltmap, y = raw_mc, by.x = "meltcurve_well_id", by.y = "V1" , all.x = TRUE)

# Create new column with only first 6 characters from Location_Code
df$LOC_CODE <- substr(df$Location_Code, 1, 6)

# Left join loc_zone onto df
df <- merge(x = df, y = loc_zone, by.x = "LOC_CODE", by.y = "location_code", all.x = TRUE)

# Remove LOC_CODE column
df <- df[,2:6]

# Rename columns
colnames(df)[4:5] <- c("Genotype", "Zone")

# Save new dataframe -----------------------
write.csv(df
          , "../Data_Mosquito/MeltCurve/MeltCurve_results/MeltCurve_410_rep2/MeltCurve_5/Analysis/PlateMap_Genotype_Zone.csv"
          , row.names = FALSE)
