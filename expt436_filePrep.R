# This script will find samples to include in the OmeSeq study of 2014 mosquitoes.

# Load libraries
library(dplyr)

# load data
iso.log <- read.csv("../../IsolationLog_IQT-Mosq.csv", header = TRUE)
loc.log <- read.csv("../../IQT_location_GIS.csv", header = TRUE)
zone.log <- read.csv("../../Location_Zone.csv", header = TRUE)
plt.log <- read.csv("../../Isolation_plt-expt_log.csv", header = TRUE)
quant.log <- read.csv("../../QuantificationLog_IQT-Mosq.csv", header = TRUE)
ngs2014.log <- read.csv("../../../QGIS_Files/Routput/masterdec_ngs.csv", header = TRUE)

# Left join datasets together
df <- merge(x = iso.log, y = loc.log, by.x = "Location_Code", by.y = "code", all.x = TRUE)
df <- merge(x = df, y = plt.log, by = "expt_id", all.x = TRUE)
df <- merge(x = df, y = zone.log, by.x = "Location_Code", by.y = "location_code", all.x = TRUE)
df <- merge(x = df, y = quant.log
            , by.x = c("expt_id", "gDNA_well_id"), by.y = c("expt_id", "well_id")
            , all.x = TRUE)

# remove samples that were used in the ngs2014 experiment
matches <- which(df$mosquito_id %in% ngs2014.log$mosquito_id)
df <- df[-matches, ]

# create newDate column (note: year must be capital Y)
df$newDate <- as.Date( as.character(df$Date), "%m/%d/%Y")

# subset 2014 samples
df.2014 <- df[grep("2014", df$Date), ]

# remove samples with na in column Latitude (Longitude is present if Latitude is present) and conc_ng.ul
df.2014 <- df.2014[!is.na(df.2014$latitude), ]

# remove unwanted columns
df.2014 <- df.2014[c("project_code", "newDate", "Location_Code", "plate_id", "expt_id"
                     , "gDNA_well_id", "mosquito_id",  "latitude", "longitude", "conc_ng.ul")]

# order dataframe by date, location_code, and expt_id
df.2014 <- df.2014[order(df.2014[,"newDate"], df.2014[,"Location_Code"]
                         , df.2014[, "expt_id"], df.2014[, "gDNA_well_id"]),]

# convert concentration to number and remove non-numeric rows
df.2014 <- df.2014[!is.na(as.numeric(as.character(df.2014$conc_ng.ul))),]

# add column to calculate amount needed for 200ng
df.2014$for200ng <- round(200/(as.numeric(as.character(df.2014$conc_ng.ul))), digits = 2)

# subset based on project_code
other.all <- df.2014[(df.2014$project_code=="other"),]
buff.pre.all <- df.2014[(df.2014$project_code == "buffer") & (df.2014$newDate < "2014-04-28"),]
buff.aft.all <- df.2014[(df.2014$project_code == "buffer") & (df.2014$newDate > "2014-04-28"),]
treat.pre.all <- df.2014[(df.2014$project_code == "treatment") & (df.2014$newDate < "2014-04-28"),]
treat.aft.all <- df.2014[(df.2014$project_code == "treatment") & (df.2014$newDate > "2014-04-28"),]

# subset based on concentration
df.2014_1.5 <- df.2014[((as.numeric(as.character(df.2014$conc_ng.ul))) > 1.5),]

other_1.5 <- df.2014_1.5[(df.2014_1.5$project_code=="other"),]
buff.pre_1.5 <- df.2014_1.5[(df.2014_1.5$project_code == "buffer") & (df.2014_1.5$newDate < "2014-04-28"),]
buff.aft_1.5 <- df.2014_1.5[(df.2014_1.5$project_code == "buffer") & (df.2014_1.5$newDate > "2014-04-28"),]
treat.pre_1.5 <- df.2014_1.5[(df.2014_1.5$project_code == "treatment") & (df.2014_1.5$newDate < "2014-04-28"),]
treat.aft_1.5 <- df.2014_1.5[(df.2014_1.5$project_code == "treatment") & (df.2014_1.5$newDate > "2014-04-28"),]

# write.csv files for printing
write.csv(df.2014_1.5, file = "../../expt436/expt436_df.2014_1.5.csv", row.names = FALSE)
write.csv(other.all, file = "../../expt436/expt436_other.all.csv", row.names = FALSE)
write.csv(buff.pre.all, file = "../../expt436/expt436_buff.pre.all.csv", row.names = FALSE)
write.csv(buff.aft.all, file = "../../expt436/expt436_buff.aft.all.csv", row.names = FALSE)
write.csv(treat.pre.all, file = "../../expt436/expt436_treat.pre.all.csv", row.names = FALSE)
write.csv(treat.aft.all, file = "../../expt436/expt436_treat.aft.all.csv", row.names = FALSE)

write.csv(other_1.5, file = "../../expt436/expt436_other_1.5.csv", row.names = FALSE)
write.csv(buff.pre_1.5, file = "../../expt436/expt436_buff.pre_1.5.csv", row.names = FALSE)
write.csv(buff.aft_1.5, file = "../../expt436/expt436_buff.aft_1.5.csv", row.names = FALSE)
write.csv(treat.pre_1.5, file = "../../expt436/expt436_treat.pre_1.5.csv", row.names = FALSE)
write.csv(treat.aft_1.5, file = "../../expt436/expt436_treat.aft_1.5.csv", row.names = FALSE)

# # Temp code: find sample
# north[(north$Location_Code == "SAB506"),]
# df.2014_1.5[(df.2014_1.5$Location_Code == "SAA630"),]

### Select samples for OmeSeq
# # order "other" samples by latitude
# other_1.5 <- other_1.5[order(other_1.5[,"latitude"]),]
# subset south of sample MC688, latitude = -3.753219
south <- other_1.5[(other_1.5$latitude > "-3.753219"),]
# subset north of sample SAB506, latitude = -3.727464 so as not to include SAB506
northW <- other_1.5[(other_1.5$latitude < "-3.727400"),]
# subset north of sample MYB083, latitude = -3.736627 and south of SAB506
north <- other_1.5[(other_1.5$latitude < "-3.736627") 
                   & (other_1.5$latitude > "-3.727400"),]
# Remove duplicates based on Location_Code, keep first row of duplicates
north <- north[!duplicated(north$Location_Code),]

# count number of occurances for each location_code
multIndivs <- df.2014_1.5 %>% group_by(Location_Code, newDate) %>% tally()
multIndivs <- arrange(multIndivs, desc(n))
multIndivs <- multIndivs[(multIndivs$n > 10),]
multIndivs <- left_join(multIndivs, df.2014_1.5, by = c("Location_Code", "newDate"))
multIndivs <- select(multIndivs, -n)
# multIndivs <- multIndivs[, c("project_code", "newDate", "Location_Code", "plate_id"
                             # , "expt_id", "gDNA_well_id", "mosquito_id", "latitude"
                             # , "longitude", "conc_ng.ul", "for200ng")]
write.csv(multIndivs, file = "../../expt436/expt436_multiplesPerLocationCode.csv", row.names = FALSE)

# find samples from experimental area
remain <- df.2014_1.5[(df.2014_1.5$project_code != "other") & (df.2014_1.5$newDate < "2014-04-28"),]
# Remove duplicates based on Location_Code, keep first row of duplicates
remain <- remain[!duplicated(remain$Location_Code),]
# Sample 175 random samples from remain
remain <- sample_n(remain, 173)

# sample count
a <- nrow(north); a
b <- nrow(northW); b
c <- nrow(south); c
d <- nrow(multIndivs); d
e <- nrow(remain); e
tot <- sum(a, b, c, d, e); tot
need <- 384 - tot; need

final384 <- bind_rows(south, north, northW, remain, multIndivs)
# write.csv(final384, file = "../../expt436/expt436_final384.csv", row.names = FALSE)

######
# Find replacement samples for evaporated samples from final384
fin <- read.csv("../../expt436/expt436_final384.csv", header = TRUE)
# change data type for compatibility
fin$newDate <- as.Date(fin$newDate, format = "%m/%d/%Y")
fin$conc_ng.ul <- as.factor(fin$conc_ng.ul)
# find differences between dataframes
replacements <- setdiff(remain, fin)
# order replacements by mosquito_id
replacements <- replacements[order(replacements[,"mosquito_id"]),]
write.csv(replacements, file = "../../expt436/expt436_replacements.csv", row.names = FALSE)

