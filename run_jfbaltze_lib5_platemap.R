# Find population name for jfbaltze_lib5 samples and create tsv files for stacks pipeline

setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

# import files
df <- read.csv("jfbaltze_lib5_platemap.csv", header = T, sep = ",")
head(df)
isolLog <- read.csv("IsolationLog_IQT-Mosq.csv", header = T, sep = ",")
head(isolLog)
zone <- read.csv("Location_Zone.csv", header = T, sep = ",")
head(zone)

# left join isolLog on df
x <- merge(x = df, y = isolLog[ , c("mosquito_id", "Location_Code", "Date")], by.x=c("sample_name"), by.y=c("mosquito_id"), all.x=TRUE)
head(x)

# left join zone on x
x2 <- merge(x = x, y = zone, by.x = c("Location_Code"), by.y = c("location_code"), all.x = T)
head(x2)

# Convert current date to readable form for R
x2$newDate <- as.character(as.Date(as.character(x2$Date), format = "%m/%d/%Y"))
head(x2)

# determine pre or post spray
library(dplyr)
x2$time <- if_else(x2$newDate < '2014-04-28', "pre", "post")
head(x2)

# Add population_name
attach(x2)
x2$population_name <- if_else(project_code == "buffer" & time == "pre", "buffer_pre"
                           , if_else(project_code == "buffer" & time == "post", "buffer_aft"
                                     , if_else(project_code == "treatment" & time == "pre", "treat_pre"
                                               , if_else(project_code == "treatment" & time == "post", "treat_aft"
                                                         , "error"))))
head(x2)

# Check for rows with NAs
hasNA <- x2[rowSums(is.na(x2)) > 0,]
hasNA


# Subset x2
x3 <- x2[, c("barcode", "sample_name", "index")]
# Split dataframe
df.list <- split(x3, as.factor(x3$index))
# Subset and rename
df.list.1 <- df.list$`1`[, c("barcode", "sample_name")]
df.list.2 <- df.list$`2`[, c("barcode", "sample_name")]
df.list.3 <- df.list$`3`[, c("barcode", "sample_name")]
df.list.4 <- df.list$`4`[, c("barcode", "sample_name")]
df.list.5 <- df.list$`5`[, c("barcode", "sample_name")]
df.list.6 <- df.list$`6`[, c("barcode", "sample_name")]
df.list.8 <- df.list$`8`[, c("barcode", "sample_name")]
df.list.12 <- df.list$`12`[, c("barcode", "sample_name")]
# Create .tsv files for bioinformatic pipeline
write.table(df.list.1, file = "barcodes.lib5.index1.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list.2, file = "barcodes.lib5.index2.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list.3, file = "barcodes.lib5.index3.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list.4, file = "barcodes.lib5.index4.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list.5, file = "barcodes.lib5.index5.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list.6, file = "barcodes.lib5.index6.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list.8, file = "barcodes.lib5.index8.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list.12, file = "barcodes.lib5.index12.tsv", sep = "\t", col.names = F, row.names = F, quote = F)

# Subset x2
x4 <- x2[ , c("sample_name", "population_name", "index")]
# Split dataframe
df.list2 <- split(x4, as.factor(x4$index))
# Subset and rename
df.list2.1 <- df.list2$`1`[, c("sample_name", "population_name")]
df.list2.2 <- df.list2$`2`[, c("sample_name", "population_name")]
df.list2.3 <- df.list2$`3`[, c("sample_name", "population_name")]
df.list2.4 <- df.list2$`4`[, c("sample_name", "population_name")]
df.list2.5 <- df.list2$`5`[, c("sample_name", "population_name")]
df.list2.6 <- df.list2$`6`[, c("sample_name", "population_name")]
df.list2.8 <- df.list2$`8`[, c("sample_name", "population_name")]
df.list2.12 <- df.list2$`12`[, c("sample_name", "population_name")]
# Create .tsv files for bioinformatic pipeline
write.table(df.list2$`1`, file = "popmap.lib5.index1.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list2$`2`, file = "popmap.lib5.index2.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list2$`3`, file = "popmap.lib5.index3.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list2$`4`, file = "popmap.lib5.index4.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list2$`5`, file = "popmap.lib5.index5.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list2$`6`, file = "popmap.lib5.index6.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list2$`8`, file = "popmap.lib5.index8.tsv", sep = "\t", col.names = F, row.names = F, quote = F)
write.table(df.list2$`12`, file = "popmap.lib5.index12.tsv", sep = "\t", col.names = F, row.names = F, quote = F)






