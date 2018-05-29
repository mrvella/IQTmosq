# Find population name for jfbaltze_lib5 samples and create tsv files for stacks pipeline

setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

# import files
df <- read.csv("jfbaltze_lib5_platemap.csv", header = T, sep = ",")
#head(df)
isolLog <- read.csv("IsolationLog_IQT-Mosq.csv", header = T, sep = ",")
#head(isolLog)
zone <- read.csv("Location_Zone.csv", header = T, sep = ",")
#head(zone)

# left join isolLog on df
x <- merge(x = df, y = isolLog[ , c("mosquito_id", "Location_Code", "Date")], by.x=c("sample_name"), by.y=c("mosquito_id"), all.x=TRUE)

# left join zone on x
x2 <- merge(x = x, y = zone, by.x = c("Location_Code"), by.y = c("location_code"), all.x = T)

# Convert current date to readable form for R
x2$newDate <- as.character(as.Date(as.character(x2$Date), format = "%m/%d/%Y"))

# determine pre or post spray
library(dplyr)
x2$time <- if_else(x2$newDate < '2014-04-28', "pre", "post")

# Add population_name
attach(x2)
x2$population_name <- if_else(project_code == "buffer" & time == "pre", "buffer_pre"
                           , if_else(project_code == "buffer" & time == "post", "buffer_aft"
                                     , if_else(project_code == "treatment" & time == "pre", "treat_pre"
                                               , if_else(project_code == "treatment" & time == "post", "treat_aft", "error"))))

head(x2)
