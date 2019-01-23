# Find samples to use for lib 8 (2000 sequencing project)

# clear memory
rm(list=ls())

# set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

# list files in current directory
list.files('.')

# load data
concs <- read.csv("2000samps_withConcentrations.csv")
head(concs)
# masterdec.csv contains GPS and neighborhood info
gps <- read.csv("../QGIS_Files/Routput/masterdec.csv")
head(gps)

# left join gps onto df
df <- merge(x = concs, y = gps[, c("X", "Y", "NEIGHBORHO", "LOC_CODE")]
            , by.x = "location_code", by.y = "LOC_CODE", all.x = TRUE)
head(df)

# order df by decreasing concentration
df <- df[order(df$conc, decreasing=TRUE),]
head(df)

# remove rows with NA
df <- df[complete.cases(df), ]

# top 150
top150 <- df[1:150,]
unique(top150$location_code)

# order df by decreasing concentration and location_code
top150.s <- top150[order(top150$location_code),]
head(top150.s)

# Save file to plot
write.csv(df, file = "../QGIS_Files/Routput/2000samps_withConcsGPS.csv", row.names = FALSE)
write.csv(top150.s, file = "../QGIS_Files/Routput/2000_top150.csv", row.names = FALSE)
