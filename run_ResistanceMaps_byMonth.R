### Prepare map of IQT mosq info by month for V1016I in 2014
### File Started: 7/10/17
### Last Updated: 7/18/17

# 0 ################################################################################
### Prepare working environment

### clear working environment
rm(list = ls())

### load libraries
# options() line below prevents R from stalling while loading sqldf library
options(gsubfn.engine = "R")
library(sqldf)
library(ggmap)

# Set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

# 1 ################################################################################
### Load GIS and kdr files and prep Location Code for analysis

# Load gis file
gis <- read.csv("IQT_location_GIS.csv", header = T, sep = ",")
head(gis)

# Load Block data
modelo <- read.csv("Blocks_2014.csv", header = T, sep = ",")
head(modelo)

# Load kdr Data
# kdrData_all is based off of the first replicate for each locus
#kdrData_all <- read.csv("kdrData_all.csv", header = T, sep = ",")
#head(kdrData_all)

# kdrData is based off of both replicates matching for each locus
kdrData <- read.csv("kdrData.csv", header = T, sep = ",")

# Create column with first 6 characters from location code
kdrData_all$location <- substr(kdrData_all$Location_Code, 0, 6)
#kdrData$location <- substr(kdrData_all$Location_Code, 0, 6)

# 2 ################################################################################
### Connect GIS data to kdr data

gis_modelo <- sqldf("SELECT * 
                    FROM gis
                    LEFT JOIN modelo
                    USING(block)")
#head(gis_modelo)

kdr_gis <- sqldf("SELECT *
                 FROM kdrData_all
                 LEFT JOIN gis_modelo 
                 ON kdrData_all.location = gis_modelo.code
                 ")
#head(kdr_gis)

# 3 ################################################################################
### Find location codes that do not have GIS coordinates and ones that do

# Does not have gis coordinates
noGis <- subset(kdr_gis, (is.na(kdr_gis$latitude)) & (is.na(kdr_gis$longitude)))
noGis <- unique(noGis$location)
write.csv(noGis, "noGis.csv", row.names = T)
#head(noGis)

# Does have gis coordinates
hasGis <- subset(kdr_gis, (!is.na(kdr_gis$latitude)) & (!is.na(kdr_gis$longitude)))
#head(hasGis)

# 4 ################################################################################
### Subset by year and genotype
# Convert current date to readable form
hasGis$newDate <- as.character(as.Date(as.character(hasGis$Date), format = "%m/%d/%Y"))
#head(hasGis)

# Subset rows in hasGis by year 2014 and for only samples without NA in modelo
hasGis <- subset(hasGis, format.Date(newDate, "%Y")=="2014" & !is.na(hasGis$modelo))
#head(hasGis)

# Specify years for analysis
month <- c("01", "02", "03", "04", "05", "06", "07", "08", "10")

# Loop to subset on month and genotype for locus 1016
for(i in month){
  assign(paste0("SS.", "1016.", i)
         , subset(hasGis, format.Date(newDate, "%m")==i & (hasGis$Rep1_1016=="SS")))
  assign(paste0("SR.", "1016.", i)
         , subset(hasGis, format.Date(newDate, "%m")==i & (hasGis$Rep1_1016=="SR")))
  assign(paste0("RR.", "1016.", i)
         , subset(hasGis, format.Date(newDate, "%m")==i & (hasGis$Rep1_1016=="RR")))
}

# Merge subset results to get pre and post spray dataframes
# Note that may is not included here as it was during the spray period
preSS <- rbind(SS.1016.01, SS.1016.02, SS.1016.03, SS.1016.04)
preSR <- rbind(SR.1016.01, SR.1016.02, SR.1016.03, SS.1016.04)
preRR <- rbind(RR.1016.01, RR.1016.02, RR.1016.03, RR.1016.04)

postSS <- rbind(SS.1016.06, SS.1016.07, SS.1016.08, SS.1016.10)
postSR <- rbind(SR.1016.06, SR.1016.07, SR.1016.08, SS.1016.10)
postRR <- rbind(RR.1016.06, RR.1016.07, RR.1016.08, RR.1016.10)


# 5 ################################################################################
### Create map elements

#get the center of the map
#iquitos_center = as.numeric(geocode("Iquitos"))
iquitos_center = c(-73.257, -3.741)

#map type: google types = "terrain", "satellite", "roadmap","hybrid"
myMapType <-c("satellite")

#get the map from google map
gg_IquitosMap <-get_googlemap(center=iquitos_center, scale=2, zoom=16, maptype = myMapType);

#save the map object
iquitosmap = ggmap(gg_IquitosMap, extent="normal")

#display the map
iquitosmap

# 7 ################################################################################
### Function to create map

# Locus 1016 
makemap.1016 <- function(RR, SR, SS){
  # Set working directory
  setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")
  # Add layers to map
  p <- iquitosmap 
  #p <- p + labs(x = "Longitude", y = "Latitude", title = "V1016I") 
  p <- p + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                 plot.margin = unit(c(1,1,1,1), "cm"))
  p <- p + geom_point(aes(x=lon, y=lat)
                      , data = data.frame(lon = RR$longitude, lat = RR$latitude)
                      , color = "red"
                      , size = 2)
  p <- p + geom_point(aes(x=lon, y=lat)
                      , data = data.frame(lon = SR$longitude, lat = SR$latitude)
                      , color = "orange"
                      , size = 2)
  p <- p + geom_point(aes(x=lon, y=lat)
                      , data = data.frame(lon = SS$longitude, lat = SS$latitude)
                      , color = "green"
                      , size = 2)
  return(p)
}


# 8 ################################################################################
### Create maps by month

V1016I.1 <- makemap.1016(RR.1016.01, SR.1016.01, SS.1016.01) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in January")
V1016I.2 <- makemap.1016(RR.1016.02, SR.1016.02, SS.1016.02) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in February")
#V1016I.3 <- makemap.1016(RR.1016.03, SR.1016.03, SS.1016.03) +
 # labs(x = "Longitude", y = "Latitude", title = "V1016I in March")
V1016I.4 <- makemap.1016(RR.1016.04, SR.1016.04, SS.1016.04) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in April")
V1016I.5 <- makemap.1016(RR.1016.05, SR.1016.05, SS.1016.05) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in May")
V1016I.6 <- makemap.1016(RR.1016.06, SR.1016.06, SS.1016.06) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in June")
V1016I.7 <- makemap.1016(RR.1016.07, SR.1016.07, SS.1016.07) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in July")
V1016I.8 <- makemap.1016(RR.1016.08, SR.1016.08, SS.1016.08) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in August")
V1016I.10 <- makemap.1016(RR.1016.10, SR.1016.10, SS.1016.10) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in October")

# ### Print maps
# V1016I.1
# V1016I.2
# #V1016I.3
# V1016I.4
# V1016I.5
# V1016I.6
# V1016I.7
# V1016I.8
# V1016I.10

# Save image of the maps
ls()
objects = ls()
myplots <- objects[48:55]
class(myplots)
l = mget(myplots)
invisible(mapply(ggsave, file=paste0("maps/map_", names(l), ".png"), plot=l))



# # 09 ################################################################################
# ### Create map by pre and post spray
# preSpray <- makemap.1016(preRR, preSR, preSS) +
#   labs(x = "Longitude", y = "Latitude", title = "V1016I Pre-Spray") 
# Spray <- makemap.1016(RR.1016.05, SR.1016.05, SS.1016.05) +
#   labs(x = "Longitude", y = "Latitude", title = "V1016I Spray")
# postSpray <- makemap.1016(postRR, postSR, postSS) +
#   labs(x = "Longitude", y = "Latitude", title = "V1016I Post-Spray") 
# 
# # Print maps
# preSpray
# Spray
# postSpray


# # 10 ################################################################################
# ### Add polygons to map to represent treatment and control areas
# 
# library(rgdal)
# 
# Neighborhoods <- readOGR("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database","City_Blocks_UTM")
# 







