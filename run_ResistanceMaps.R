### Prepare map of IQT mosq info
### File Started: 7/5/17
### Last Updated: 7/19/17

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
#head(gis)

# Load kdr Data
kdrData_all <- read.csv("kdrData_all.csv", header = T, sep = ",")
#head(kdrData_all)

# Create column with first 6 characters from location code
kdrData_all$location <- substr(kdrData_all$Location_Code, 0, 6)

# 2 ################################################################################
### Connect GIS data to kdr data

kdr_gis <- sqldf("select 
                 kdrData_all.*
                 , gis.latitude
                 , gis.longitude
                 from kdrData_all
                 left join gis
                 on kdrData_all.location = gis.code
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
hasGis$newDate <- as.Date(hasGis$Date, format = "%m/%d/%Y")

# Specify years for analysis
year <- c(2000:2016)

# Loop to subset on year and genotype for locus 1534
for(i in year){
  assign(paste0("SS.", "1534.", i), subset(hasGis, format.Date(newDate, "%Y")==i & (hasGis$Rep1_1534=="SS")))
  assign(paste0("SR.", "1534.", i), subset(hasGis, format.Date(newDate, "%Y")==i & (hasGis$Rep1_1534=="SR")))
  assign(paste0("RR.", "1534.", i), subset(hasGis, format.Date(newDate, "%Y")==i & (hasGis$Rep1_1534=="RR")))
}

# Loop to subset on year and genotype for locus 1016
for(i in year){
  #dots <- vector()
  assign(paste0("SS.", "1016.", i), subset(hasGis, format.Date(newDate, "%Y")==i & (hasGis$Rep1_1016=="SS")))
  assign(paste0("SR.", "1016.", i), subset(hasGis, format.Date(newDate, "%Y")==i & (hasGis$Rep1_1016=="SR")))
  assign(paste0("RR.", "1016.", i), subset(hasGis, format.Date(newDate, "%Y")==i & (hasGis$Rep1_1016=="RR")))
  #assign(dots, lapply(paste0("SS.", "1016.", i), paste0("SR.", "1016.", i), paste0("RR.", "1016.", i)))
}


# 5 ################################################################################
### Create map elements

#get the center of the map
#iquitos_center = as.numeric(geocode("Iquitos"))
iquitos_center = c(-73.255, -3.740)

#map type: google types = "terrain", "satellite", "roadmap","hybrid"
myMapType <-c("roadmap")

#get the map from google map
gg_IquitosMap <-get_googlemap(center=iquitos_center, scale=2, zoom=14, maptype = myMapType);

#save the map object
iquitosmap = ggmap(gg_IquitosMap, extent="normal")

#display the map
#iquitosmap

# # 6 ################################################################################
# ### Create map plotting all samples
# 
# # Create array of coordinates
# # These points represent all data
# x.lon = as.vector(hasGis$longitude)
# y.lat = as.vector(hasGis$latitude)
# 
# # Add layers to the map
# p <- iquitosmap 
# p <- p + labs(x = "Longitude", y = "Latitude", title = "ALL") 
# p <- p + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
# p <- p + geom_point(aes(x=lon, y=lat), data = data.frame(lon = x.lon, lat = y.lat), color = "red")
# p
# 
# #save image of the map
# #ggsave("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/maps/mapSave.png",dpi=300)

# 7 ################################################################################
### Function to create map

# Locus 1534 
makemap.1534 <- function(RR, SR, SS){
  # Set working directory
  setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")
  # Add layers to map
  p <- iquitosmap 
  #p <- p + labs(x = "Longitude", y = "Latitude", title = "F1534C") 
  p <- p + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
  p <- p + geom_point(aes(x=lon, y=lat)
                      , data = data.frame(lon = RR$longitude, lat = RR$latitude)
                      , color = "red"
                      , size = 1.5)
  p <- p + geom_point(aes(x=lon, y=lat)
                      , data = data.frame(lon = SR$longitude, lat = SR$latitude)
                      , color = "dark orange"
                      , size = 1.5)
  p <- p + geom_point(aes(x=lon, y=lat)
                      , data = data.frame(lon = SS$longitude, lat = SS$latitude)
                      , color = "dark green"
                      , size = 1.5)
  return(p)
}

# Locus 1016 
makemap.1016 <- function(RR, SR, SS){
  # Set working directory
  setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")
  # Add layers to map
  p <- iquitosmap 
  #p <- p + labs(x = "Longitude", y = "Latitude", title = "V1016I") 
  p <- p + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5))
  p <- p + geom_point(aes(x=lon, y=lat)
                      , data = data.frame(lon = RR$longitude, lat = RR$latitude)
                      , color = "red"
                      , size = 1.5)
  p <- p + geom_point(aes(x=lon, y=lat)
                      , data = data.frame(lon = SR$longitude, lat = SR$latitude)
                      , color = "dark orange"
                      , size = 1.5)
  p <- p + geom_point(aes(x=lon, y=lat)
                      , data = data.frame(lon = SS$longitude, lat = SS$latitude)
                      , color = "dark green"
                      , size = 1.5)
  return(p)
}


# 8 ################################################################################
### Create maps

# Locus 1534
F1534C.2000 <- makemap.1534(RR.1534.2000, SR.1534.2000, SS.1534.2000) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2000")
F1534C.2001 <- makemap.1534(RR.1534.2001, SR.1534.2001, SS.1534.2001) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2001")
F1534C.2002 <- makemap.1534(RR.1534.2002, SR.1534.2002, SS.1534.2002) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2002")
F1534C.2003 <- makemap.1534(RR.1534.2003, SR.1534.2003, SS.1534.2003) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2003")
F1534C.2004 <- makemap.1534(RR.1534.2004, SR.1534.2004, SS.1534.2004) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2004")
F1534C.2005 <- makemap.1534(RR.1534.2005, SR.1534.2005, SS.1534.2005) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2005")
F1534C.2006 <- makemap.1534(RR.1534.2006, SR.1534.2006, SS.1534.2006) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2006")
F1534C.2007 <- makemap.1534(RR.1534.2007, SR.1534.2007, SS.1534.2007) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2007")
F1534C.2008 <- makemap.1534(RR.1534.2008, SR.1534.2008, SS.1534.2008) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2008")
F1534C.2009 <- makemap.1534(RR.1534.2009, SR.1534.2009, SS.1534.2009) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2009")
F1534C.2010 <- makemap.1534(RR.1534.2010, SR.1534.2010, SS.1534.2010) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2010")
F1534C.2011 <- makemap.1534(RR.1534.2011, SR.1534.2011, SS.1534.2011) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2011")
F1534C.2012 <- makemap.1534(RR.1534.2012, SR.1534.2012, SS.1534.2012) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2012")
#F1534C.2013 <- makemap.1534(RR.1534.2013, SR.1534.2013, SS.1534.2013) +
 # labs(x = "Longitude", y = "Latitude", title = "F1534C in 2013")
F1534C.2014 <- makemap.1534(RR.1534.2014, SR.1534.2014, SS.1534.2014) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2014")
#F1534C.2015 <- makemap.1534(RR.1534.2015, SR.1534.2015, SS.1534.2015) +
 # labs(x = "Longitude", y = "Latitude", title = "F1534C in 2015")
F1534C.2016 <- makemap.1534(RR.1534.2016, SR.1534.2016, SS.1534.2016) +
  labs(x = "Longitude", y = "Latitude", title = "F1534C in 2016")
  
# Locus 1016
V1016I.2000 <- makemap.1016(RR.1016.2000, SR.1016.2000, SS.1016.2000) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2000") 
V1016I.2001 <- makemap.1016(RR.1016.2001, SR.1016.2001, SS.1016.2001) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2001") 
V1016I.2002 <- makemap.1016(RR.1016.2002, SR.1016.2002, SS.1016.2002) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2002") 
V1016I.2003 <- makemap.1016(RR.1016.2003, SR.1016.2003, SS.1016.2003) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2003") 
V1016I.2004 <- makemap.1016(RR.1016.2004, SR.1016.2004, SS.1016.2004) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2004") 
V1016I.2005 <- makemap.1016(RR.1016.2005, SR.1016.2005, SS.1016.2005) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2005") 
V1016I.2006 <- makemap.1016(RR.1016.2006, SR.1016.2006, SS.1016.2006) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2006") 
V1016I.2007 <- makemap.1016(RR.1016.2007, SR.1016.2007, SS.1016.2007) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2007") 
V1016I.2008 <- makemap.1016(RR.1016.2008, SR.1016.2008, SS.1016.2008) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2008") 
V1016I.2009 <- makemap.1016(RR.1016.2009, SR.1016.2009, SS.1016.2009) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2009") 
V1016I.2010 <- makemap.1016(RR.1016.2010, SR.1016.2010, SS.1016.2010) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2010") 
V1016I.2011 <- makemap.1016(RR.1016.2011, SR.1016.2011, SS.1016.2011) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2011") 
V1016I.2012 <- makemap.1016(RR.1016.2012, SR.1016.2012, SS.1016.2012) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2012") 
#V1016I.2013 <- makemap.1016(RR.1016.2013, SR.1016.2013, SS.1016.2013) +
 # labs(x = "Longitude", y = "Latitude", title = "V1016I in 2013") 
V1016I.2014 <- makemap.1016(RR.1016.2014, SR.1016.2014, SS.1016.2014) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2014") 
#V1016I.2015 <- makemap.1016(RR.1016.2015, SR.1016.2015, SS.1016.2015) +
 # labs(x = "Longitude", y = "Latitude", title = "V1016I in 2015") 
V1016I.2016 <- makemap.1016(RR.1016.2016, SR.1016.2016, SS.1016.2016) +
  labs(x = "Longitude", y = "Latitude", title = "V1016I in 2016") 


# Save image of the 1534 maps
ls()
objects = ls()
myplots <- objects[1:15]
class(myplots)
l = mget(myplots)
invisible(mapply(ggsave, file=paste0("maps/map_", names(l), ".png"), plot=l))

# Save image of the 1016 maps
ls()
objects = ls()
myplots <- objects[133:147]
class(myplots)
l = mget(myplots)
invisible(mapply(ggsave, file=paste0("maps/map_", names(l), ".png"), plot=l))


# # Print maps for F1534C
# F1534C.2000
# F1534C.2001
# F1534C.2002
# F1534C.2003
# F1534C.2004
# F1534C.2005
# F1534C.2006
# F1534C.2007
# F1534C.2008
# F1534C.2009
# F1534C.2010
# F1534C.2011
# F1534C.2012
# #F1534C.2013
# F1534C.2014
# #F1534C.2015
# F1534C.2016
# 
# # Print maps for V1016I
# V1016I.2000
# V1016I.2001
# V1016I.2002
# V1016I.2003
# V1016I.2004
# V1016I.2005
# V1016I.2006
# V1016I.2007
# V1016I.2008
# V1016I.2009
# V1016I.2010
# V1016I.2011
# V1016I.2012
# #V1016I.2013
# V1016I.2014
# #V1016I.2015
# V1016I.2016



