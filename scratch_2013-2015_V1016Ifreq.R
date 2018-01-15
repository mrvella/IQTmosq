setwd("~/Dropbox/")
pltMap <- read.csv("temp.csv", header = T)
head(pltMap)
pltMap$newDate <- as.character(as.Date(as.character(pltMap$Date), format = "%m/%d/%Y"))
yr2013 <- sqldf("Select * from pltMap where newDate between '2013-01-01' and '2013-12-31'")
yr2015 <- sqldf("Select * from pltMap where newDate between '2015-01-01' and '2015-12-31'")

### 2013 
RR.2013 <- sum(yr2013$Genotype == "HomozygousLow")
SS.2013 <- sum(yr2013$Genotype == "HomozygousHigh")
SR.2013 <- sum(yr2013$Genotype == "Heterozygous")
RR.2013
SS.2013
SR.2013
n.2013 <- RR.2013 + SS.2013 + SR.2013; n.2013
countR.2013 <- 2*RR.2013 + SR.2013; countR.2013
countS.2013 <- 2*SS.2013 + SR.2013; countS.2013
total.2013 <- countR.2013 + countS.2013; total.2013
freqR.2013 <- countR.2013/total.2013; freqR.2013
freqS.2013 <- countS.2013/total.2013; freqS.2013


### 2015 
RR.2015 <- sum(yr2015$Genotype == "HomozygousLow")
SS.2015 <- sum(yr2015$Genotype == "HomozygousHigh")
SR.2015 <- sum(yr2015$Genotype == "Heterozygous")
RR.2015
SS.2015
SR.2015
n.2015 <- RR.2015 + SS.2015 + SR.2015; n.2015
countR.2015 <- 2*RR.2015 + SR.2015; countR.2015
countS.2015 <- 2*SS.2015 + SR.2015; countS.2015
total.2015 <- countR.2015 + countS.2015; total.2015
freqR.2015 <- countR.2015/total.2015; freqR.2015
freqS.2015 <- countS.2015/total.2015; freqS.2015



