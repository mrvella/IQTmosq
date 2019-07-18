# Add kdr genotypes to platemap for jfbaltz_lib5

# clear working environment
rm(list = ls())

# set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

# import & prep files
pltmap <- read.csv("jfbaltz_lib5_platemap.csv", header = T, sep = ",")
colnames(pltmap)[colnames(pltmap)=="sample_name"] <- "mosquito_id"
pltmap <- pltmap[,-6]
head(pltmap)

popmap <- read.csv("popmap.lib5.tsv", header = F, sep = "\t")
names(popmap) <- c("mosquito_id", "zone_time")
head(popmap)

kdr <- read.csv("kdrData_reduced.csv", header = T, sep = ",")
head(kdr)
names(kdr)

# Left join popmap to pltmap
pltmap <- merge(x = pltmap, y = popmap, by = c("mosquito_id"), all.x=TRUE)
head(pltmap)

# Left join kdr haplotypes to x
pltmap <- merge(x = pltmap, y = kdr[ , c("mosquito_id", "haplotype")], by = c("mosquito_id"), all.x=TRUE)
head(pltmap)


# Add separate column for zone
pltmap$zone <- ifelse(pltmap$zone_time == "buffer_pre", "buffer"
                              , ifelse(pltmap$zone_time == "buffer_aft", "buffer"
                                        , ifelse(pltmap$zone_time == "treat_pre", "treatment"
                                                  , ifelse(pltmap$zone_time == "treat_aft", "treatment"
                                                            , "error"))))
head(pltmap)

# Add separate column for time
pltmap$time <- ifelse(pltmap$zone_time == "buffer_pre", "pre"
                      , ifelse(pltmap$zone_time == "buffer_aft", "aft"
                               , ifelse(pltmap$zone_time == "treat_pre", "pre"
                                        , ifelse(pltmap$zone_time == "treat_aft", "aft"
                                                 , "error"))))
head(pltmap)


# Hand correcting errors in haplotype calls ----------------------
### These corrections were verified by eye on 6/17/19 by JFB
# Update haplotypes for each sample which contains an "error"
# change IQT00714 from RRerror to RRRR
pltmap <- within(pltmap, haplotype[mosquito_id == 'IQT00714'] <- 'RRRR')
# change IQT00723 from RRerror to RRRR
pltmap <- within(pltmap, haplotype[mosquito_id == 'IQT00723'] <- 'RRRR')
# change IQT00817 from RRerror to RRRR
pltmap <- within(pltmap, haplotype[mosquito_id == 'IQT00817'] <- 'RRRR')
# change IQT01286 from SRerror to RRRR
pltmap <- within(pltmap, haplotype[mosquito_id == 'IQT01286'] <- 'SRRR')
# change IQT01321 from RRerror to RRRR
pltmap <- within(pltmap, haplotype[mosquito_id == 'IQT01321'] <- 'RRRR')
# change IQT01323 from SRerror to SRRR
pltmap <- within(pltmap, haplotype[mosquito_id == 'IQT01323'] <- 'SRRR')
# change IQT02451 from errorRR to SSRR
pltmap <- within(pltmap, haplotype[mosquito_id == 'IQT02451'] <- 'SSRR')


# Create popmap with kdr genotypes as 2 groups
popmap_kdr <- pltmap[,c(1,7)]
unique(popmap_kdr$haplotype)

# Write new files with info
write.csv(pltmap, file = "jfbaltz_lib5_pltmap_genos.csv", sep = ",", col.names = T, row.names = F, quote = F)
write.table(popmap_kdr, file = "popmap.lib5.genos.tsv", sep = "\t", col.names = F, row.names = F, quote = F)






