setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")
library(tidyverse)


# Load data
pltmap <- read.csv("jfbaltz_lib5_platemap.csv")
head(pltmap)

kdrData <- read.csv("kdrData_reduced_2018-11-09.csv")
head(kdrData)


# Make new dataframe with genotype info for lib5 samples
x <- pltmap %>% 
  left_join(select(kdrData, haplotype, mosquito_id)
            , by = c("sample_name" = "mosquito_id"))
head(x)

# Reduce columns
y <- x[,c(5,7)]
head(y)

write.csv(y, file = "lib5_kdr-genos.csv", row.names = FALSE)


