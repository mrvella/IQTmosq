# This code will create the input file to run WFABC model 
##########################################################
### input file format
# The first line you define the number of loci & number of time points.
# The second line indicates the time of the 12 points. 
#   The important thing here is that the difference between 
#   the values corresponds to the number of generations between 
#   the time points. In this example the first time point is labeled 
#   as generation “1”, and the second time point is 9 generations 
#   later (“10”). Adding or subtracting a constant to all the values 
#   won’t change the results. They can also be negative!
# The rest of the file has to contain 1000 pairs of lines corresponding 
#   to the 1000 loci. For each locus the first line corresponds to the 
#   sample size at each time point (in number of chromosomes, so twice 
#   the number of individuals for diploids), and the second line to the 
#   number of A alleles at each time point. In this example all sample 
#   sizes are 100, and we only show the first 3 loci!
##########################################################
### Set up the working space ----------------------------
# clear working environment
rm(list = ls())

# Set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

### Create the input file ----------------------------
# Load data
mc.1016.yr <- read.csv("mc.1016.yr_reduced.csv")
mc.1534.yr <- read.csv("mc.1534.yr_reduced.csv")
sel.1016 <- mc.1016.yr[11:15,]
sel.1534 <- mc.1534.yr[3:9,]

# Define some params
# for all
g_per_yr <- 12

# For all loci under all years
n_timepts <- nrow(mc.1534.yr)
n_yr <- c(1:n_timepts)
gens_all <- g_per_yr*n_yr
n_chr_1016 <- 2*mc.1016.yr$n
n_R_1016 <- ((2*mc.1016.yr$RR) + mc.1016.yr$SR)
n_chr_1534 <- 2*mc.1534.yr$n
n_R_1534 <- ((2*mc.1534.yr$RR) + mc.1534.yr$SR)

# For 1016 under selection
n_timepts_1016 <- nrow(sel.1016)
n_yr_1016 <- c(1:n_timepts_1016)
gens_1016 <- g_per_yr*n_yr_1016
n_chr_1016_sel <- 2*sel.1016$n
n_R_1016_sel <- ((2*sel.1016$RR) + sel.1016$SR)

n_timepts_1534 <- nrow(sel.1534)
n_yr_1534 <- c(1:n_timepts_1534)
gens_1534 <- g_per_yr*n_yr_1534
n_chr_1534_sel <- 2*sel.1534$n
n_R_1534_sel <- ((2*sel.1534$RR) + sel.1534$SR)

# write txt file
sink("./WFABC/WFABC_multiple_loci.txt")
cat("2", n_timepts, "\n")
cat(gens_all, "\n")
cat(n_chr_1016, "\n")
cat(n_R_1016, "\n")
cat(n_chr_1534, "\n")
cat(n_R_1534)
sink()

sink("./WFABC/WFABC_V1016I_underSelection.txt")
cat("1", n_timepts_1016, "\n")
cat(gens_1016, "\n")
cat(n_chr_1016_sel, "\n")
cat(n_R_1016_sel)
sink()

sink("./WFABC/WFABC_F1534C_underSelection.txt")
cat("1", n_timepts_1534, "\n")
cat(gens_1534, "\n")
cat(n_chr_1534_sel, "\n")
cat(n_R_1534_sel)
sink()

