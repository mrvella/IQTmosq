# This script will run the slattice package to estimate selection coefficents

# # temp, delete after working
# require(devtools)
# install_github("mathii/slattice", build_vignettes=TRUE)


### clear working environment
rm(list = ls())

### Set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

# load library
library("slattice")
library("berryFunctions")

# For some brief examples of how to run the estimator, look at the vignette (in R):
# vignette("slattice")
# Vignette Examples ---------------------
# First, simulate some data under a Wright-Fisher model using the generate.observations function:
Ne <- 1000                              #N_e
g<-100                                  #Number of generations
p0 <- 0.1                               #Initial freq
s <- 0.05                               #Selection coefficient
data<-generate.observations(Ne, g, p0, s, missing.p=0.8, size.params=list(N=100,p=0.5))

# The data is a data frame with one row per generation, and two columns named N for the total 
# number of chromosomes observed and N.A for the total number that carry the allele that is under selection.
head(data$obs)

# Run the EM estimator using the default “Soft EM” algorithm. The output is an object that constains the 
# estimated selection coefficient, and various other information including the posterior decoding of the 
# frequency. Set verbose=FALSE to avoid seeing any output.
estimate <- estimate.s(data$obs, Ne, method="Soft EM", verbose=TRUE)
estimate$s

# Simulate data under the Wright-Fisher lattice model.
k1 <- 4                                  # Number of rows of demes
k2 <- 3                                  # Number of cols of demes
Ne <- 1000                               # N_e in each deme
g<-100                                   # Number of generations
p0 <- 0.1                                # Initial frequency
s <- matrix(0.06*seq(1,-1,length.out=k1), k1, k2, byrow=FALSE) #S^{ij} - matrix of selection coefficients
m <- 0.04                                # Scaled migration rate
lattice.data<-generate.lattice.observations(Ne, g, p0, s, k1, k2, Ne*m, missing.p=0.9
                                            , size.params=list(N=100, p=0.5))

# Now run the lattice EM estimator. The output includes the final estimates, as well as all the 
# posterior decodings. Here we set verbose=FALSE to avoid plotting the intermediate steps. 
# You do not need to specify initial.M, but the estimator seems to perform better if you do.

lattice.estimate<-estimate.s.m(lattice.data$obs, Ne, M=NULL, update="Soft EM", max.iters=10
                               , verbose=FALSE, initial.M=m)

# A nice way to plot the combined observations and results:

plot.wright.fisher.lattice.observations(lattice.data$obs, lattice.data$f, lattice.estimate$f
                                        , est.s=lattice.estimate$s, error.bars=TRUE, main="Lattice Example")


### Work on my data -------------------------------
# Load data
mc.1016.yr <- read.csv("mc.1016.yr_reduced.csv")
mc.1534.yr <- read.csv("mc.1534.yr_reduced.csv")

# Define some params
n_timepts <- nrow(mc.1534.yr)
g_per_yr <- 12
n_yr <- c(1:n_timepts)
generations <- g_per_yr*n_yr

# Create df for V1016I
N.1016 <- 2*mc.1016.yr$n 
N.A.1016 <- ((2*mc.1016.yr$RR) + (mc.1016.yr$SR))
df.1016 <- as.data.frame(cbind(generations, N=N.1016, N.A=N.A.1016))
zeros <- data.frame(matrix(0, ncol = 3, nrow = 11))
colnames(zeros) <- c("generations", "N", "N.A")
temp <- rbind(zeros, df.1016[1,], zeros, df.1016[2,], zeros, df.1016[3,]
              , zeros, df.1016[4,], zeros, df.1016[5,], zeros, df.1016[6,]
              , zeros, df.1016[7,], zeros, df.1016[8,], zeros, df.1016[9,]
              , zeros, df.1016[10,], zeros, df.1016[11,], zeros, df.1016[12,]
              , zeros, df.1016[13,], zeros, df.1016[14,], zeros, df.1016[15,]
              , zeros, df.1016[16,], zeros, df.1016[17,], zeros, df.1016[18,])
df.1016.gens <- temp[, 2:3]

# Create df for F1534C
N.1534 <- 2*mc.1534.yr$n 
N.A.1534 <- ((2*mc.1534.yr$RR) + (mc.1534.yr$SR))
df.1534 <- as.data.frame(cbind(generations, N=N.1534, N.A=N.A.1534))
zeros <- data.frame(matrix(0, ncol = 3, nrow = 11))
colnames(zeros) <- c("generations", "N", "N.A")
temp <- rbind(zeros, df.1534[1,], zeros, df.1534[2,], zeros, df.1534[3,]
              , zeros, df.1534[4,], zeros, df.1534[5,], zeros, df.1534[6,]
              , zeros, df.1534[7,], zeros, df.1534[8,], zeros, df.1534[9,]
              , zeros, df.1534[10,], zeros, df.1534[11,], zeros, df.1534[12,]
              , zeros, df.1534[13,], zeros, df.1534[14,], zeros, df.1534[15,]
              , zeros, df.1534[16,], zeros, df.1534[17,], zeros, df.1534[18,])
df.1534.gens <- temp[, 2:3]


# Run the EM estimator 
Ne <- 50000
h <- 0.8
# methods: "Soft EM", "Hard EM", "Stochastic EM", "Simple"
estimate.1016.soft <- estimate.s(df.1016.gens, h=h, Ne=Ne, method="Soft EM", verbose=TRUE)
estimate.1016.hard <- estimate.s(df.1016.gens, h=h, Ne=Ne, method="Hard EM", verbose=TRUE)
estimate.1016.stoc <- estimate.s(df.1016.gens, h=h, Ne=Ne, method="Stochastic EM", verbose=TRUE)
estimate.1016.simp <- estimate.s(df.1016.gens, h=h, Ne=Ne, method="Simple", verbose=TRUE)

estimate.1534.soft <- estimate.s(df.1534.gens, h=h, Ne=Ne, method="Soft EM", verbose=TRUE)
estimate.1534.hard <- estimate.s(df.1534.gens, h=h, Ne=Ne, method="Hard EM", verbose=TRUE)
estimate.1534.stoc <- estimate.s(df.1534.gens, h=h, Ne=Ne, method="Stochastic EM", verbose=TRUE)
estimate.1534.simp <- estimate.s(df.1534.gens, h=h, Ne=Ne, method="Simple", verbose=TRUE)

estimate.1016.soft$s
estimate.1016.hard$s
estimate.1016.stoc$s
estimate.1016.simp$s
estimate.1534.soft$s
estimate.1534.hard$s
estimate.1534.stoc$s
estimate.1534.simp$s


