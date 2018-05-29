# GEE model 
# See paper: 
# Nooraee, N., Molenberghs, G. & van den Heuvel, E. R. GEE for longitudinal ordinal data: 
#     Comparing R-geepack, R-multgee, R-repolr, SAS-GENMOD, SPSS-GENLIN. Computational Statistics 
#     and Data Analysis 77, 70–83 (2014).
###############################################################################
# set working directory
setwd("~/Dropbox/GouldLab/Project_Mosquito/Database")

mc.1016 <- read.csv("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/mc.1016.mo_reduced.csv")
# Load mc.1016.t and mc.1016.b data
mc.1016.t <- read.csv("mc.1016.t.csv")
mc.1016.b <- read.csv("mc.1016.b.csv")

# 3/19/18 - Subset mc.1016.t to remove March data because sample size is too low, currently n = 4
mc.1016.t <- mc.1016.t[-3,]
mc.1016.b <- mc.1016.b[-3,]

# Add column for treatment type
# for spray region
list1 <- 1:9
treatment <- rep("spray",length(list1))
mc.1016.t <- cbind(mc.1016.t, treatment)
mc.1016.t

# for buffer region
treatment <- rep("buffer", length(list1))
mc.1016.b <- cbind(mc.1016.b, treatment)
mc.1016.b

# Combine the datasets
mc.1016.2014 <- rbind(mc.1016.b, mc.1016.t)
mc.1016.2014

# Remove rows with NA
mc.1016.2014 <- mc.1016.2014[complete.cases(mc.1016.2014), ]

# Calculate number of R and S alleles from genotype counts
mc.1016.2014$R <- mc.1016.2014$SR + (2*mc.1016.2014$RR)
mc.1016.2014$S <- mc.1016.2014$SR + (2*mc.1016.2014$SS)

# Add column with unique id
mc.1016.2014$id <- c(1:16)
mc.1016.2014

###############################################################################
library("multgee")

# Example from package documentation
data(arthritis)
fitord <- ordLORgee(y~factor(time)+factor(trt)+factor(baseline), data=arthritis,
                    id=id, repeated=time)
summary(fitord)

# Now with my data
fitord2 <- ordLORgee(freqR ~ factor(month) + factor(treatment), data = mc.1016.2014
                     , id = id, repeated = month)
summary(fitord2)

###############################################################################
library("gee")

# example from gee
data(warpbreaks)
## marginal analysis of random effects model for wool
summary(gee(breaks ~ tension, id=wool, data=warpbreaks, corstr="exchangeable"))
## test for serial correlation in blocks
summary(gee(breaks ~ tension, id=wool, data=warpbreaks, corstr="AR-M", Mv=1))

# example formula
# y~factor(time)+factor(trt), data=arthritis,id=id, repeated=time)
# V1016I ~ newDate + project_code

# gee usage 
gee(formula, id,
    data, subset, na.action,
    R = NULL, b = NULL,
    tol = 0.001, maxiter = 25,
    family = gaussian, corstr = "independence",
    Mv = 1, silent = TRUE, contrasts = NULL,
    scale.fix = FALSE, scale.value = 1, v4.4compat = FALSE)



