# Calculate Chi-Square for 2014 Perturbation Experiment
# Started: 7/26/17
# Last Update: 8/1/17

########################################################################################
# Prepare workspace
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")

# This code assumes mc.1016.t and mc.1016.b created from file run_MeltCurve_analysis.R
# Load required dataframes
mc.1016.t <- read.csv("mc.1016.t.csv")
mc.1016.b <- read.csv("mc.1016.b.csv")

# add column for freqS
S <- (2*mc.1016.t$SS) + mc.1016.t$SR
R <- (2*mc.1016.t$RR) + mc.1016.t$SR
mc.1016.t <- cbind(mc.1016.t, S, R); mc.1016.t

S <- (2*mc.1016.b$SS) + mc.1016.b$SR
R <- (2*mc.1016.b$RR) + mc.1016.b$SR
mc.1016.b <- cbind(mc.1016.b, S, R); mc.1016.b

########################################################################################
# Count genotypes 
#for whole year
countSS.trt <- sum(mc.1016.t$SS); countSS.trt
countSR.trt <- sum(mc.1016.t$SR); countSR.trt
countRR.trt <- sum(mc.1016.t$RR); countRR.trt
sum(countSS.trt, countSR.trt, countRR.trt)

countSS.buf <- sum(mc.1016.b$SS); countSS.buf
countSR.buf <- sum(mc.1016.b$SR); countSR.buf
countRR.buf <- sum(mc.1016.b$RR); countRR.buf
sum(countSS.buf, countSR.buf, countRR.buf)

# for before spray (i.e., January - March)
beforeSS.trt <- sum(mc.1016.t$SS[1:4]); beforeSS.trt
beforeSR.trt <- sum(mc.1016.t$SR[1:4]); beforeSR.trt
beforeRR.trt <- sum(mc.1016.t$RR[1:4]); beforeRR.trt
sum(beforeSS.trt, beforeSR.trt, beforeRR.trt)

beforeSS.buf <- sum(mc.1016.b$SS[1:4]); beforeSS.buf
beforeSR.buf <- sum(mc.1016.b$SR[1:4]); beforeSR.buf
beforeRR.buf <- sum(mc.1016.b$RR[1:4]); beforeRR.buf
sum(beforeSS.buf, beforeSR.buf, beforeRR.buf)

# for after spray (i.e., May - October)
afterSS.trt <- sum(mc.1016.t$SS[6:10]); afterSS.trt
afterSR.trt <- sum(mc.1016.t$SR[6:10]); afterSR.trt
afterRR.trt <- sum(mc.1016.t$RR[6:10]); afterRR.trt
sum(afterSS.trt, afterSR.trt, afterRR.trt)

afterSS.buf <- sum(mc.1016.b$SS[6:10]); afterSS.buf
afterSR.buf <- sum(mc.1016.b$SR[6:10]); afterSR.buf
afterRR.buf <- sum(mc.1016.b$RR[6:10]); afterRR.buf
sum(afterSS.buf, afterSR.buf, afterRR.buf)

########################################################################################
# Count allele frequency
#for whole year
countS.trt <- sum(mc.1016.t$S); countS.trt
countR.trt <- sum(mc.1016.t$R); countR.trt
sum(countS.trt, countR.trt)

countS.buf <- sum(mc.1016.b$S); countS.buf
countR.buf <- sum(mc.1016.b$R); countR.buf
sum(countS.buf, countR.buf)

# for before spray (i.e., January - March)
beforeS.trt <- sum(mc.1016.t$S[1:4]); beforeS.trt
beforeR.trt <- sum(mc.1016.t$R[1:4]); beforeR.trt
sum(beforeS.trt, beforeR.trt)

beforeS.buf <- sum(mc.1016.b$S[1:4]); beforeS.buf
beforeR.buf <- sum(mc.1016.b$R[1:4]); beforeR.buf
sum(beforeS.buf, beforeR.buf)

# for after spray (i.e., May - October)
afterS.trt <- sum(mc.1016.t$S[6:10]); afterS.trt
afterR.trt <- sum(mc.1016.t$R[6:10]); afterR.trt
sum(afterS.trt, afterR.trt)

afterS.buf <- sum(mc.1016.b$S[6:10]); afterS.buf
afterR.buf <- sum(mc.1016.b$R[6:10]); afterR.buf
sum(afterS.buf, afterR.buf)

########################################################################################
# Create Chi-square table to test for association between genotypes and experimental zone
# for whole year
genotypes <- matrix(c(countSS.trt, countSR.trt, countRR.trt, countSS.buf, countSR.buf, countRR.buf), ncol=3, byrow = T)
colnames(genotypes) <- c("SS", "SR", "RR")
rownames(genotypes) <- c("treatment", "buffer")
genotypes <- as.table(genotypes)
genotypes

# for before spray
gBefore <- matrix(c(beforeSS.trt, beforeSR.trt, beforeRR.trt, beforeSS.buf, beforeSR.buf, beforeRR.buf), ncol=3, byrow = T)
colnames(gBefore) <- c("SS", "SR", "RR")
rownames(gBefore) <- c("treatment", "buffer")
gBefore <- as.table(gBefore)
gBefore

# for after spray
gAfter <- matrix(c(afterSS.trt, afterSR.trt, afterRR.trt, afterSS.buf, afterSR.buf, afterRR.buf), ncol=3, byrow = T)
colnames(gAfter) <- c("SS", "SR", "RR")
rownames(gAfter) <- c("treatment", "buffer")
gAfter <- as.table(gAfter)
gAfter

# Create Chi-square table to test for association between allele frequencies and experimental zone
# for whole year
alleles <- matrix(c(countS.trt, countR.trt, countS.buf, countR.buf), ncol=2, byrow = T)
colnames(alleles) <- c("S", "R")
rownames(alleles) <- c("treatment", "buffer")
alleles <- as.table(alleles)
alleles

# for before spray
aBefore <- matrix(c(beforeS.trt, beforeR.trt, beforeS.buf, beforeR.buf), ncol=2, byrow = T)
colnames(aBefore) <- c("S", "R")
rownames(aBefore) <- c("treatment", "buffer")
aBefore <- as.table(aBefore)
aBefore

# for after spray
aAfter <- matrix(c(afterS.trt, afterR.trt, afterS.buf, afterR.buf), ncol=2, byrow = T)
colnames(aAfter) <- c("S", "R")
rownames(aAfter) <- c("treatment", "buffer")
aAfter <- as.table(aAfter)
aAfter

########################################################################################
# Perform Chi-square test
# Ho: Genotype and zone are independent
# Ha: Genotpe and zone are associated
chisq.test(genotypes)
chisq.test(gBefore)
chisq.test(gAfter)

# Ho: Allele Frequency and zone are independent
# Ha: Allele Frequency and zone are associated
chisq.test(alleles)
chisq.test(aBefore)
chisq.test(aAfter)


########################################################################################
# Add Chisq values to kdrZones figure

kdrZones
kdrZones <- kdrZones +  
  annotate("text", x=3, y=0.95, label = "Before Spray \n p-value = 0.4455", fontface = 4) +
  annotate("text", x=8, y=0.95, label = "After Spray \n p-value = 0.0002602", fontface = 4) +
  annotate("text", x=10, y=1, label = "Overall Chi-Square \n p-value = 0.0001447", fontface = 4)
kdrZones
















