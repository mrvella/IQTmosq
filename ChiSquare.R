# Chi-square Test between spray and buffer treatments for each month.
# Started 4 April 2018

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
mc.1016.2014

# Make a contingency table
row.b <- as.data.frame(cbind(mc.1016.2014$month[1:8], mc.1016.2014$R[1:8], mc.1016.2014$S[1:8]))
colnames(row.b) <- c("month", "R", "S")
row.b

row.t <- as.data.frame(cbind(mc.1016.2014$month[1:8], mc.1016.2014$R[9:16], mc.1016.2014$S[9:16]))
colnames(row.t) <- c("month", "R", "S")
row.t

# for january
tbl.1 <- rbind(row.b[1,2:3], row.t[1,2:3])  
rownames(tbl.1) <- c("Buffer", "Spray")
tbl.1

# for febrary
tbl.2 <- rbind(row.b[2,2:3], row.t[2,2:3])  
rownames(tbl.2) <- c("Buffer", "Spray")
tbl.2

# for april
tbl.4 <- rbind(row.b[3,2:3], row.t[3,2:3])  
rownames(tbl.4) <- c("Buffer", "Spray")
tbl.4

# for may
tbl.5 <- rbind(row.b[4,2:3], row.t[4,2:3])  
rownames(tbl.5) <- c("Buffer", "Spray")
tbl.5

# for june
tbl.6 <- rbind(row.b[5,2:3], row.t[5,2:3])  
rownames(tbl.6) <- c("Buffer", "Spray")
tbl.6

# for july
tbl.7 <- rbind(row.b[6,2:3], row.t[6,2:3])  
rownames(tbl.7) <- c("Buffer", "Spray")
tbl.7

# for august
tbl.8 <- rbind(row.b[7,2:3], row.t[7,2:3])  
rownames(tbl.8) <- c("Buffer", "Spray")
tbl.8

# for october
tbl.10 <- rbind(row.b[8,2:3], row.t[8,2:3])  
rownames(tbl.10) <- c("Buffer", "Spray")
tbl.10


# Perform X^2
chisq.test(tbl.1)
chisq.test(tbl.2)
chisq.test(tbl.4)
chisq.test(tbl.5)
chisq.test(tbl.6)
chisq.test(tbl.7)
chisq.test(tbl.8)
chisq.test(tbl.10)








