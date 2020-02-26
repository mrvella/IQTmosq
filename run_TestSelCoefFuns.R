# This script will create a results matrix that tests various values for q.1 and q.2
# applied to the selection coefficient functions.

### clear working environment
rm(list = ls())

### Set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/R_Scripts/IQTmosq")

# source functions
source("function_Part.Dom.Sel.Coef.for.q.R")
source("function_Dom.Sel.Coef.for.q.R")

# set some parameters
q.1 <- seq(from = 0, to = 1, by = 0.1)
q.2 <- seq(from = 0, to = 1, by = 0.1)
# g = 1

# Test Partial Dominance function ############################################
part.g1 <- round(outer(q.1, q.2, Part.Dom.Sel.Coef.for.q, g=1), digits = 2)
part.g12 <- round(outer(q.1, q.2, Part.Dom.Sel.Coef.for.q, g=12), digits = 2)
# add row and column names to matrix
rownames(part.g1) <- c("q.1=0.0", "q.1=0.1", "q.1=0.2","q.1=0.3","q.1=0.4","q.1=0.5","q.1=0.6","q.1=0.7","q.1=0.9","q.1=0.9","q.1=1.0")
colnames(part.g1) <- c("q.2=0.0", "q.2=0.1", "q.2=0.2","q.2=0.3","q.2=0.4","q.2=0.5","q.2=0.6","q.2=0.7","q.2=0.9","q.2=0.9","q.2=1.0")
rownames(part.g12) <- c("q.1=0.0", "q.1=0.1", "q.1=0.2","q.1=0.3","q.1=0.4","q.1=0.5","q.1=0.6","q.1=0.7","q.1=0.9","q.1=0.9","q.1=1.0")
colnames(part.g12) <- c("q.2=0.0", "q.2=0.1", "q.2=0.2","q.2=0.3","q.2=0.4","q.2=0.5","q.2=0.6","q.2=0.7","q.2=0.9","q.2=0.9","q.2=1.0")
# view matrices
part.g1
part.g12
# save files
write.csv(part.g1, file = "../../SelCoef_analysis_part.g1.csv")
write.csv(part.g12, file = "../../SelCoef_analysis_part.g12.csv")

# Test Dominance of q function ###############################################
dom.g1 <- round(outer(q.1, q.2, Dom.Sel.Coef.for.q, g=1), digits = 2)
dom.g12 <- round(outer(q.1, q.2, Dom.Sel.Coef.for.q, g=12), digits = 2)
# add row and column names to matrix
rownames(dom.g1) <- c("q.1=0.0", "q.1=0.1", "q.1=0.2","q.1=0.3","q.1=0.4","q.1=0.5","q.1=0.6","q.1=0.7","q.1=0.9","q.1=0.9","q.1=1.0")
colnames(dom.g1) <- c("q.2=0.0", "q.2=0.1", "q.2=0.2","q.2=0.3","q.2=0.4","q.2=0.5","q.2=0.6","q.2=0.7","q.2=0.9","q.2=0.9","q.2=1.0")
rownames(dom.g12) <- c("q.1=0.0", "q.1=0.1", "q.1=0.2","q.1=0.3","q.1=0.4","q.1=0.5","q.1=0.6","q.1=0.7","q.1=0.9","q.1=0.9","q.1=1.0")
colnames(dom.g12) <- c("q.2=0.0", "q.2=0.1", "q.2=0.2","q.2=0.3","q.2=0.4","q.2=0.5","q.2=0.6","q.2=0.7","q.2=0.9","q.2=0.9","q.2=1.0")
# view matrices
dom.g1
dom.g12
# save files
write.csv(dom.g1, file = "../../SelCoef_analysis_dom.g1.csv")
write.csv(dom.g12, file = "../../SelCoef_analysis_dom.g12.csv")






