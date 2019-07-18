# This script will create a results matrix that tests various values for q.1 and q.2
# applied to the selection coefficient functions.

# source functions
source("function_Part.Dom.Sel.Coef.for.q.R")
source("function_Dom.Sel.Coef.for.q.R")

# set some parameters
q.1 <- seq(from = 0, to = 1, by = 0.1)
q.2 <- seq(from = 0, to = 1, by = 0.1)
g = 1

# Test Partial Dominance of q
part.df <- round(outer(q.1, q.2, Part.Dom.Sel.Coef.for.q, g), digits = 2)
rownames(part.df) <- c("q.1=0.0", "q.1=0.1", "q.1=0.2","q.1=0.3","q.1=0.4","q.1=0.5"
                      ,"q.1=0.6","q.1=0.7","q.1=0.9","q.1=0.9","q.1=1.0")
colnames(part.df) <- c("q.2=0.0", "q.2=0.1", "q.2=0.2","q.2=0.3","q.2=0.4","q.2=0.5"
                      ,"q.2=0.6","q.2=0.7","q.2=0.9","q.2=0.9","q.2=1.0")
part.df

# # Test Dominance of q
dom.df <- round(outer(q.1, q.2, Dom.Sel.Coef.for.q, g), digits = 2)
rownames(dom.df) <- c("q.1=0.0", "q.1=0.1", "q.1=0.2","q.1=0.3","q.1=0.4","q.1=0.5"
                      ,"q.1=0.6","q.1=0.7","q.1=0.9","q.1=0.9","q.1=1.0")
colnames(dom.df) <- c("q.2=0.0", "q.2=0.1", "q.2=0.2","q.2=0.3","q.2=0.4","q.2=0.5"
                      ,"q.2=0.6","q.2=0.7","q.2=0.9","q.2=0.9","q.2=1.0")
dom.df






