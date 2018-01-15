# This is the function to calculate selection coefficient assuming no dominance of either allele and selection for p
# Solves for s in Table 2.2, equation 1 - column heading "Change of gene frequency" 
# Falconer and Mackay, page 28

# To Load Required Libraries
library(scatterplot3d)

# To Calculate s
NoDom.Sel.Coef = function (q.1, q.2, g) {
  delta.q = (q.2-q.1)/g
  s = (delta.q)/(q.1 *(0.5*q.1+delta.q-0.5))
}

# To plot s
plot.NoDom.Sel.Coef = function(q.1, q.2, g, s, dataframe) {
  delta.q = (q.2-q.1)/g
  df.name <- deparse(substitute(dataframe))
  png(file = print(paste0(df.name,"NoDom.png")), units = "px", height = 600, width = 900)
  scatterplot3d(delta.q, q.1, abs(s),highlight.3d = TRUE, col.axis = "blue", 
                cex = 2.5, cex.axis = 1.5, cex.lab = 2, cex.main = 2, col.grid = "lightblue", 
                main = "Co-dominance of p and q", xlab = "Delta q", ylab = "",
                zlab = "Selection Coefficient", pch = 20, zlim = c(0,.75))
  dev.off()
}
