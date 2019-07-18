# This is the function to calculate selection coefficient assuming partial dominance 
# of q and selection for q, where A1 = p = R, A2 = q = S
# Solves for s in Table 2.2, equation 2 - column heading "Change of gene frequency" 
# Falconer and Mackay, page 28

# To Load Required Libraries
library(scatterplot3d)


# To Calculate s, default value of h = 0.5
Part.Dom.Sel.Coef.for.q = function(q.1, q.2, g, h=0.5) {
  p.1 = 1 - q.1     # to select for q set q.1 = p, where p = 1-q 
  p.2 = 1 - q.2     # to select for q set q.2 = p, where p = 1-q
  delta.p = (p.2-p.1)/g
  s = (delta.p)/(-p.1^2 + p.1^3 - h*p.1 + 3*h*p.1^2 - 2*h*p.1^3 + (delta.p * (2*h*p.1 - 2*h*p.1^2 + p.1^2)))
}


# To plot s
plot.Part.Dom.Sel.Coef.for.q = function(q.1, q.2, g, s, dataframe) {
  delta.p = (p.2-p.1)/g
  df.name <- deparse(substitute(dataframe))
  png(file = print(paste0(df.name,"_PartDom-forq.png")), units = "px", height = 600, width = 900)
  scatterplot3d(delta.p, p.1, abs(s),highlight.3d = TRUE, col.axis = "blue", 
                cex = 2.5, cex.axis = 1.5, cex.lab = 2, cex.main = 2, col.grid = "lightblue", 
                main = "Partial Dominance of p, Selection for q", xlab = "Delta q", ylab = "", 
                zlab = "Selection Coefficient", pch = 20, zlim = c(0,.75))
  dev.off()
}