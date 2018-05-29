
library(ggplot2)

ds.allele <- structure ( c (           2000,   0.987261146,  0.012738854,  0,      0.00000000, 2000,
                                       
                                       2001,   0.991014121,  0.008985879,  0,      0.00000000, 2001,
                                       
                                       2002,   0.935714286,  0.064285714,  0,      0.00000000, 2002,
                                       
                                       2003,   0.556666667,  0.443333333,  0,      0.00000000, 2003,
                                       
                                       2004,   0.308035714,  0.691964286,  0,      0.00000000, 2004,
                                       
                                       2005,   0.012658228,  0.987341772,  0,      0.00000000, 2005,
                                       
                                       2006,   0.010204082,  0.989795918,  0,      0.00000000, 2006,
                                       
                                       2007,   0.005747126,  0.994252874,  0,      0.00000000, 2007,
                                       
                                       2008,   0.093750000,  0.906250000,  0,      0.00000000, 2008,
                                       
                                       2009,   0.000000000,  1.000000000,  0,      0.00000000, 2009,
                                       
                                       2010,   0.000000000,  0.969626168,  0,      0.03037383, 2010,
                                       
                                       2011,   0.000000000,  0.820224719,  0,      0.17977528, 2011,
                                       
                                       2012,   0.000000000,  0.639705882,  0,      0.36029412, 2012,
                                       
                                       2013,   0.003030303,  0.439393939,  0,      0.55757576, 2013,
                                       
                                       2014,   0.000000000,  0.244948532,  0,      0.75467022, 2014,
                                       
                                       2015,   0.000000000,  0.291946309,  0,      0.70805369, 2015,
                                       
                                       2016,   0.000000000,  0.364942529,  0,      0.63505747, 2016,
                                       
                                       2017,   0.001915709,  0.346743295,  0,      0.65134100, 2017), .Dim=c(6L,18L),
                                       .Dimnames=list( c( 'x', 'SS',  'SR', 'RS', 'RR',  'year') ,  NULL))

count = c( 628, 779, 70, 150, 112, 237, 49,87,96,163, 214, 89, 136, 330, 2623, 149,522, 522)

data.allele= as.data.frame(t(ds.allele))
data.allele$n=count
data.ncases= data.allele[,2:5]* count
data.ncases 

dim(data.allele)

#calculate CI

n=data.allele$n
n2 = n*2
z=1.96

## RR
data.allele$RR
p_hat.RR=data.allele$RR 
p_hat.RR
ci.lim.RR<- p_hat.RR + (z*sqrt(p_hat.RR*(1-p_hat.RR)/n2))%*%cbind(-1,1)
ci.lim.RR
data.allele$lwr.RR=ci.lim.RR[,1]
data.allele$upr.RR=ci.lim.RR[,2]

## RS
p_hat.RS=data.allele$RS 
p_hat.RS
ci.lim.RS<- p_hat.RS + (z*sqrt(p_hat.RS*(1-p_hat.RS)/n2))%*%cbind(-1,1)
ci.lim.RS
data.allele$lwr.RS=ci.lim.RS[,1]
data.allele$upr.RS=ci.lim.RS[,2]

## SR
p_hat.SR=data.allele$SR 
p_hat.SR
ci.lim.SR<- p_hat.SR + (z*sqrt(p_hat.SR*(1-p_hat.SR)/n2))%*%cbind(-1,1)
ci.lim.SR
data.allele$lwr.SR=ci.lim.SR[,1]
data.allele$upr.SR=ci.lim.SR[,2] 

## SS
p_hat.SS=data.allele$SS 
p_hat.SS
ci.lim.SS<- p_hat.SS + (z*sqrt(p_hat.SS*(1-p_hat.SS)/n2))%*%cbind(-1,1)
ci.lim.SS
data.allele$lwr.SS=ci.lim.SS[,1]
data.allele$upr.SS=ci.lim.SS[,2] 
  
#get graph
# p <- ggplot(data.allele, aes(x=year, y=p_hat.SR, ymin=lwr.SR, ymax=upr.SR)) +
#   geom_ribbon(aes(fill='red'), alpha=0.4) +
#   geom_line(aes(color=n ))
# p

gph <- ggplot(data.allele, aes(x=year ), ylim=range(0, 1)) +
  theme_bw() + #removes grey background
  theme(plot.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , panel.border = element_blank()
        , axis.line = element_line(color = 'black')) +
  scale_x_continuous(breaks = pretty(data.allele$x, n=9)) +
  scale_y_continuous(limits=c(-0.05, 1.1), breaks=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  ### Add background to represent type of insecticides used
  geom_rect(data = NULL, aes(xmin = 2001.5, xmax = 2014.5, ymin = -Inf, ymax = Inf), fill="#DDCC77", alpha = 0.03) +
  geom_rect(data = NULL, aes(xmin = 1999.0, xmax = 2001.5, ymin = -Inf, ymax = Inf), fill="#88CCEE", alpha = 0.05) +
  geom_rect(data = NULL, aes(xmin = 2014.5, xmax = 2018.0, ymin = -Inf, ymax = Inf), fill="#88CCEE", alpha = 0.05) +
  ### Add text to represent type of insecticides used
  annotate("text", x= 2008.0, y=1.09, label = "Pyrethroids", size = 5, fontface = 2) +
  annotate("text", x=2000.25, y=1.09, label = "No \n Pyrethroids", size = 5, fontface = 2) +
  annotate("text", x=2016.0, y=1.09, label = "No \n Pyrethroids", size = 5, fontface = 2) +
  ### Add data
  geom_ribbon( aes( ymin=lwr.RR, ymax=upr.RR) ,fill='#882255', alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=p_hat.RR, color=n)) +
  geom_ribbon( aes( ymin=lwr.RS, ymax=upr.RS) ,fill='#CC6677', alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=p_hat.RS, color=n)) +
  geom_ribbon( aes( ymin=lwr.SR, ymax=upr.SR) ,fill='#332288', alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=p_hat.SR, color=n)) +
  geom_ribbon( aes( ymin=lwr.SS, ymax=upr.SS) ,fill='#117733', alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=p_hat.SS, color=n)) +
  ### Add labels and theme elements
  labs(x = "Year", y = "Frequency", title = "Frequency of Resistance Allele at Two Loci", subtitle = "Error = 95% CI") +
  theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic")
        , axis.title=element_text(size=14,face="bold")
        , plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        , axis.text=element_text(size=12)) +
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size=12)) 

gph

# Write plot to png
ggsave(filename = paste0("kdrHaps_CIs.png"), width = 11, height = 8, dpi = 600, units = "in", device='png')



# library(Hmisc)
# ## get CI for proportions - method ='wilson'
# method=c("wilson","exact","asymptotic","all")
# binconf(620, 628, alpha=0.05, method="wilson",
#         include.x=FALSE, include.n=FALSE, return.df=FALSE)
# binconf(8, 628, alpha=0.05,
#         method="wilson",
#         include.x=FALSE, include.n=FALSE, return.df=FALSE)
# binconf(772, 779, alpha=0.05,
#         method="wilson",
#         include.x=FALSE, include.n=FALSE, return.df=FALSE)
# 
# ## spline curve plus  AR(1) residual term)
# library(gam)
# library(mgcv)
# set.seed(321)
# n <- length(data.allele$year)  
# time <- data.allele$year
# xt <- time/n
# y <- data.allele$SR 
# length(y)
# length(xt)
# 
# m1 <- smooth.spline(xt, y)
# m2 <- gam(y ~ s(xt, k = 10))
# m3 <- gamm(y ~ s(xt, k = 10), correlation = corAR1(form = ~ time))
# 
# edf2 <- summary(m2)$edf
# edf3 <- summary(m3$gam)$edf
# 
# plot(y ~ xt, xlab = expression(x[t]), ylab = expression(y[t]))
# lines(y ~ xt, lty = "dashed", lwd = 1)
# lines(fitted(m1) ~ xt, lty = "solid", col = "darkolivegreen", lwd = 2)
# lines(fitted(m2) ~ xt, lty = "solid", col = "red", lwd = 2)
# lines(fitted(m3$lme) ~ xt, lty = "solid", col = "midnightblue", lwd = 2)
# legend("topleft",
#        legend = c("Truth",
#                   paste("Cubic spline (edf = ", round(m1$df, 2), ")", sep = ""),
#                   paste("AM (edf = ", round(edf2, 2), ")", sep = ""),
#                   paste("AM + AR(1) (edf = ", round(edf3, 2), ")", sep = "")),
#        col = c("black", "darkgreen", "red", "midnightblue"),
#        lty = c("dashed", rep("solid", 3)),
#        lwd = c(1, rep(2, 3)),
#        bty = "n", cex = 0.8)
# 
