######################################################
# This is code to plot the haplotype frequencies
# over time and include ribbon confidence intervals.
# Code based off of pgm_CI_prop_04182018.R, which was
# given to me by Consuelo Arellano (NCSU stats prof)
######################################################

# Prep work environment ---------------------------------------------------
# clear working environment
rm(list = ls())

# Load libraries
library(ggplot2)

# Set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")


# Load and prep data for plotting -----------------------------------------
# Load data
freqAll <- read.csv("freqAll.csv")
mc.haps.yr <- read.csv("mc.haps.yr_reduced.csv")
# Load mc.1534.yr and mc.1016.yr for selection coefficient annotation
mc.1534.yr <- read.csv("mc.1534.yr_withSelCoeff.csv")
mc.1016.yr <- read.csv("mc.1016.yr_withSelCoeff.csv")

# Make new df
df <- cbind(freqAll, n=mc.haps.yr$n)


# Calculate CIs for haplotypes --------------------------------------------
# Multiply number of indiv by 2 to get number of haplotypes
n2 = df$n*2
# For 95% CI
z=1.96

## RR
p_hat.RR=df$RR 
ci.lim.RR<- p_hat.RR + (z*sqrt(p_hat.RR*(1-p_hat.RR)/n2))%*%cbind(-1,1)
df$lwr.RR=ci.lim.RR[,1]
df$upr.RR=ci.lim.RR[,2]

## RS
p_hat.RS=df$RS 
ci.lim.RS<- p_hat.RS + (z*sqrt(p_hat.RS*(1-p_hat.RS)/n2))%*%cbind(-1,1)
df$lwr.RS=ci.lim.RS[,1]
df$upr.RS=ci.lim.RS[,2]

## SR
p_hat.SR=df$SR 
ci.lim.SR<- p_hat.SR + (z*sqrt(p_hat.SR*(1-p_hat.SR)/n2))%*%cbind(-1,1)
df$lwr.SR=ci.lim.SR[,1]
df$upr.SR=ci.lim.SR[,2] 

## SS
p_hat.SS=df$SS 
ci.lim.SS<- p_hat.SS + (z*sqrt(p_hat.SS*(1-p_hat.SS)/n2))%*%cbind(-1,1)
df$lwr.SS=ci.lim.SS[,1]
df$upr.SS=ci.lim.SS[,2] 



# Plotting the graph ------------------------------------------------------
# specify haplotypes and colors for plot
cols <- c("wt1016 & wt1534"="#117733", "wt1016 & Cys1534"="#332288"
          , "Ile1016 & wt1534"="#CC6677", "Ile1016 & Cys1534"="#882255")

# plot the data
kdrHaps_ribbons <- ggplot(df, aes(x=year), ylim=range(0, 1)) +
  theme_bw() + #removes grey background
  theme(plot.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , panel.border = element_blank()
        , axis.line = element_line(color = 'black')) +
  scale_x_continuous(breaks = pretty(df$year, n=9)) +
  scale_y_continuous(limits=c(-0.05, 1.1), breaks=c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)) +
  
  ### Add background to represent type of insecticides used
  geom_rect(data = NULL, aes(xmin = 2001.5, xmax = 2014.5, ymin = -Inf, ymax = Inf), fill="#DDCC77", alpha = 0.03) +
  geom_rect(data = NULL, aes(xmin = 1999.0, xmax = 2001.5, ymin = -Inf, ymax = Inf), fill="#88CCEE", alpha = 0.05) +
  geom_rect(data = NULL, aes(xmin = 2014.5, xmax = 2018.0, ymin = -Inf, ymax = Inf), fill="#88CCEE", alpha = 0.05) +
  
  ### Add text to represent type of insecticides used
  annotate("text", x=2008.0, y=1.09, label = "Pyrethroids", size = 5, fontface = 2) +
  annotate("text", x=2000.25, y=1.09, label = "No \n Pyrethroids", size = 5, fontface = 2) +
  annotate("text", x=2016.0, y=1.09, label = "No \n Pyrethroids", size = 5, fontface = 2) +
  
  ### Add n to each year
  annotate("text", x=1999.3, y=-0.03, label = "n =", fontface = 2, color = "#332288") +
  annotate("text", x=2000, y=-0.03, label = mc.haps.yr$n[1], fontface = 2, color = "#332288") +
  annotate("text", x=2001, y=-0.03, label = mc.haps.yr$n[2], fontface = 2, color = "#332288") +
  annotate("text", x=2002, y=-0.03, label = mc.haps.yr$n[3], fontface = 2, color = "#332288") +
  annotate("text", x=2003, y=-0.03, label = mc.haps.yr$n[4], fontface = 2, color = "#332288") +
  annotate("text", x=2004, y=-0.03, label = mc.haps.yr$n[5], fontface = 2, color = "#332288") +
  annotate("text", x=2005, y=-0.03, label = mc.haps.yr$n[6], fontface = 2, color = "#332288") +
  annotate("text", x=2006, y=-0.03, label = mc.haps.yr$n[7], fontface = 2, color = "#332288") +
  annotate("text", x=2007, y=-0.03, label = mc.haps.yr$n[8], fontface = 2, color = "#332288") +
  annotate("text", x=2008, y=-0.03, label = mc.haps.yr$n[9], fontface = 2, color = "#332288") +
  annotate("text", x=2009, y=-0.03, label = mc.haps.yr$n[10], fontface = 2, color = "#332288") +
  annotate("text", x=2010, y=-0.03, label = mc.haps.yr$n[11], fontface = 2, color = "#332288") +
  annotate("text", x=2011, y=-0.03, label = mc.haps.yr$n[12], fontface = 2, color = "#332288") +
  annotate("text", x=2012, y=-0.03, label = mc.haps.yr$n[13], fontface = 2, color = "#332288") +
  annotate("text", x=2013, y=-0.03, label = mc.haps.yr$n[14], fontface = 2, color = "#332288") +
  annotate("text", x=2014, y=-0.03, label = mc.haps.yr$n[15], fontface = 2, color = "#332288") +
  annotate("text", x=2015, y=-0.03, label = mc.haps.yr$n[16], fontface = 2, color = "#332288") +
  annotate("text", x=2016, y=-0.03, label = mc.haps.yr$n[17], fontface = 2, color = "#332288") +
  annotate("text", x=2017, y=-0.03, label = mc.haps.yr$n[18], fontface = 2, color = "#332288") +
  
  ### Add data
  geom_ribbon( aes( ymin=lwr.SS, ymax=upr.SS, fill="wt1016 & wt1534") , alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=p_hat.SS)) +
  geom_ribbon( aes( ymin=lwr.SR, ymax=upr.SR, fill="wt1016 & Cys1534") , alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=p_hat.SR)) +
  geom_ribbon( aes( ymin=lwr.RS, ymax=upr.RS, fill="Ile1016 & wt1534") , alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=p_hat.RS)) +
  geom_ribbon( aes( ymin=lwr.RR, ymax=upr.RR, fill="Ile1016 & Cys1534") , alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=p_hat.RR)) +
  
  # Add Legend
  scale_fill_manual(name="Haplotypes",values=cols,breaks=c("wt1016 & wt1534","wt1016 & Cys1534","Ile1016 & wt1534","Ile1016 & Cys1534")) +
  
  # ### Add selection coefficient to select years
  # annotate("text", x=2000.9, y = 0.08, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[2], digits = 2), color = "#332288", fontface = 2, size = 6) +
  # annotate("text", x=2001.8, y = 0.28, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[3], digits = 2), color = "#332288", fontface = 2, size = 6) +
  # annotate("text", x=2002.7, y = 0.6, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[4], digits = 2), color = "#332288", fontface = 2, size = 6) +
  # annotate("text", x=2003.4, y = 0.85, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[5], digits = 2), color = "#332288", fontface = 2, size = 6) +
  # 
  # annotate("text", x=2009.85, y = 0.12, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[11], digits = 2), color = "#882255", fontface = 2, size = 6) +
  # annotate("text", x=2010.85, y = 0.25, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[12], digits = 2), color = "#882255", fontface = 2, size = 6) +
  # annotate("text", x=2011.85, y = 0.44, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[13], digits = 2), color = "#882255", fontface = 2, size = 6) +
  # annotate("text", x=2012.7, y = 0.65, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[14], digits = 2), color = "#882255", fontface = 2, size = 6) +
  # # annotate("text", x=2014.7, y = 0.8, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[15], digits = 2), color = "#882255", fontface = 2, size = 7) +
  # # annotate("text", x=2015.7, y = 0.8, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[16], digits = 2), color = "#882255", fontface = 2, size = 7) +
  # # annotate("text", x=2016.6, y = 0.7, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[17], digits = 2), color = "#882255", fontface = 2, size = 7) +

  ### Add labels and theme elements
  labs(x = "Year", y = "Frequency"
       , title = "Frequency of Resistance Allele at Two Loci"
       , subtitle = "Entire City") +
  theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic")
        , axis.title=element_text(size=14,face="bold")
        , plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        , axis.text=element_text(size=12)) +
  theme(legend.title = element_text(size = 14, hjust=0.5, face = "bold"), legend.text = element_text(size=12)) 

# print graph to screen
kdrHaps_ribbons


# Saving the graph ----------------------------------------------------------
# Write plot to png
ggsave(filename = paste0("./figures/kdrHaps/kdrHaps_ribbons/kdrHaps_ribbons_", Sys.Date(), ".png")
       , width = 11, height = 8, dpi = 600, units = "in", device='png')

