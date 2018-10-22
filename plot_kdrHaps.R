# Plot the frequency of kdr haplotypes over time with ggplot()

# Load required libraries
library(ggplot2)
library(RColorBrewer)

# Create custom color palette
myColors <- c("#117733", "#332288", "#CC6677", "#882255")
names(myColors) <- levels(freqAll_long$Haplotype)
colScale <- scale_colour_manual(name = "Haplotype",values = myColors)

# Load mc.1534.yr and mc.1016.yr for selection coefficient annotation
mc.1534.yr <- read.csv("mc.1534.yr_withSelCoeff.csv")
mc.1016.yr <- read.csv("mc.1016.yr_withSelCoeff.csv")

##########################
# Key code for this script
##########################
# Split data at year 2013
# head(freqAll_long)

# To plot
kdrHaps <- ggplot(data = freqAll_long[!is.na(freqAll_long$Frequency),], aes(x=year, y=Frequency)) +
  theme_bw() + #removes grey background  
  theme(plot.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , panel.border = element_blank()
        , axis.line = element_line(color = 'black')) +
  # xlim(c(2000, 2017)) +
  #ylim(c(-0.1, 1.1)) +
  scale_y_continuous(limits=c(-0.05, 1.1), breaks=c(0.0,0.2, 0.4, 0.6, 0.8, 1.0)) +
  scale_x_continuous(breaks = pretty(freqAll_long$year, n = 18)) +
  #scale_colour_discrete(name ="Haplotype", labels=c("wt1016/wt1534", "wt1016/Cys1534", "Ile1016/wt1534", "Ile1016/Cys1534")) +
  #geom_hline(yintercept = 0) +

  ### Add background to represent type of insecticides used
  # Pyrethroids 2002 - 2014
  geom_rect(data = NULL, aes(xmin = 2001.5, xmax = 2013.5, ymin = -Inf, ymax = Inf), fill="#DDCC77", alpha = 0.005) +
  # No pyrethroids
  geom_rect(data = NULL, aes(xmin = 1999.0, xmax = 2001.5, ymin = -Inf, ymax = Inf), fill="#88CCEE", alpha = 0.005) +
  # # Delatmethrin 2002 - 2006
  # geom_rect(data = NULL, aes(xmin = 2001.5, xmax = 2006.0, ymin = -Inf, ymax = Inf), fill="#F0E442", alpha = 0.01) +
  # # Cypermethrin 2006 - 2008
  # geom_rect(data = NULL, aes(xmin = 2006.0, xmax = 2008.5, ymin = -Inf, ymax = Inf), fill="#E69F00", alpha = 0.01) +
  # # a-Cypermethrin 2008 - 2011
  # geom_rect(data = NULL, aes(xmin = 2008.5, xmax = 2011.5, ymin = -Inf, ymax = Inf), fill="#D55E00", alpha = 0.005) +
  # # Cypermethrin & a-Cypermethrin 2012
  # geom_rect(data = NULL, aes(xmin = 2011.5, xmax = 2012.5, ymin = -Inf, ymax = Inf), fill="turquoise", alpha = 0.005) +
  # # Lambda & Alpha & Cyper & Alpha+pyriprox in 2013
  # geom_rect(data = NULL, aes(xmin = 2012.5, xmax = 2013.5, ymin = -Inf, ymax = Inf), fill="turquoise", alpha = 0.005) +
  # # Cypermethrin 2014
  # geom_rect(data = NULL, aes(xmin = 2013.5, xmax = 2014.5, ymin = -Inf, ymax = Inf), fill="#E69F00", alpha = 0.01) +
  # No pyrethroids
  geom_rect(data = NULL, aes(xmin = 2013.5, xmax = 2018.0, ymin = -Inf, ymax = Inf), fill="#88CCEE", alpha = 0.005) +

  ### Add text to represent type of insecticides used
  annotate("text", x= 2008.0, y=1.09, label = "Pyrethroids", size = 5, fontface = 2) +
  annotate("text", x=2000.25, y=1.09, label = "No \n Pyrethroids", size = 5, fontface = 2) +
  # annotate("text", x=2003.75, y=-0.09, label= "Deltamethrin", size = 4) +
  # annotate("text", x=2007.25, y=-0.09, label= "Cypermethrin", size = 4) +
  # annotate("text", x=2010, y=-0.09, label= "alpha-Cypermethrin", size = 4) +
  # #annotate("text", x=2012, y=-0.05, label= "a- & Cyper- \n methrin", size = 4) +
  # annotate("text", x=2012.5, y=-0.09, label= "Multiple", size = 4) +
  # annotate("text", x=2014.0, y=-0.09, label = "Cyper- \n methrin", size = 4) +
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
  # annotate("text", x=2014, y=-0.03, label = mc.haps.yr$n[15], fontface = 2, color = "#332288") +
  # annotate("text", x=2015, y=-0.03, label = mc.haps.yr$n[16], fontface = 2, color = "#332288") +
  # annotate("text", x=2016, y=-0.03, label = mc.haps.yr$n[17], fontface = 2, color = "#332288") +
  # annotate("text", x=2017, y=-0.03, label = mc.haps.yr$n[18], fontface = 2, color = "#332288") +
  
  ### Add data
  geom_line(aes(colour=Haplotype), size = 2) +
  geom_point(aes(colour=Haplotype), size = 3) +
  geom_errorbar(data = freqAll_long, aes(ymin=Frequency-CI_95, ymax=Frequency+CI_95, colour=Haplotype), width=.2, size = 0.7) +
  
  labs(x = "Year", y = "Frequency", title = "kdr Haplotype Frequency"
       #, subtitle = "Error Bars = 95% CI"
       ) +
  colScale +
  
  # ### Add selection coefficient to select years
  # annotate("text", x=2001.0, y = 0.08, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[2], digits = 2), color = "#332288", fontface = 2, size = 7) +
  # annotate("text", x=2001.85, y = 0.28, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[3], digits = 2), color = "#332288", fontface = 2, size = 7) +
  # annotate("text", x=2002.8, y = 0.6, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[4], digits = 2), color = "#332288", fontface = 2, size = 7) +
  # annotate("text", x=2003.8, y = 0.85, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[5], digits = 2), color = "#332288", fontface = 2, size = 7) +
  # 
  # annotate("text", x=2009.75, y = 0.14, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[11], digits = 2), color = "#882255", fontface = 2, size = 7) +
  # annotate("text", x=2010.72, y = 0.3, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[12], digits = 2), color = "#882255", fontface = 2, size = 7) +
  # annotate("text", x=2011.75, y = 0.47, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[13], digits = 2), color = "#882255", fontface = 2, size = 7) +
  # annotate("text", x=2012.65, y = 0.65, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[14], digits = 2), color = "#882255", fontface = 2, size = 7) +
  # # annotate("text", x=2014.62, y = 0.76, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[15], digits = 2), color = "#882255", fontface = 2, size = 4) +
  # # annotate("text", x=2015.65, y = 0.7, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[16], digits = 2), color = "#882255", fontface = 2, size = 4) +
  # # annotate("text", x=2016.5, y = 0.68, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[17], digits = 2), color = "#882255", fontface = 2, size = 4) +
  
  
  theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic")
        , axis.title=element_text(size=14,face="bold")
        , plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        , axis.text=element_text(size=12)
  ) +
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size=12)
        , legend.title.align=0.5) 

# # Print graph to screen
# kdrHaps

# Write plot to png
ggsave(filename = paste0("figures/kdrHaps/kdrHaps_", Sys.Date(), ".png"), width = 11, height = 8, dpi = 600, units = "in", device='png')


# # Source function to move x-axis with labels
# source("R_Scripts/function_moveXaxis.R")
# # Plot x-axis moved up
# kdrHaps <- shift_axis(kdrHaps, 0)
# kdrHaps




