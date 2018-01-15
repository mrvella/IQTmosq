### Plot Frequency of Resistance Alleles at two loci across years
# Plot based off of dataframes mc.1016.yr and mc.1534.yr
# mc.1016.yr <- read.csv("mc.1016.yr.csv")
# mc.1534.yr <- read.csv("mc.1534.yr.csv")

### Or plot based off reduced dataframes
# mc.1016.yr <- read.csv("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/mc.1016.yr_reduced.csv")
# mc.1534.yr <- read.csv("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database/mc.1534.yr_reduced.csv")

# Plot with ggplot()
kdrYears <- ggplot(mc.1016.yr, aes(x=year, y=freqR)) +
  #theme_bw() + #removes grey background
  theme(plot.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , panel.border = element_blank()
        , axis.line = element_line(color = 'black')) +
  #xlim(c(2000, 2017)) +
  scale_x_continuous(breaks = pretty(mc.1016.yr$year, n=8)) +
  ylim(c(-0.05, 1.1)) +

  # ### Add labels for 1534 S values
  # #annotate("text", x=2000.5, y=mc.1534.yr$freqR[1] + .05, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[1], digits=3), color = "dark green", fontface = 7) +
  # annotate("text", x=2001.5, y=mc.1534.yr$freqR[2] + .05, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[2], digits=3), color = "dark green", fontface = 7) +
  # annotate("text", x=2002.1, y=mc.1534.yr$freqR[3] + .15, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[3], digits=3), color = "dark green", fontface = 7) +
  # annotate("text", x=2003.0, y=mc.1534.yr$freqR[4] + .2, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[4], digits=3), color = "dark green", fontface = 7) +
  # annotate("text", x=2004.4, y=mc.1534.yr$freqR[5] + .1, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[5], digits=3), color = "dark green", fontface = 7) +
  # # annotate("text", x=2005.5, y=mc.1534.yr$freqR[6] + .1, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[6], digits=3), color = "dark green", fontface = 7) +
  # # annotate("text", x=2006.5, y=mc.1534.yr$freqR[7] + .1, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[7], digits=3), color = "dark green", fontface = 7) +
  # # annotate("text", x=2007.5, y=mc.1534.yr$freqR[8] + .1, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[8], digits=3), color = "dark green", fontface = 7) +
  # # annotate("text", x=2008.5, y=mc.1534.yr$freqR[9] + .1, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[9], digits=3), color = "dark green", fontface = 7) +
  # # annotate("text", x=2009.5, y=mc.1534.yr$freqR[10] + .1, label = round(mc.1534.yr$Dom.Sel.Coef.for.q[10], digits=3), color = "dark green", fontface = 7) +
  #
  #
  # ### Add labels for 1016 S values
  # annotate("text", x=2010.0, y=mc.1016.yr$freqR[11] + .1, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[11], digits=2), color = "blue", fontface = 7) +
  # annotate("text", x=2011.0, y=mc.1016.yr$freqR[12] + .15, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[12], digits=2), color = "blue", fontface = 7) +
  # annotate("text", x=2012.5, y=mc.1016.yr$freqR[13] + .2, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[13], digits=2), color = "blue", fontface = 7) +
  # annotate("text", x=2015.1, y=mc.1016.yr$freqR[14] - .02, label = round(mc.1016.yr$Dom.Sel.Coef.for.q[14], digits=2), color = "blue", fontface = 7) +


  ### Add background to represent type of insecticides used
  # Delatmethrin 2002 - 2006
  geom_rect(data = NULL, aes(xmin = 2001.5, xmax = 2006.5, ymin = -Inf, ymax = Inf), fill="lightgoldenrod", alpha = 0.05) +
  # Cypermethrin 2006 - 2008
  geom_rect(data = NULL, aes(xmin = 2006.5, xmax = 2008.5, ymin = -Inf, ymax = Inf), fill="orange", alpha = 0.05) +
  # Cypermethrin 2014
  geom_rect(data = NULL, aes(xmin = 2013.5, xmax = 2014.5, ymin = -Inf, ymax = Inf), fill="orange", alpha = 0.05) +
  # a-Cypermethrin 2008 - 2010
  geom_rect(data = NULL, aes(xmin = 2008.5, xmax = 2010.5, ymin = -Inf, ymax = Inf), fill="tomato", alpha = 0.05) +
  # a-Cypermethrin 2013
  geom_rect(data = NULL, aes(xmin = 2012.5, xmax = 2013.5, ymin = -Inf, ymax = Inf), fill="tomato", alpha = 0.05) +
  # No pyrethroids
  geom_rect(data = NULL, aes(xmin = 2014.5, xmax = 2016.5, ymin = -Inf, ymax = Inf), fill="turquoise", alpha = 0.05) +


  #Add background to represent Dengue spray periods as described in Stoddard et al 2014
  #and from our experiment
  geom_rect(data = NULL, aes(xmin = 2002.8110, xmax = 2003.1123, ymin = -Inf, ymax = Inf), fill="lightgoldenrod3", alpha = 0.05) +
  geom_rect(data = NULL, aes(xmin = 2004.9178, xmax = 2005.0137, ymin = -Inf, ymax = Inf), fill="lightgoldenrod3", alpha = 0.05) +
  geom_rect(data = NULL, aes(xmin = 2007.9890, xmax = 2008.1836, ymin = -Inf, ymax = Inf), fill="orange3", alpha = 0.05) +
  geom_rect(data = NULL, aes(xmin = 2008.8027, xmax = 2008.8411, ymin = -Inf, ymax = Inf), fill="tomato3", alpha = 0.05) +
  geom_rect(data = NULL, aes(xmin = 2009.0986, xmax = 2009.1425, ymin = -Inf, ymax = Inf), fill="tomato3", alpha = 0.05) +
  geom_rect(data = NULL, aes(xmin = 2010.1781, xmax = 2010.2356, ymin = -Inf, ymax = Inf), fill="tomato3", alpha = 0.05) +
  geom_rect(data = NULL, aes(xmin = 2014.3233, xmax = 2014.4192, ymin = -Inf, ymax = Inf), fill="orange3", alpha = 0.05) +
  geom_rect(data = NULL, aes(xmin = 2013.3068, xmax = 2013.4219, ymin = -Inf, ymax = Inf), fill="tomato3", alpha = 0.05) +

  ### Add text to represent type of insecticides used
  annotate("text", x=2004, y=1.1, label= "Deltamethrin", size = 5) +
  annotate("text", x=2007.5, y=1.1, label= "Cyper- \n methrin", size = 5) +
  annotate("text", x=2014, y=1.1, label= "Cyper- \n methrin \n 20%", size = 4) +
  annotate("text", x=2013, y=1.1, label= "a-Cyper- \n methrin \n 10%", size = 4) +
  annotate("text", x=2009.5, y=1.1, label= "a-Cyper- \n methrin", size = 5) +
  annotate("text", x = 2015.5, y=1.1, label = "No \n Pyrethroids", size = 5) +

  
  ### Add Data
  geom_point(data = mc.1016.yr, aes(color = "1016"), size = 4) +
  geom_line(data = mc.1016.yr[!is.na(mc.1016.yr$freqR),], color = "blue", size = 2) +
  geom_errorbar(data = mc.1016.yr, aes(ymin=mc.1016.yr$freqR-CI_95, ymax=mc.1016.yr$freqR+CI_95), width=.2, color = " blue", size = 0.7) +
  geom_point(data = mc.1534.yr, aes(color = "1534"), size = 4) +
  geom_line(data = mc.1534.yr[!is.na(mc.1534.yr$freqR),], color = "dark green", size = 2) +
  geom_errorbar(data = mc.1534.yr, aes(ymin=mc.1534.yr$freqR-CI_95, ymax=mc.1534.yr$freqR+CI_95), width=.2, color = "dark green", size = 0.7) +
  labs(x = "Year", y = "Frequency", title = "Frequency of Resistance Allele at Two Loci", subtitle = "Error Bars = 95% CI") +
  theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic")
        , axis.title=element_text(size=14,face="bold")
        , plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        , axis.text=element_text(size=12)
        #, plot.margin = unit(c(.5,.5,.5,.5), "in")
        ) +


  ### Add n labels for each point
  annotate("text", x=2000, y=(mc.1016.yr$freqR[1] + 0.05), label = mc.1016.yr$n[1], color = "blue", fontface = 2) +
  annotate("text", x=2001, y=(mc.1016.yr$freqR[2] + 0.05), label = mc.1016.yr$n[2], color = "blue", fontface = 2) +
  annotate("text", x=2002, y=(mc.1016.yr$freqR[3] + 0.05), label = mc.1016.yr$n[3], color = "blue", fontface = 2) +
  annotate("text", x=2003, y=(mc.1016.yr$freqR[4] + 0.05), label = mc.1016.yr$n[4], color = "blue", fontface = 2) +
  annotate("text", x=2004, y=(mc.1016.yr$freqR[5] + 0.05), label = mc.1016.yr$n[5], color = "blue", fontface = 2) +
  annotate("text", x=2005, y=(mc.1016.yr$freqR[6] + 0.05), label = mc.1016.yr$n[6], color = "blue", fontface = 2) +
  annotate("text", x=2006, y=(mc.1016.yr$freqR[7] + 0.05), label = mc.1016.yr$n[7], color = "blue", fontface = 2) +
  annotate("text", x=2007, y=(mc.1016.yr$freqR[8] + 0.05), label = mc.1016.yr$n[8], color = "blue", fontface = 2) +
  annotate("text", x=2008, y=(mc.1016.yr$freqR[9] + 0.05), label = mc.1016.yr$n[9], color = "blue", fontface = 2) +
  annotate("text", x=2009, y=(mc.1016.yr$freqR[10] + 0.05), label = mc.1016.yr$n[10], color = "blue", fontface = 2) +
  annotate("text", x=2010, y=(mc.1016.yr$freqR[11] + 0.05), label = mc.1016.yr$n[11], color = "blue", fontface = 2) +
  annotate("text", x=2011, y=(mc.1016.yr$freqR[12] + 0.05), label = mc.1016.yr$n[12], color = "blue", fontface = 2) +
  annotate("text", x=2012, y=(mc.1016.yr$freqR[13] + 0.05), label = mc.1016.yr$n[13], color = "blue", fontface = 2) +
  #annotate("text", x=2013, y=(mc.1016.yr$freqR[14] + 0.05), label = mc.1016.yr$n[14], color = "blue", fontface = 2) +
  annotate("text", x=2014, y=(mc.1016.yr$freqR[15] + 0.05), label = mc.1016.yr$n[15], color = "blue", fontface = 2) +
  #annotate("text", x=2015, y=(mc.1016.yr$freqR[16] + 0.05), label = mc.1016.yr$n[16], color = "blue", fontface = 2) +
  annotate("text", x=2016, y=(mc.1016.yr$freqR[17] + 0.05), label = mc.1016.yr$n[17], color = "blue", fontface = 2) +
  annotate("text", x=2000, y=(mc.1534.yr$freqR[1] + 0.05), label = mc.1534.yr$n[1], color = "dark green", fontface = 2) +
  annotate("text", x=2001, y=(mc.1534.yr$freqR[2] + 0.05), label = mc.1534.yr$n[2], color = "dark green", fontface = 2) +
  annotate("text", x=2002, y=(mc.1534.yr$freqR[3] + 0.05), label = mc.1534.yr$n[3], color = "dark green", fontface = 2) +
  annotate("text", x=2003, y=(mc.1534.yr$freqR[4] + 0.05), label = mc.1534.yr$n[4], color = "dark green", fontface = 2) +
  annotate("text", x=2004, y=(mc.1534.yr$freqR[5] + 0.05), label = mc.1534.yr$n[5], color = "dark green", fontface = 2) +
  annotate("text", x=2005, y=(mc.1534.yr$freqR[6] + 0.05), label = mc.1534.yr$n[6], color = "dark green", fontface = 2) +
  annotate("text", x=2006, y=(mc.1534.yr$freqR[7] + 0.05), label = mc.1534.yr$n[7], color = "dark green", fontface = 2) +
  annotate("text", x=2007, y=(mc.1534.yr$freqR[8] + 0.05), label = mc.1534.yr$n[8], color = "dark green", fontface = 2) +
  annotate("text", x=2008, y=(mc.1534.yr$freqR[9] + 0.05), label = mc.1534.yr$n[9], color = "dark green", fontface = 2) +
  annotate("text", x=2009, y=(mc.1534.yr$freqR[10] + 0.05), label = mc.1534.yr$n[10], color = "dark green", fontface = 2) +
  annotate("text", x=2010, y=(mc.1534.yr$freqR[11] + 0.05), label = mc.1534.yr$n[11], color = "dark green", fontface = 2) +
  annotate("text", x=2011, y=(mc.1534.yr$freqR[12] + 0.05), label = mc.1534.yr$n[12], color = "dark green", fontface = 2) +
  annotate("text", x=2012, y=(mc.1534.yr$freqR[13] + 0.05), label = mc.1534.yr$n[13], color = "dark green", fontface = 2) +
  #annotate("text", x=2013, y=(mc.1534.yr$freqR[14] + 0.05), label = mc.1534.yr$n[14], color = "dark green", fontface = 2) +
  annotate("text", x=2014, y=(mc.1534.yr$freqR[15] + 0.05), label = mc.1534.yr$n[15], color = "dark green", fontface = 2) +
  #annotate("text", x=2015, y=(mc.1534.yr$freqR[16] + 0.05), label = mc.1534.yr$n[16], color = "dark green", fontface = 2) +
  annotate("text", x=2016, y=(mc.1534.yr$freqR[17] + 0.05), label = mc.1534.yr$n[17], color = "dark green", fontface = 2)

  scale_color_manual(name = "Locus", values = c("1016" = "blue", "1534" = "dark green")) +
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size=12)) 

 kdrYears

# # Write plot to pdf
# pdf(file = paste("figures/kdrYears_reduced", Sys.Date(), ".pdf", sep = ""), 11, 8.5)
# print(kdrYears)
# dev.off()

#ggsave(file=paste0("figures/kdrYears_", Sys.Date(), ".png"), plot=kdrYears)
