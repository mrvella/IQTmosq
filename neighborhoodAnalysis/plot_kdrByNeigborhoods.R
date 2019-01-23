######################################################
# This is code to separate kdr haplotype data by
# neighborhood in order to plot haplotype frequencies
# over time on smaller scale.
######################################################

# Prep work environment ---------------------------------------------------

# Set working directory
setwd("/Users/jenbaltz/Dropbox/GouldLab/Project_Mosquito/Database")


# # # Load and prep data ---------------------------------------------------------------
# # top <- read.csv("freqAll_top.csv")
# # mid <- read.csv("freqAll_mid.csv")
# # bot <- read.csv("freqAll_bot.csv")
# # top.R <- read.csv("freqAll_top.R.csv")
# # top.L <- read.csv("freqAll_top.L.csv")
# 
# # Calculate CIs for top ---------------------------------------------------
# # Multiply number of indiv by 2 to get number of haplotypes
# n2 = top$n*2
# # For 95% CI
# z=1.96
# 
# ## RR
# # top$RR = p_hat for RR
# ci.lim.RR<- top$RR + (z*sqrt(top$RR*(1-top$RR)/n2))%*%cbind(-1,1)
# top$lwr.RR=ci.lim.RR[,1]
# top$upr.RR=ci.lim.RR[,2]
# 
# ## RS
# ci.lim.RS<- top$RS + (z*sqrt(top$RS*(1-top$RS)/n2))%*%cbind(-1,1)
# top$lwr.RS=ci.lim.RS[,1]
# top$upr.RS=ci.lim.RS[,2]
# 
# ## SR
# ci.lim.SR<- top$SR + (z*sqrt(top$SR*(1-top$SR)/n2))%*%cbind(-1,1)
# top$lwr.SR=ci.lim.SR[,1]
# top$upr.SR=ci.lim.SR[,2]
# 
# ## SS
# ci.lim.SS<- top$SS + (z*sqrt(top$SS*(1-top$SS)/n2))%*%cbind(-1,1)
# top$lwr.SS=ci.lim.SS[,1]
# top$upr.SS=ci.lim.SS[,2]
# 
# 
# # Calculate CIs for top.R -------------------------------------------------
# # Multiply number of indiv by 2 to get number of haplotypes
# n2 = top.R$n*2
# # For 95% CI
# z=1.96
# 
# ## RR
# ci.lim.RR<- top.R$RR + (z*sqrt(top.R$RR*(1-top.R$RR)/n2))%*%cbind(-1,1)
# top.R$lwr.RR=ci.lim.RR[,1]
# top.R$upr.RR=ci.lim.RR[,2]
# 
# ## RS
# ci.lim.RS<- top.R$RS + (z*sqrt(top.R$RS*(1-top.R$RS)/n2))%*%cbind(-1,1)
# top.R$lwr.RS=ci.lim.RS[,1]
# top.R$upr.RS=ci.lim.RS[,2]
# 
# ## SR
# ci.lim.SR<- top.R$SR + (z*sqrt(top.R$SR*(1-top.R$SR)/n2))%*%cbind(-1,1)
# top.R$lwr.SR=ci.lim.SR[,1]
# top.R$upr.SR=ci.lim.SR[,2] 
# 
# ## SS
# ci.lim.SS<- top.R$SS + (z*sqrt(top.R$SS*(1-top.R$SS)/n2))%*%cbind(-1,1)
# top.R$lwr.SS=ci.lim.SS[,1]
# top.R$upr.SS=ci.lim.SS[,2] 
# 
# 
# # Calculate CIs for top.L -------------------------------------------------
# # Multiply number of indiv by 2 to get number of haplotypes
# n2 = top.L$n*2
# # For 95% CI
# z=1.96
# 
# ## RR
# # note that top.L$RR is equivalent to p_hat for RR 
# ci.lim.RR<- top.L$RR + (z*sqrt(top.L$RR*(1-top.L$RR)/n2))%*%cbind(-1,1)
# top.L$lwr.RR=ci.lim.RR[,1]
# top.L$upr.RR=ci.lim.RR[,2]
# 
# ## RS
# ci.lim.RS<- top.L$RS + (z*sqrt(top.L$RS*(1-top.L$RS)/n2))%*%cbind(-1,1)
# top.L$lwr.RS=ci.lim.RS[,1]
# top.L$upr.RS=ci.lim.RS[,2]
# 
# ## SR
# ci.lim.SR<- top.L$SR + (z*sqrt(top.L$SR*(1-top.L$SR)/n2))%*%cbind(-1,1)
# top.L$lwr.SR=ci.lim.SR[,1]
# top.L$upr.SR=ci.lim.SR[,2] 
# 
# ## SS
# ci.lim.SS<- top.L$SS + (z*sqrt(top.L$SS*(1-top.L$SS)/n2))%*%cbind(-1,1)
# top.L$lwr.SS=ci.lim.SS[,1]
# top.L$upr.SS=ci.lim.SS[,2] 
# 
# # Calculate CIs for mid ---------------------------------------------------
# # Multiply number of indiv by 2 to get number of haplotypes
# n2 = mid$n*2
# # For 95% CI
# z=1.96
# 
# ## RR
# ci.lim.RR<- mid$RR + (z*sqrt(mid$RR*(1-mid$RR)/n2))%*%cbind(-1,1)
# mid$lwr.RR=ci.lim.RR[,1]
# mid$upr.RR=ci.lim.RR[,2]
# 
# ## RS
# ci.lim.RS<- mid$RS + (z*sqrt(mid$RS*(1-mid$RS)/n2))%*%cbind(-1,1)
# mid$lwr.RS=ci.lim.RS[,1]
# mid$upr.RS=ci.lim.RS[,2]
# 
# ## SR
# ci.lim.SR<- mid$SR + (z*sqrt(mid$SR*(1-mid$SR)/n2))%*%cbind(-1,1)
# mid$lwr.SR=ci.lim.SR[,1]
# mid$upr.SR=ci.lim.SR[,2]
# 
# ## SS
# ci.lim.SS<- mid$SS + (z*sqrt(mid$SS*(1-mid$SS)/n2))%*%cbind(-1,1)
# mid$lwr.SS=ci.lim.SS[,1]
# mid$upr.SS=ci.lim.SS[,2]
# 
# 
# 
# # Calculate CIs for bot ---------------------------------------------------
# # # remove rows with NAs
# # bot <- bot[complete.cases(bot), ]
# 
# # Multiply number of indiv by 2 to get number of haplotypes
# n2 = bot$n*2
# # For 95% CI
# z=1.96
# 
# ## RR
# ci.lim.RR<- bot$RR + (z*sqrt(bot$RR*(1-bot$RR)/n2))%*%cbind(-1,1)
# bot$lwr.RR=ci.lim.RR[,1]
# bot$upr.RR=ci.lim.RR[,2]
# 
# ## RS
# ci.lim.RS<- bot$RS + (z*sqrt(bot$RS*(1-bot$RS)/n2))%*%cbind(-1,1)
# bot$lwr.RS=ci.lim.RS[,1]
# bot$upr.RS=ci.lim.RS[,2]
# 
# ## SR
# ci.lim.SR<- bot$SR + (z*sqrt(bot$SR*(1-bot$SR)/n2))%*%cbind(-1,1)
# bot$lwr.SR=ci.lim.SR[,1]
# bot$upr.SR=ci.lim.SR[,2]
# 
# ## SS
# ci.lim.SS<- bot$SS + (z*sqrt(bot$SS*(1-bot$SS)/n2))%*%cbind(-1,1)
# bot$lwr.SS=ci.lim.SS[,1]
# bot$upr.SS=ci.lim.SS[,2]


# Plotting the graph for top ------------------------------------------------------
# specify haplotypes and colors for plot
cols <- c("wt1016 & wt1534"="#117733", "wt1016 & Cys1534"="#332288"
          , "Ile1016 & wt1534"="#CC6677", "Ile1016 & Cys1534"="#882255")

# plot the data
kdrHaps <- ggplot(freqAll_long, aes(x=freqAll_long$year), ylim=range(0, 1)) +
  theme_bw() + #removes grey background
  theme(plot.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , panel.border = element_blank()
        , axis.line = element_line(color = 'black')) +
  scale_x_continuous(breaks = pretty(freqAll_long$year, n=9)) +
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
  annotate("text", x=2000, y=-0.03, label = freqAll_long$n[1], fontface = 2, color = "#332288") +
  annotate("text", x=2001, y=-0.03, label = freqAll_long$n[2], fontface = 2, color = "#332288") +
  annotate("text", x=2002, y=-0.03, label = freqAll_long$n[3], fontface = 2, color = "#332288") +
  annotate("text", x=2003, y=-0.03, label = freqAll_long$n[4], fontface = 2, color = "#332288") +
  annotate("text", x=2004, y=-0.03, label = freqAll_long$n[5], fontface = 2, color = "#332288") +
  annotate("text", x=2005, y=-0.03, label = freqAll_long$n[6], fontface = 2, color = "#332288") +
  annotate("text", x=2006, y=-0.03, label = freqAll_long$n[7], fontface = 2, color = "#332288") +
  annotate("text", x=2007, y=-0.03, label = freqAll_long$n[8], fontface = 2, color = "#332288") +
  annotate("text", x=2008, y=-0.03, label = freqAll_long$n[9], fontface = 2, color = "#332288") +
  annotate("text", x=2009, y=-0.03, label = freqAll_long$n[10], fontface = 2, color = "#332288") +
  annotate("text", x=2010, y=-0.03, label = freqAll_long$n[11], fontface = 2, color = "#332288") +
  annotate("text", x=2011, y=-0.03, label = freqAll_long$n[12], fontface = 2, color = "#332288") +
  annotate("text", x=2012, y=-0.03, label = freqAll_long$n[13], fontface = 2, color = "#332288") +
  annotate("text", x=2013, y=-0.03, label = freqAll_long$n[14], fontface = 2, color = "#332288") +
  annotate("text", x=2014, y=-0.03, label = freqAll_long$n[15], fontface = 2, color = "#332288") +
  annotate("text", x=2015, y=-0.03, label = freqAll_long$n[16], fontface = 2, color = "#332288") +
  annotate("text", x=2016, y=-0.03, label = freqAll_long$n[17], fontface = 2, color = "#332288") +
  annotate("text", x=2017, y=-0.03, label = freqAll_long$n[18], fontface = 2, color = "#332288") +

  ### Add data
  geom_ribbon( aes( ymin=freqAll_long$lwr.SS, ymax=freqAll_long$upr.SS, fill="wt1016 & wt1534") , alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=freqAll_long$SS)) +
  geom_point( aes( y=freqAll_long$SS)) +
  geom_ribbon( aes( ymin=freqAll_long$lwr.SR, ymax=freqAll_long$upr.SR, fill="wt1016 & Cys1534") , alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=freqAll_long$SR)) +
  geom_point( aes( y=freqAll_long$SR)) +
  geom_ribbon( aes( ymin=freqAll_long$lwr.RS, ymax=freqAll_long$upr.RS, fill="Ile1016 & wt1534") , alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=freqAll_long$RS)) +
  geom_point( aes( y=freqAll_long$RS)) +
  geom_ribbon( aes( ymin=freqAll_long$lwr.RR, ymax=freqAll_long$upr.RR, fill="Ile1016 & Cys1534") , alpha=0.9,  show.legend = NA) +
  geom_line( aes( y=freqAll_long$RR)) +
  geom_point( aes( y=freqAll_long$RR)) +
  
  # Add Legend
  scale_fill_manual(name="Haplotypes",values=cols,breaks=c("wt1016 & wt1534","wt1016 & Cys1534","Ile1016 & wt1534","Ile1016 & Cys1534")) +

  ### Add labels and theme elements
  geom_hline(yintercept = 0) +
  labs(x = "Year", y = "Frequency"
       , title = "Frequency of Resistance Allele at Two Loci"
       , subtitle = "North") +
  theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic")
        , axis.title=element_text(size=14,face="bold")
        , plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        , axis.text=element_text(size=12)) +
  theme(legend.title = element_text(size = 14, hjust=0.5, face = "bold"), legend.text = element_text(size=12))

# print graph to screen
kdrHaps

# # Write plot to png
# ggsave(filename = paste0("./figures/kdrHaps/kdrNeighborhoods_ribbons/top_ribbons_", Sys.Date(), ".png")
#        , width = 11, height = 8, dpi = 600, units = "in", device='png')
