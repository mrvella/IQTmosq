### Plot Frequency of Resistance Allele at 1016 locus by zone
# Plot based off of dataframes mc.1016.t and mc.1016.b
# Last update: 23 Oct 2017

# Read in dataframes
mc.1016.t <- read.csv("mc.1016.t.csv")
mc.1016.b <- read.csv("mc.1016.b.csv")

# Plot
kdrZones <- ggplot(mc.1016.t, aes(x=month, y=freqR)) +
  theme(plot.background = element_blank()
        , panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , panel.border = element_blank()
        , axis.line = element_line(color = 'dark grey')
        , plot.subtitle=element_text(size=18, hjust=0.5, face="italic")
        , axis.title=element_text(size=14,face="bold")
        , plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        , axis.text=element_text(size=12)
        , legend.title = element_text(size = 14, face = "bold")
        , legend.text = element_text(size=12)) +
  
  labs(x = "Month", y = "Frequency", title = "Frequency of Ile1016/Cys1534 Haplotype \n By Treatment Group"
       #, subtitle = "Error Bars = 95% CI"
       ) +
  scale_color_manual(name = "Treatment"
                     , values = c("Treatment" = "red", "Buffer" = "blue")
                     , labels = c("Spray Zone", "Buffer Zone")
                     , breaks=c("Treatment","Buffer")) +
  #Add yellow background to represent spray periods
  geom_rect(data = NULL, aes(xmin = 1.95, xmax = 2.05, ymin = -Inf, ymax = Inf)
            , fill="yellow", alpha = 0.05) +
  annotate("text", x=2, y=0.5, label= "City Wide", size = 5) +
  geom_rect(data = NULL, aes(xmin = 4.6, xmax = 5.4, ymin = -Inf, ymax = Inf)
            , fill="yellow", alpha = 0.05) +
  annotate("text", x=5, y=0.5, label= "Experimental", size = 5) +
  #Add data
  geom_point(data = mc.1016.t, aes(color = "Treatment"), size = 5) +
  geom_line(data = mc.1016.t[!is.na(mc.1016.t$freqR),], color = "red", size = 2) +
  geom_errorbar(data = mc.1016.t, aes(ymin=freqR-CI_95, ymax=freqR+CI_95), width=.2
                , color = " red", size = 0.7) +
  geom_point(data = mc.1016.b, aes(color = "Buffer"), size = 5) +
  geom_line(data = mc.1016.b[!is.na(mc.1016.b$freqR),], color = "blue", size = 2) +
  geom_errorbar(data = mc.1016.b, aes(ymin=freqR-CI_95, ymax=freqR+CI_95), width=.2
                , color = "blue", size = 0.7) +
  # #Add n labels for each point
  # annotate("text", x=1, y=0.53, label = mc.1016.t$n[1], color = "blue", fontface = 2) +
  # annotate("text", x=2, y=0.53, label = mc.1016.t$n[2], color = "blue", fontface = 2) +
  # annotate("text", x=4, y=0.53, label = mc.1016.t$n[4], color = "blue", fontface = 2) +
  # annotate("text", x=5, y=0.53, label = mc.1016.t$n[5], color = "blue", fontface = 2) +
  # annotate("text", x=6, y=0.53, label = mc.1016.t$n[6], color = "blue", fontface = 2) +
  # annotate("text", x=7, y=0.53, label = mc.1016.t$n[7], color = "blue", fontface = 2) +
  # annotate("text", x=8, y=0.53, label = mc.1016.t$n[8], color = "blue", fontface = 2) +
  # annotate("text", x=10, y=0.53, label = mc.1016.t$n[10], color = "blue", fontface = 2) +
  # annotate("text", x=1, y=0.5, label = mc.1016.b$n[1], color = "dark green", fontface = 2) +
  # annotate("text", x=2, y=0.5, label = mc.1016.b$n[2], color = "dark green", fontface = 2) +
  # annotate("text", x=4, y=0.5, label = mc.1016.b$n[4], color = "dark green", fontface = 2) +
  # annotate("text", x=5, y=0.5, label = mc.1016.b$n[5], color = "dark green", fontface = 2) +
  # annotate("text", x=6, y=0.5, label = mc.1016.b$n[6], color = "dark green", fontface = 2) +
  # annotate("text", x=7, y=0.5, label = mc.1016.b$n[7], color = "dark green", fontface = 2) +
  # annotate("text", x=8, y=0.5, label = mc.1016.b$n[8], color = "dark green", fontface = 2) +
  # annotate("text", x=10, y=0.5, label = mc.1016.b$n[10], color = "dark green", fontface = 2) +

  ylim(c(0.5,1)) +
  #xlim(c(1,11)) +
  scale_x_continuous(breaks = pretty(mc.1016.t$month, n = 10)) 


# View plot
kdrZones
 
# Write plot to png
ggsave(filename = paste0("figures/kdrZones/kdrZones_", Sys.Date(), ".png"), width = 11, height = 8, dpi = 600, units = "in", device='png')
 
# 
# # Write plot to pdf
# pdf(file = paste("figures/kdrZones/kdrZones_", Sys.Date(), ".pdf", sep = ""), 11, 8.5)
# print(kdrZones)
# dev.off()
