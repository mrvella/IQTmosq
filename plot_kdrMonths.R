### Plot Frequency of Resistance Allele at 1016 locus across year 2014
# Plot based off of dataframe mc.1016.mo

# Read in dataframe
mc.1016.mo <- read.csv("mc.1016.mo.csv")

# Plot 
kdrMonths <- ggplot(mc.1016.mo, aes(x=month, y=freqR)) +
  theme(plot.background = element_blank(), panel.grid.major = element_blank()
        , panel.grid.minor = element_blank(), panel.border = element_blank()
        , axis.line = element_line(color = 'dark grey')) +
  ylim(c(0.5,1)) +
  xlim(c(1,10)) +
  #Add yellow background to represent spray periods
  geom_rect(data = NULL, aes(xmin = 1.95, xmax = 2.05, ymin = -Inf, ymax = Inf)
            , fill="yellow", alpha = 0.05) +
  annotate("text", x=2, y=1, label= "City Wide", size = 5) +
  geom_rect(data = NULL, aes(xmin = 4.5, xmax = 5.5, ymin = -Inf, ymax = Inf)
            , fill="yellow", alpha = 0.05) +
  annotate("text", x=5, y=1, label= "Experimental", size = 5) +
  #Add data
  geom_point(data = mc.1016.mo, aes(color = "1016"), size = 5) +
  geom_line(data = mc.1016.mo[!is.na(mc.1016.mo$freqR),], color = "blue", size = 2) +
  geom_errorbar(data = mc.1016.mo, aes(ymin=freqR-CI_95, ymax=freqR+CI_95), width=.2
                , color = " blue", size = 0.7) +
  labs(x = "Months", y = "Frequency", title = "Frequency of V1016I Resistance Allele Across 2014 "
       , subtitle = "Error Bars = 95% CI") +
  theme(plot.subtitle=element_text(size=18, hjust=0.5, face="italic")
        , axis.title=element_text(size=14,face="bold")
        , plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
        , axis.text=element_text(size=12)) +
  scale_color_manual(name = "Locus", values = c("1016" = "blue")) +
  theme(legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size=12))

# # View plot
 kdrMonths

# # Write plot to pdf
# pdf(file = paste("kdrMonths_", Sys.Date(), ".pdf", sep = ""), 11, 8.5)
# print(kdrMonths)
# dev.off()
