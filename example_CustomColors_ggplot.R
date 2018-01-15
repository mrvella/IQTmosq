### Code to use specific colors in ggplot

#Some test data
dat <- data.frame(x=runif(10),y=runif(10),
                  grp = rep(LETTERS[1:5],each = 2),stringsAsFactors = TRUE)

#Create a custom color scale
library(RColorBrewer)
library(ggplot2)
#myColors <- brewer.pal(5,"Set1")
myColors <- c("#a3c1ad", "#004d4d", "#875d70", "#5c1414", "#a38c6f" )
names(myColors) <- levels(dat$grp)
colScale <- scale_colour_manual(name = "grp",values = myColors)

#One plot with all the data
p <- ggplot(dat,aes(x,y,colour = grp)) + geom_point()
p1 <- p + colScale

#A second plot with only four of the levels
p2 <- p %+% droplevels(subset(dat[4:10,])) + colScale

p1
p2
