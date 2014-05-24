## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")




png(file="plot1.png", width=480,height=480)
library(gplots)

tE <- tapply(NEI$Emissions/1000,NEI$year,sum)
br <- barplot2(tE, col = 'gray',las=1, 
               main = expression("Total" ~ PM[2.5] ~ "emission from all sources"),
               plot.grid = TRUE, ylab="Emission (kt)", xlab="Years")
box()
lines(br, tE, type = 'h', col = 'red', lwd = 2)

dev.off()

##########################################################

summary(NEI)


png(file="plot2.png", width=480,height=480)

library(gplots)

NEI.24510 <- subset(NEI, fips == "24510")
tE <- tapply(NEI.24510$Emissions,NEI.24510$year,sum)
br <- barplot2(tE, col = 'gray',las=1, 
               main = expression("Total" ~ PM[2.5] ~ "emission in the Baltimore City"),
               plot.grid = TRUE, ylab="Emission (t)", xlab="Years")
box()
lines(br, tE, type = 'h', col = 'red', lwd = 2)

dev.off()

##########################################################


png(file="plot3.png", width=600,height=480)

library(reshape2)
library(ggthemes)

NEI.24510 <- subset(NEI, fips == 24510)
NEI.24510$type <- factor(NEI.24510$type)

tE <- with(NEI.24510, tapply(Emissions,list(year,type),sum))
tE <- melt(tE, varnames=c("year","type"), value.name = "Emissions")

ggplot(tE) +
  geom_bar(aes(x = factor(year), y = Emissions, fill=type),
           alpha = 0.85,
           colour="deepskyblue4",           
           stat = "identity", position="dodge") + 
  xlab("Year") + ylab("Emission (t)") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  ggtitle(expression(PM[2.5] ~ "emission in the Baltimore City, by type")) + 
  labs(fill="Type: ") +
  theme_stata() +  
  theme(axis.title.y = element_text(vjust = 1, size=15),
        axis.title.x = element_text(vjust = 1, size=15),
        legend.title = element_text(face = "bold"),
        legend.key.width = unit(1.0, "cm")
  )

dev.off()

##########################################################

library(plyr)
library(gplots)
SCC$SCC <- as.character(SCC$SCC)

SCC.1 <- SCC[grep("(Coal).*(Comb)|(Comb).*(Coal)", SCC$Short.Name),]

NEI.1 <- join(NEI, SCC.1, by = "SCC", type = "inner")

png(file="plot4.png", width=480,height=480)


tE <- tapply(NEI.1$Emissions/1000,NEI.1$year,sum)
br <- barplot2(tE, col = 'gray',las=1, 
               main = expression("Coal combustion-related" ~ PM[2.5] ~ "emissions"),
               plot.grid = TRUE, ylab="Emission (kt)", xlab="Years")
box()
lines(br, tE, type = 'h', col = 'red', lwd = 2)

dev.off()

############################################################

library(plyr)
library(gplots)
SCC$SCC <- as.character(SCC$SCC)

SCC.2 <- SCC[grep("Mobile Sources", SCC$SCC.Level.One),]

NEI.24510 <- subset(NEI, fips == "24510")
NEI.2 <- join(NEI.24510, SCC.2, by = "SCC", type = "inner")

png(file="plot5.png", width=480,height=480)

tE <- tapply(NEI.2$Emissions,NEI.2$year,sum)
br <- barplot2(tE, col = 'gray',las=1, 
               main = expression("Motor Vehicle" ~ PM[2.5] ~ "emission in the Baltimore City"),
               plot.grid = TRUE, ylab="Emission (t)", xlab="Years")
box()
lines(br, tE, type = 'h', col = 'red', lwd = 2)

dev.off()


############################################################

library(plyr)
library(gplots)
SCC$SCC <- as.character(SCC$SCC)

SCC.2 <- SCC[grep("Mobile Sources", SCC$SCC.Level.One),]
NEI.2 <- subset(NEI, fips %in% c("24510","06037"))
NEI.3 <- join(NEI, SCC.2, by = "SCC", type = "inner")


png(file="plot5.png", width=480,height=480)

tE <- tapply(NEI.24510$Emissions,NEI.24510$year,sum)
br <- barplot2(tE, col = 'gray',las=1, 
               main = expression("Motor Vehicle" ~ PM[2.5] ~ "emission in the Baltimore City"),
               plot.grid = TRUE, ylab="Emission (t)", xlab="Years")
box()
lines(br, tE, type = 'h', col = 'red', lwd = 2)

dev.off()
