# Getting the data
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
dir.create("/Users/alex/Documents/R directory/Exploratory-Data-Analysis/PeerAssessment")
download.file(url,destfile="/Users/alex/Documents/R directory/Exploratory-Data-Analysis/PeerAssessment/test.zip",method="curl")
unzip("/Users/alex/Documents/R directory/Exploratory-Data-Analysis/PeerAssessment/test.zip",exdir="/Users/alex/Documents/R directory/Exploratory-Data-Analysis/PeerAssessment/")

# Reading the files
NEI <- readRDS("/Users/alex/Documents/R directory/Exploratory-Data-Analysis/PeerAssessment/summarySCC_PM25.rds")
SCC <- readRDS("/Users/alex/Documents/R directory/Exploratory-Data-Analysis/PeerAssessment/Source_Classification_Code.rds")

# Assignment

#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all 
# sources for each of the years 1999, 2002, 2005, and 2008.

EmisSum <- aggregate(NEI$Emissions,by=list(NEI$year),sum)

png(filename="/Users/alex/Documents/R directory/Exploratory-Data-Analysis/PeerAssessment/plot1.png", width=480, height=480, units="px",bg="linen")
plot(EmisSum,type="l",xlab="Year",ylab="Total Emission",main="Total Emissions from PM2.5 in the Uninted States",col="blue")
dev.off()

# Have total emissions from PM2.5 decreased in 
# the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.
BaltDat <- NEI[which(NEI$fips=="24510"),]
BaltEmis <- aggregate(BaltDat$Emissions,by=list(BaltDat$year),sum)

png(filename="/Users/alex/Documents/R directory/Exploratory-Data-Analysis/PeerAssessment/plot2.png", width=480, height=480, units="px",bg="linen")
plot(BaltEmis,type="l",xlab="Year",ylab="Total Emission",main="Baltimore, Maryland Total Emission", col="red")
dev.off()

# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
# variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.
library(ggplot2)
png(filename="/Users/alex/Documents/R directory/Exploratory-Data-Analysis/PeerAssessment/plot3.png", width=480, height=480, units="px",bg="linen")

ggplot(BaltDat, aes(x=year,y=Emissions,colour=type,)) + 
    geom_line(stat="summary",fun.y="sum") +
        ggtitle("Total Emission by Type") + xlab("Year") + ylab("Total Emission")
dev.off()

# Across the United States, how have emissions from coal combustion-related sources 
# changed from 1999–2008?

CCList <- grep("Coal", SCC$Short.Name, ignore.case = T)
CCdata <- SCC[CCList,1]
Edata <- NEI[which(NEI$SCC %in% CCdata),]
EdataSum <- aggregate(Edata$Emissions,by=list(Edata$year),FUN=sum)

png(filename="/Users/alex/Documents/R directory/Exploratory-Data-Analysis/PeerAssessment/plot4.png", width=480, height=480, units="px",bg="linen")

qplot(data=EdataSum,x=EdataSum[,1],y=EdataSum[,2],geom="line",main="U.S. Coal Combustion-related Emissions", xlab="Year",ylab="Total Emission")

dev.off()
# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

library(plyr)

MotBalt <- grep("motor",SCC$Short.Name,ignore.case=T,)
 MotList <- SCC[MotBalt,1]

BaltDat2 <- BaltDat[BaltDat$SCC %in% MotList,]
MotBaltFin <- aggregate(BaltDat2$Emissions,by=list(BaltDat2$year),FUN=sum)

png(filename="/Users/alex/Documents/R directory/Exploratory-Data-Analysis/PeerAssessment/plot5.png", width=480, height=480, units="px",bg="linen")
ggplot(MotBaltFin,aes(MotBaltFin[,1],MotBaltFin[,2])) +
    geom_line(stat="identity", colour="orange",linetype=1, size=2) +
        ggtitle("Baltimore City Motor Vehicle Emissions") + xlab("Year") + ylab("Total Emission")
dev.off()
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
# in Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?
