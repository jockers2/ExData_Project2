## plot4.R

## Make a plot showing the total PM2.5 emission from coal burning
## sources changes from 1999-2008

## fetch zip file from URL (if required)

fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
localFilename <- "FNEI_data.zip"

if (!file.exists(localFilename)) {
    print("Fetching data set from URL...")
    download.file(fileUrl, destfile = localFilename, mode = "wb")
} else {
    print("Using local copy of data set.")
}

## unzip the file to the ./data directory (if required)

dataFileList <- unzip(localFilename,list = TRUE)

if (!file.exists("data")) {  
    unzip(localFilename, overwrite=FALSE, exdir="data")
}

## Read the two files if they are not already in the R
## environment (saves time to check first)

if (!exists("NEI")) {
    NEI <- readRDS("./data/summarySCC_PM25.rds")
}
if (!exists("SCC")) {
    SCC <- readRDS("./data/Source_Classification_Code.rds")
}

## Which SCC ids are related to coal combustion?

coal_sccs <- SCC$SCC[grep("coal|Coal",SCC$Short.Name,value=FALSE)]
NEI_coal <- NEI[NEI$SCC %in% coal_sccs, ]

# generate summary by year

library(plyr)
coalEmissionsByYear <- ddply(NEI_coal, c("year"), summarise,
                             total.Emissions=sum(Emissions),
                             median.Emissions=median(Emissions), 
                             number.Readings=length(Emissions) )
years < unique(coalEmissionsByYear$year)

## Multiple plots
par(mfrow = c(1,2))
par(ann=FALSE)

## Plot4a Total coal based emissions over Time

with(coalEmissionsByYear, plot(year,total.Emissions, xaxt="n"))
axis(1,at=years,labels=years)
model4a <- lm(total.Emissions ~ year, coalEmissionsByYear)
abline(model4a, lwd=2)
title("Total Coal Based Emissions over Time\nacross United States",
      ylab = "Total Emissions (tons)",
      xlab = "Year")

#Plot4b Median coal based emissions over Time

with(coalEmissionsByYear, plot(year,median.Emissions,xaxt="n"))
axis(1,at=years,labels=years)
model4b <- lm(median.Emissions ~ year, coalEmissionsByYear)
abline(model4b, lwd=2)
title("Coal Based Emissions over Time\nacross United States",
      ylab = "Median Emissions (tons)",
      xlab = "Year")

## create plot directly on png device

png("plot4.png",width=2*480)
    par(mfrow = c(1,2))
    par(ann=FALSE)

    ## Plot4a

    with(coalEmissionsByYear, plot(year,total.Emissions, xaxt="n"))
    axis(1,at=years,labels=years)
    model4a <- lm(total.Emissions ~ year, coalEmissionsByYear)
    abline(model4a, lwd=2)
    title("Total Coal Based Emissions over Time\nacross United States",
          ylab = "Total Emissions (tons)",
          xlab = "Year")

    #Plot4b
    with(coalEmissionsByYear, plot(year,median.Emissions,xaxt="n"))
    axis(1,at=years,labels=years)
    model4b <- lm(median.Emissions ~ year, coalEmissionsByYear)
    abline(model4b, lwd=2)
    title("Coal Based Emissions over Time\nacross United States",
          ylab = "Median Emissions (tons)",
          xlab = "Year")

dev.off()