## plot2.R

## Make a plot showing the total PM2.5 emission from all sources for
## Baltimore City in each of the years 1999, 2002, 2005, and 2008

## fetch zip file from URL (if required)

fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
localFilename <- "FNEI_data.zip"

if (!file.exists(localFilename)) {
    print("Fetching data set from URL...")
    download.file(fileUrl, destfile = localFilename, mode = "wb")
} else {
    print("Using local copy of data set.")
}

dataFileList <- unzip(localFilename,list = TRUE)

## unzip the file to the .data directory (if required)

if (!file.exists("data")) {  
    unzip(localFilename, overwrite=FALSE, exdir="data")
}

## Read the two data files if they are not already in the R
## environment (saves time to check first)

if (!exists("NEI")) {
    NEI <- readRDS("./data/summarySCC_PM25.rds")
}
if (!exists("SCC")) {
    SCC <- readRDS("./data/Source_Classification_Code.rds")
}

## generate summary of total Emissions by year for ZIP code 24510

NEI_24510 <- NEI[NEI$fips == 24510,]

## Generate summary by year, Uses plyr package

library(plyr)
totalEmissionsByYear_24510 <- ddply(NEI_24510, c("year"), summarise, total.Emissions=sum(Emissions),
                              number.Readings=length(Emissions))
years <- unique(NEI$year)

## stuff for box plot

byYear <- split(NEI_24510,NEI_24510$year)
nonZero1999 <- byYear[[1]]$Emissions > 0.0
nonZero2002 <- byYear[[2]]$Emissions > 0.0
nonZero2005 <- byYear[[3]]$Emissions > 0.0
nonZero2008 <- byYear[[4]]$Emissions > 0.0

## Multiple plots
par(mfrow = c(1,3))
par(ann=FALSE)

## Plot2a with linear model

with(totalEmissionsByYear_24510, plot(year,total.Emissions, xaxt="n"))
axis(1,at=years,labels=years)
model2 <- lm(total.Emissions ~ year, totalEmissionsByYear_24510)
abline(model2, lwd=2)
title("Total Emission over Time\nfor Baltimore City, MD (24510)",
      ylab = "Total Emissions (tons)",
      xlab = "Year")

## Plot2b bar plot number of readings
with(totalEmissionsByYear_24510, barplot(number.Readings, xaxt="n"))
axis(1,at=1:4,labels=years)
title("Amount of Data over Time",
      ylab = "Number of Readings",
      xlab = "Year")

## Plot3c boxplot of Log transformed ddata
## (without taking the log of data, the
## quantiles of the dataset are illegible)

par(pch = 0)
boxplot(log(byYear[[1]]$Emissions[nonZero1999]),
        log(byYear[[2]]$Emissions[nonZero2002]),
        log(byYear[[3]]$Emissions[nonZero2005]),
        log(byYear[[4]]$Emissions[nonZero2008]), 
        range = 0, xaxt="n")
axis(1,at=1:4,labels=years)
title("Boxplot of Log Transformed Nonzero Emissions\nfor Baltimore City, MD (24510)",
      ylab = "log(Emissions > 0.0)",
      xlab = "Year")

## create plot directly on png device

png("plot2.png", width=480*2)
    par(mfrow = c(1,3))
    par(ann=FALSE)

    ## Plot2a

    with(totalEmissionsByYear_24510, plot(year,total.Emissions, xaxt="n"))
    axis(1,at=years,labels=years)
    model2 <- lm(total.Emissions ~ year, totalEmissionsByYear_24510)
    abline(model2, lwd=2)
    title("Total Emission over Time\nfor Baltimore City, MD (24510)",
          ylab = "Total Emissions (tons)",
          xlab = "Year")

    ## Plot2b
    with(totalEmissionsByYear_24510, barplot(number.Readings, xaxt="n"))
    axis(1,at=1:4,labels=years)
    title("Amount of Data over Time",
          ylab = "Number of Readings",
          xlab = "Year")

    ## Plot3c
    par(pch = 0)
    boxplot(log(byYear[[1]]$Emissions[nonZero1999]),
            log(byYear[[2]]$Emissions[nonZero2002]),
            log(byYear[[3]]$Emissions[nonZero2005]),
            log(byYear[[4]]$Emissions[nonZero2008]),
            range = 0, xaxt="n")
    axis(1,at=1:4,labels=years)
    title("Boxplot of Log Transformed Nonzero Emissions\nfor Baltimore City, MD (24510)",
          ylab = "log(Emissions > 0.0)",
          xlab = "Year")
dev.off()
