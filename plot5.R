## plot5.R

## Make a plot showing the total PM2.5 emission from motor vehicle
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

dataFileList <- unzip(localFilename,list = TRUE)

## unzip the file to the ./data directory (if required)

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

## which SCC ids are related to motor vehicle sources?

motor_vehicle_sccs <- SCC$SCC[grep("motor|Motor vehicle|Vehicle",SCC$Short.Name,value=FALSE)]

## peel out data for motor vehicles in Baltimore City

NEI_mvb <- NEI[NEI$SCC %in% motor_vehicle_sccs & NEI$fips == "24510", ]

# generate summary by year. Uses plyr package

library(plyr)
mvbEmissionsByYear <- ddply(NEI_mvb, c("year"), summarise,
                             total.Emissions=sum(Emissions),
                             median.Emissions=median(Emissions), 
                             number.Readings=length(Emissions) )
years <- unique(mvbEmissionsByYear$year)


## Multiple plots
par(mfrow = c(1,3))
par(ann=FALSE)

## Plot 5a with linear model
with(mvbEmissionsByYear, plot(year,total.Emissions, xaxt="n"))
axis(1,at=years,labels=years)
model5a <- lm(total.Emissions ~ year, mvbEmissionsByYear)
abline(model5a, lwd=2)
title("Total Motor Vehicle Emissions over Time\nfor Baltimore City, MD (24510)",
      ylab = "Total Emissions (tons)",
      xlab = "Year")

## Plot 5b bar plot number of readings
with(mvbEmissionsByYear, barplot(number.Readings, xaxt="n"))
axis(1,at=1:4,labels=years)
title("Amount of Data over Time",
      ylab = "Number of Readings",
      xlab = "Year")

## Plot 5c median Emissions
with(mvbEmissionsByYear, plot(year,median.Emissions, xaxt="n"))
axis(1,at=years,labels=years)
model5b <- lm(median.Emissions ~ year, mvbEmissionsByYear)
abline(model5b, lwd=2)
title("Median Motor Vehicle Emissions over Time\nfor Baltimore City, MD (24510)",
      ylab = "Median Emissions (tons)",
      xlab = "Year")

## create plot directly on png device

png("plot5.png", width=480*2)
    par(mfrow = c(1,3))
    par(ann=FALSE)

    ## Plot 5a
    with(mvbEmissionsByYear, plot(year,total.Emissions, xaxt="n"))
    axis(1,at=years,labels=years)
    model5a <- lm(total.Emissions ~ year, mvbEmissionsByYear)
    abline(model5a, lwd=2)
    title("Total Motor Vehicle Emissions over Time\nfor Baltimore City, MD (24510)",
          ylab = "Total Emissions (tons)",
          xlab = "Year")

    ## Plot 5b
    with(mvbEmissionsByYear, barplot(number.Readings, xaxt="n"))
    axis(1,at=1:4,labels=years)
    title("Amount of Data over Time",
          ylab = "Number of Readings",
          xlab = "Year")

    ## Plot 5c
    with(mvbEmissionsByYear, plot(year,median.Emissions, xaxt="n"))
    axis(1,at=years,labels=years)
    model5b <- lm(median.Emissions ~ year, mvbEmissionsByYear)
    abline(model5b, lwd=2)
    title("Median Motor Vehicle Emissions over Time\nfor Baltimore City, MD (24510)",
          ylab = "Median Emissions (tons)",
          xlab = "Year")
dev.off()