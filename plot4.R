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

dataFileList <- unzip(localFilename,list = TRUE)

if (!file.exists("data")) {  
    unzip(localFilename, overwrite=FALSE, exdir="data")
}

## Read two files

if (!exists("NEI")) {
    NEI <- readRDS("./data/summarySCC_PM25.rds")
}
if (!exists("SCC")) {
    SCC <- readRDS("./data/Source_Classification_Code.rds")
}

## which SCC ids are related to coal combustion?

coal_sccs <- SCC$SCC[grep("coal|Coal",SCC$Short.Name,value=FALSE)]

NEI_coal <- NEI[NEI$SCC %in% coal_sccs, ]

# generate summary by year

library(plyr)
coalEmissionsByYear <- ddply(NEI_coal, c("year"), summarise,
                             total.Emissions=sum(Emissions),
                             median.Emissions=median(Emissions), 
                             number.Readings=length(Emissions) )

with(coalEmissionsByYear, plot(year,median.Emissions))
model <- lm(median.Emissions ~ year, coalEmissionsByYear)
abline(model, lwd=2)