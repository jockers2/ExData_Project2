## plot3.R

## Make a plot showing the total PM2.5 emission from each sources for
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

## peel out data related to ZIP code 24510

NEI_24510 <- NEI[NEI$fips == 24510,]

## Generate summary by year and type. Uses plyr package

library(plyr)
df_plot3 <- ddply(NEI_24510, c("year","type"), summarise, total.Emissions=sum(Emissions),
                              number.Readings=length(Emissions))

df_plot3$type <- factor(df_plot3$type, levels=unique(df_plot3$type))
years <- unique(NEI_24510$year)

## Create plot (in ggplot2 system)

require(ggplot2)
p <- qplot(year,total.Emissions,data=df_plot3,facets = .~type,
           xlab="Year", ylab="Total Emissions (tons)")
p <- p + geom_smooth(method="lm",formula=y~x)
print(p)

## Create plot directly on png device
png("plot3.png", width = 2*480)
    p <- qplot(year,total.Emissions,data=df_plot3,facets = .~type,
               xlab="Year", ylab="Total Emissions (tons)")
    p <- p + geom_smooth(method="lm",formula=y~x)
    print(p)
dev.off()