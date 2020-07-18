library(tidyverse)

if (!file.exists("rawdata//emissionsdata.zip")){
    download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                  destfile = "rawdata//emissionsdata.zip")
}
unzip(zipfile = "rawdata//emissionsdata.zip", exdir = "rawdata//emissionsdata")

# get the rawdata
NEI <- readRDS("rawdata//emissionsdata//summarySCC_PM25.rds")
SCC <- readRDS("rawdata//emissionsdata//Source_Classification_Code.rds")

NEIbaltimore <- NEI[which(NEI$fips=="24510"), ]
NEIbaltbytype <- split(NEIbaltimore, NEIbaltimore$type)
years <- years <- unique(NEI[ , 6])
frame <- NEIbaltbytype[[1]]
framesplit <- split(frame, frame$year)
colSums(framesplit[[1]][ , 4, drop = F], na.rm = T)
