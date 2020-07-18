if (!file.exists("rawdata//emissionsdata.zip")){
    download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                  destfile = "rawdata//emissionsdata.zip")
}
unzip(zipfile = "rawdata//emissionsdata.zip", exdir = "rawdata//emissionsdata")

# get the rawdata
NEI <- readRDS("rawdata//emissionsdata//summarySCC_PM25.rds")
SCC <- readRDS("rawdata//emissionsdata//Source_Classification_Code.rds")

NEIbaltimore <- NEI[which(NEI$fips=="24510"), ]
NEIbaltbyyear <- split(NEIbaltimore, NEIbaltimore$year)
years <- unique(NEI[ , 6])
totalpm25 <- vector(mode = "numeric")
for (frame in NEIbaltbyyear){
    totalpm25yr <- sum(frame[ , 4])
    totalpm25 <- c(totalpm25, totalpm25yr)
}
totalpm25frame <- cbind(years, totalpm25)
png(filename = "plot2.png")
plot(totalpm25frame)
abline(lm(totalpm25 ~ years))
dev.off()