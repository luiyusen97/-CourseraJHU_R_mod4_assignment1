library(tidyverse)

if (!file.exists("rawdata//emissionsdata.zip")){
    download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                  destfile = "rawdata//emissionsdata.zip")
}
unzip(zipfile = "rawdata//emissionsdata.zip", exdir = "rawdata//emissionsdata")

# get the rawdata
NEI <- readRDS("rawdata//emissionsdata//summarySCC_PM25.rds")
SCC <- readRDS("rawdata//emissionsdata//Source_Classification_Code.rds")

# we only need Baltimore data
NEIbaltimore <- NEI[which(NEI$fips=="24510"), ]
# get the SCC codes of the pollutant type which comes from motor vehicles
rowindices <- grep("Motor", SCC[, 3])
SCCmotorcodes <- SCC[rowindices, 1, drop = TRUE]
# get the pollutants coming from motor vehicles in Baltimore
NEImotor <- NEIbaltimore[NEIbaltimore$SCC %in% SCCmotorcodes, ]
sumpm25 <- vector(mode = "numeric")
NEImotorsplityr <- split(NEImotor, NEImotor$year)
# calculate sum of pollutants and organise them in a similar dataframe to
# NEI rawdata, just with sum of pollutants in place of Emissions column
for (frame in NEImotorsplityr){
    sumpm25frame <- colSums(frame[ , "Emissions", drop = FALSE])
    sumpm25 <- c(sumpm25, sumpm25frame)
}
NEIsums <- distinct(NEImotor, year, .keep_all = TRUE)
NEIsums[ , 4] <- sumpm25
colnames(NEIsums)[4] <- "sum.pm25"

# plot out the sums data with linear regression, but without its
# confidence band
NEIplot <- ggplot(NEIsums, mapping = aes(year, sum.pm25)) +
    geom_point() +
    stat_smooth(method = "lm", se = FALSE) +
    ggtitle("Total motor vehicle emissions by year")
print(NEIplot)
ggsave(filename = "plot5.png",
       plot = NEIplot,
       device = png()
       )
# switch off png graphing device
dev.off()