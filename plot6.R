library(tidyverse)

if (!file.exists("rawdata//emissionsdata.zip")){
    download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                  destfile = "rawdata//emissionsdata.zip")
}
unzip(zipfile = "rawdata//emissionsdata.zip", exdir = "rawdata//emissionsdata")

# get the rawdata
NEI <- readRDS("rawdata//emissionsdata//summarySCC_PM25.rds")
SCC <- readRDS("rawdata//emissionsdata//Source_Classification_Code.rds")

# get SCC codes for emissions from motor vehicles
rowindices <- grep("Motor", SCC[, 3])
SCCmotorcodes <- SCC[rowindices, 1, drop = TRUE]

# subset out motor vehicle emissions in Baltimore data
NEIbaltimore <- NEI[which(NEI$fips=="24510"), ]
NEIbaltimore <- NEIbaltimore[NEIbaltimore$SCC %in% SCCmotorcodes, ]
NEIsplit <- split(NEIbaltimore, NEIbaltimore$year)
# calculate the sums of emissions for each year
sumpm25 <- vector(mode = "numeric")
for (frame in NEIsplit){
    pm25 <- colSums(frame[ , 4, drop = FALSE])
    sumpm25 <- c(sumpm25, pm25)
}
NEIsumpm25_balt <- distinct(NEIbaltimore, year, .keep_all = TRUE)
NEIsumpm25_balt[ , 4] <- sumpm25

# same for LA
NEILA <- NEI[which(NEI$fips=="06037"), ]
NEILA <- NEILA[NEILA$SCC %in% SCCmotorcodes, ]
NEIsplit <- split(NEILA, NEILA$year)
sumpm25 <- vector(mode = "numeric")
for (frame in NEIsplit){
    pm25 <- colSums(frame[ , 4, drop = FALSE])
    sumpm25 <- c(sumpm25, pm25)
}
NEIsumpm25_LA <- distinct(NEILA, year, .keep_all = TRUE)
NEIsumpm25_LA[ , 4] <- sumpm25

# combine the 2 sum dataframes
NEIsumpm25 <- rbind(NEIsumpm25_balt, NEIsumpm25_LA)
colnames(NEIsumpm25)[4] <- "sum.pm25"
# replace the fips(county code) column with the county names so ggplot
# automatically names the 2 facets
# as.numeric somehow produced NAs even though there's nothing but numbers
# in the strings, so I can't make the column a factor variable
for (i in 1:8){
    if (NEIsumpm25[i, 1] == "24510"){
        NEIsumpm25[i, 1] <- "Baltimore"
    } else if (NEIsumpm25[i, 1] == "06037"){
        NEIsumpm25[i, 1] <- "LA"
    }
}
levels(NEIsumpm25[ , 1]) <- c("LA", "Baltimore")

# plot the data, split by county, and with linear regression, but without its
# confidence bands
NEIplot <- ggplot(NEIsumpm25, mapping = aes(year, sum.pm25)) +
    facet_grid(. ~ fips) +
    geom_point() +
    stat_smooth(method = "lm", se = FALSE) +
    ggtitle("LA and Baltimore emissions from motor vehicles by year")
# create plot file
ggsave(filename = "plot6.png",
       plot = NEIplot,
       device = png()
       )
# close graphing device/file connection
dev.off()