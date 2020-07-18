library(tidyverse)

if (!file.exists("rawdata//emissionsdata.zip")){
    download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                  destfile = "rawdata//emissionsdata.zip")
}
unzip(zipfile = "rawdata//emissionsdata.zip", exdir = "rawdata//emissionsdata")

# get the rawdata
NEI <- readRDS("rawdata//emissionsdata//summarySCC_PM25.rds")
SCC <- readRDS("rawdata//emissionsdata//Source_Classification_Code.rds")

# test code
NEIbaltimore <- NEI[which(NEI$fips=="24510"), ]
NEIbaltbytype <- split(NEIbaltimore, NEIbaltimore$type)
years <- unique(NEI[ , 6])
frame <- NEIbaltbytype[[1]]
framesplit <- split(frame, frame$year)
colSums(framesplit[[1]][ , 4, drop = F], na.rm = T)

# split by year
NEIbaltbyyear <- split(NEIbaltimore, NEIbaltimore$year)
# create empty dataframe with 1 row and 5 columns
# this is dataframe that will be used to store the pm25 sums
sumpm25byyrandtype <- data.frame(
    fips = character(),
    Pollution = character(),
    sum.pm25 <- numeric(),
    type = character(),
    year = numeric()
)
for (frame in NEIbaltbyyear){
    # split again by type
    framebytype <- split(frame, frame$type)
    # create a dataframe to store individual type's sumdata in
    framebytypesum <- sumpm25byyrandtype
    # iterate over each type's dataframe
    for (i in 1:4){
        # calc sum
        sumpm25 <- sum(framebytype[[i]][, 4])
        # replace raw pm25 data with sums, now the whole column is just one value
        framebytype[[i]][, 4] <- rep(sumpm25, nrow(framebytype[[i]]))
        # remove SCC column to prevent any errors in distinct function later
        framebytype[[i]] <- framebytype[[i]][ , -2]
        # change name of Emissions column
        colnames(framebytype[[i]])[3] <- "sum.pm25"
        # since all values are now the same, just get first row for unique values
        framebytype[[i]] <- framebytype[[i]][1, ]
        # bind all the types' sum dataframes to a sum dataframe for one year
        framebytypesum <- rbind(framebytypesum, framebytype[[i]])
    }
    # bind all years' sum dataframes together
    sumpm25byyrandtype <- rbind(sumpm25byyrandtype, framebytypesum)
    # remove duplicate values based on sum values and year
    sumpm25byyrandtype <- dplyr::distinct(sumpm25byyrandtype, year, sum.pm25, .keep_all = TRUE)
    print(sumpm25byyrandtype)
}
# create plot
NEIplot <- ggplot(data = sumpm25byyrandtype, mapping = aes(year, sum.pm25)) + 
    # split into 4 scatterplots by type of emissions
    facet_grid(. ~ as.factor(as.character(type))) + 
    # add the datapoints from dataframe
    geom_point() + 
    # add regression lines for each plot, without confidence bands
    geom_smooth(method = "lm", se = FALSE)
print(NEIplot)
# save plot to png file
ggsave(filename = "plot3.png",
       plot = NEIplot,
       device = png()
       )
dev.off()