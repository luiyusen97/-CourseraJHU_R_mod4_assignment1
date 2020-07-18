library(tidyverse)

if (!file.exists("rawdata//emissionsdata.zip")){
    download.file(url = "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                  destfile = "rawdata//emissionsdata.zip")
}
unzip(zipfile = "rawdata//emissionsdata.zip", exdir = "rawdata//emissionsdata")

# get the rawdata
NEI <- readRDS("rawdata//emissionsdata//summarySCC_PM25.rds")
SCC <- readRDS("rawdata//emissionsdata//Source_Classification_Code.rds")

# convert the 2 columns containing types of particles into character factors for grep function
SCC[ , 10] <- apply(SCC[ , 10, drop=F], 2, as.character)
SCC[ , 9] <- apply(SCC[ , 9, drop=F], 2, as.character)
# grep on a column returns row indices, so which function does not work on non-boolean expressions
# thus, get the row indices by finding , for the 2 columns, which rows contain coal. Concatenate
# the index numeric vectors and remove duplicate values
isitcoalrowindices <- unique(c(grep("Coal", SCC[ , 9]), grep("Coal", SCC[ , 10])))
# since it is a numeric vector, just subset the dataframe using it directly
SCC <- SCC[isitcoalrowindices, ]
# get the SCC index no.s, since these are used in the NEI dataframe
SCCcoal <- as.character(SCC[ , 1])

# get only rows which correspond to these index no.s
NEIcoal <- NEI[NEI$SCC %in% SCCcoal, ]
# create a template dataframe to contain sum values
NEIcoalyrsums <- distinct(NEIcoal, year, .keep_all = TRUE)
# create a vector to temporarily store sum values
sumpm25 <- vector(mode = "numeric")
# same as before, split up dataframe by year, and calculate the sums of Emissions in each dataframe
# append them to sumpm25
NEIcoalsplitbyyr <- split(NEIcoal, NEIcoal$year)
for (frame in NEIcoalsplitbyyr){
    pm25total <- colSums(frame[ , 4, drop = FALSE])
    sumpm25 <- c(sumpm25, pm25total)
}
# replace the redundant Emissions column with the sum values
NEIcoalyrsums[ , 4] <- sumpm25
colnames(NEIcoalyrsums)[4] <- "sum.pm25"

# plot the values and add a linear regression line without its confidence band
NEIcoalplot <- ggplot(NEIcoalyrsums, mapping = aes(year, sum.pm25)) + 
    geom_point() + 
    stat_smooth(method = "lm", se = FALSE) + 
    ggtitle("Total emissions from coal sources per year")
print(NEIcoalplot)
ggsave(filename = "plot4.png",
       plot = NEIcoalplot,
       device = png()
)
dev.off()