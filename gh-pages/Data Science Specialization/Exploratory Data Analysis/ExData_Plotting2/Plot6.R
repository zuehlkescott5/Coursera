#install.packages("ggplot2")
library(ggplot2)
setwd("H:/Coursera/Data Science Specialization/4- Exploratory Data Analysis")
# Check to see if DataForPeerAssessment.zip already exists in working directory.  If it doesn't exist already, 
# pull data from Coursera webpage and load into working directory under zip file DataForPeerAssessment.zip
# If it does exist, do nothing.

if(all(!file.exists("DataForPeerAssessment.zip"), !dir.exists("DataForPeerAssessment"),!file.exists("Source_Classification_Code.rds"))) {
  tryCatch({
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip", "DataForPeerAssessment.zip")
    print("File did not exist, but was successfully downloaded from the URL.")
  },
  error = function(cond) {
    print(
      "File did not exist, but there was an error in downloading the file from the provided URL."
    )
  })
}

# Check to see if "DataForPeerAssessment" already exists in working directory.  If it doesn't exist already, 
# zip data from DataForPeerAssessment.zip and load into working directory as "DataForPeerAssessment".
# If it does exist, remove DataForPeerAssessment.zip from working directory.

if (!dir.exists("DataForPeerAssessment")) {
  tryCatch({
    unzip("DataForPeerAssessment.zip")
    print("File household_power_consumption.txt Dataset did not exist, but was successfully unzipped from DataForPeerAssessment.zip")
  },
  error = function(cond) {
    print("File household_power_consumption.txt Dataset did not exist, and there was an error in unzipping the file.")
  })
} else {
  file.remove("DataForPeerAssessment.zip")
  print("FromCoursera.zip deleted since rds files were successfully created.")
}

if (file.exists("Source_Classification_Code.rds")) {
  file.remove("DataForPeerAssessment.zip")
  print("DataForPeerAssessment.zip deleted since rds files were successfully created.") 
}

## This first line will likely take a few seconds. Be patient!


if(exists("NEI")) {
  print("NEI exists")
} else {
  NEI <- readRDS("summarySCC_PM25.rds")
}


if(exists("SCC")) {
  print("SCC exists")
} else {
  SCC <- readRDS("Source_Classification_Code.rds")
}


# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, 
# California (???????????????? == ????????????????????). Which city has seen greater changes over time in motor vehicle emissions?

subset.NEI <- NEI[(NEI$fips=="24510"|NEI$fips=="06037") & NEI$type=="ON-ROAD",  ]

totalByYear <- aggregate(Emissions ~ year + fips, subset.NEI, sum)
totalByYear$fips[totalByYear$fips=="24510"] <- "Baltimore City"
totalByYear$fips[totalByYear$fips=="06037"] <- "Los Angeles"


png("plot6.pnExData_Plotting2/g", width=1040, height=480)
plt <- ggplot(totalByYear, aes(factor(year), Emissions, fill='red'))
plt <- plt + facet_grid(. ~ fips)
plt <- plt + geom_bar(stat="identity", color = 'red')  +
  xlab("year") +
  ylab(expression('PM 2.5 emission')) +
  theme(legend.position='none') +
  ggtitle('Total Emissions from motor vehicle in Baltimore City vs Los Angeles   (1999 - 2008 by 3 year intervals)')
print(plt)
dev.off()