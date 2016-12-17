#install.packages("ggplot2")
library(ggplot2)
setwd("H:/Coursera/Data Science Specialization/4- Exploratory Data Analysis")
# Check to see if DataForPeerAssessment.zip already exists in working directory.  If it doesn't exist already,
# pull data from Coursera webpage and load into working directory under zip file DataForPeerAssessment.zip
# If it does exist, do nothing.

if (all(
  !file.exists("DataForPeerAssessment.zip"),
  !dir.exists("DataForPeerAssessment"),
  !file.exists("Source_Classification_Code.rds")
)) {
  tryCatch({
    download.file(
      "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
      "DataForPeerAssessment.zip"
    )
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
    print(
      "File household_power_consumption.txt Dataset did not exist, but was successfully unzipped from DataForPeerAssessment.zip"
    )
  },
  error = function(cond) {
    print(
      "File household_power_consumption.txt Dataset did not exist, and there was an error in unzipping the file."
    )
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


if (exists("NEI")) {
  print("NEI exists")
} else {
  NEI <- readRDS("summarySCC_PM25.rds")
}


if (exists("SCC")) {
  print("SCC exists")
} else {
  
  SCC <- readRDS("Source_Classification_Code.rds")
}

# Of the four types of sources indicated by the ???????????????? (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions
# from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.


NEI.subset  <- NEI[NEI$fips == "24510",]

totalByYear <- aggregate(Emissions ~ year + type, NEI.subset, sum)

png('ExData_Plotting2/plot3.png', width=1080, height=640)
plt <- ggplot(totalByYear, aes(year, Emissions, color = type))
plt <- plt + 
  geom_line() +
  xlab("Year") +
  ylab(expression('PM 2.5 emission')) +
  ggtitle('PM 2.5 emission in Baltimore City, MD by source type (1999-200 -  by 3 year interval).')
print(plt)
dev.off()