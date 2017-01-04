
# Check to see if FromCoursera.zip already exists in working directory.  If it doesn't exist already, 
# pull data from Coursera webpage and load into working directory under zip file FromCoursera.zip.
# If it does exist, do nothing.

if (!file.exists("FromCoursera.zip")) {
  tryCatch({
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", "FromCoursera.zip")
    print("File did not exist, but was successfully downloaded from the URL.")
  },
  error = function(cond) {
    print(
      "File did not exist, but there was an error in downloading the file from the provided URL."
    )
  })
} else {
  print("File FromCoursera.zip already exists")
}

# Check to see if "household_power_consumption.txt" already exists in working directory.  If it doesn't exist already, 
# zip data from FromCoursera.zip and load into working directory as "household_power_consumption.txt".
# If it does exist, do nothing.

if (!file.exists("household_power_consumption.txt")) {
  tryCatch({
    unzip("FromCoursera.zip")
    print("File household_power_consumption.txt Dataset did not exist, but was successfully unzipped from FromCoursera.zip.")
  },
  error = function(cond) {
    print("File household_power_consumption.txt Dataset did not exist, and there was an error in unzipping the file.")
  })
} else {
  file.remove("FromCoursera.zip")
  print("FromCoursera.zip deleted since text was successfully created.")
}

if (file.exists("household_power_consumption.txt")) {
  file.remove("FromCoursera.zip")
  print("FromCoursera.zip deleted since text was successfully created.") 
}


# After downloading .zip file, create data.full data frame with text file data, then remove file from working directory.
data.full <- read.table("household_power_consumption.txt", header=TRUE, sep=";", stringsAsFactors=FALSE, dec=".")
file.remove("household_power_consumption.txt")

# From HW details, The dataset has 2,075,259 rows and 9 columns. 
str(data.full)
# From R console, data.frame:	2075259 obs. of  9 variables:

data.subsetted <- data.full[data.full$Date %in% c("1/2/2007","2/2/2007") ,]

str(data.subsetted)

#str(subSetData)
date.time <- strptime(paste(data.subsetted$Date, data.subsetted$Time, sep=" "), "%d/%m/%Y %H:%M:%S") 
globalActivePower <- as.numeric(data.subsetted$Global_active_power)
globalReactivePower <- as.numeric(data.subsetted$Global_reactive_power)
voltage <- as.numeric(data.subsetted$Voltage)
sub.metering.1 <- as.numeric(data.subsetted$Sub_metering_1)
sub.metering.2 <- as.numeric(data.subsetted$Sub_metering_2)
sub.metering.3 <- as.numeric(data.subsetted$Sub_metering_3)


png("plot4.png", width=480, height=480)
par(mfrow = c(2, 2)) 

plot(date.time, globalActivePower, type="l", xlab="", ylab="Global Active Power", cex=0.2)

plot(date.time, voltage, type="l", xlab="datetime", ylab="Voltage")

plot(date.time, sub.metering.1, type="l", ylab="Energy Submetering", xlab="")
lines(date.time, sub.metering.2, type="l", col="red")
lines(date.time, sub.metering.3, type="l", col="blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=, lwd=2.5, col=c("black", "red", "blue"), bty="o")

plot(date.time, globalReactivePower, type="l", xlab="datetime", ylab="Global_reactive_power")

dev.off()