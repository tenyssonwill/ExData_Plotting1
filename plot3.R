plot2 <- function(){
  data <- read.table("household_power_consumption.txt", header = TRUE, sep =";",
        colClasses = c("character",rep("NULL",8)))
  date <- as.Date(data$Date,format="%d/%m/%Y")
  firstInd = min(which(date==as.Date("2007-02-02") | date==as.Date("2007-02-01")))
  LastInd = max(which(date==as.Date("2007-02-02") | date==as.Date("2007-02-01")))
  data <- read.table("household_power_consumption.txt", header = TRUE, sep =";",
                     nrows = 1)
  data <- read.table("household_power_consumption.txt", header = TRUE, sep =";",
        skip = firstInd -1, nrows = LastInd - firstInd + 1,col.names = names(data))
  
  datetime <- paste(data$Date, data$Time)
  data$Datetime <- as.POSIXct(datetime,format="%d/%m/%Y %H:%M:%S")
  
  png("plot3.png")
  with(data, {plot(Sub_metering_1~Datetime, type = "l", col= "black",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~Datetime,col='Red')
  lines(Sub_metering_3~Datetime,col='Blue')})
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, 
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  dev.off()
}

