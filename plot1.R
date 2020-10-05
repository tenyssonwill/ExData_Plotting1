plot1 <- function(){
  data <- read.table("household_power_consumption.txt", header = TRUE, sep =";",
        colClasses = c("character",rep("NULL",8)))
  date <- as.Date(data$Date,format="%d/%m/%Y")
  firstInd = min(which(date==as.Date("2007-02-02") | date==as.Date("2007-02-01")))
  LastInd = max(which(date==as.Date("2007-02-02") | date==as.Date("2007-02-01")))
  data <- read.table("household_power_consumption.txt", header = TRUE, sep =";",
                     nrows = 1)
  data <- read.table("household_power_consumption.txt", header = TRUE, sep =";",
        skip = firstInd -1, nrows = LastInd - firstInd + 1,col.names = names(data))
  
  png("plot1.png")
  hist(data$Global_active_power, col= "red", main = "Global Active Power",
       xlab = "Global Active Power (kilowatts)")
  dev.off()
}

