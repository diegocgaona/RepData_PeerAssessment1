require("ggplot2")
require("scales")
## Get the URL
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipdatafile <- "./datafile.zip" ## create zipdata file
download.file(fileURL, destfile = zipdatafile, mode = "wb") ## download the file
unzip(zipdatafile) ## unzip the file
## Read the data
rawactivity <- read.csv("activity.csv")
## Transform the all the interval variables in 4 digits
activity <- rawactivity
activity$interval <- sprintf("%04d", activity$interval)
## Summarize the data, sum steps by day
sumstepsbyday <- aggregate(steps ~ date, activity, sum, na.action = na.pass)
## Replace NA in steps, by 0 to be showed in the histogram
sumstepsbyday$steps[is.na(sumstepsbyday$steps)] <- 0

## Make a histogram of the steps by day
png("plot_hist.png", width = 600, height = 500) ## initiate png graphic device
## Plot with lines separated by type color
p <- ggplot(sumstepsbyday, aes(steps)) ## Initializes ggplot object
p <- p + geom_histogram(col="red", fill="black", alpha = .7, binwidth = 1000)
p <- p + ylab("Frequency") +  xlab("steps") ##  Show labels
## p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p <- p + ggtitle("Histogram of steps taken by day") ## Show the title
p
dev.off()  ## Close the device (png)

## Return the mean steps by day
meanstepsbyday <- mean(sumstepsbyday$steps, na.rm = TRUE)
## Return the median steps by day
medianstepsbyday <- median(sumstepsbyday$steps, na.rm = TRUE)
## Summarize the average steps by interval
meanstepsbyinterval <- aggregate(steps ~ interval, activity, mean)

## Transform interval in Posix
time <- meanstepsbyinterval$interval
time <- strptime(time, "%H%M")

time <- as.numeric(format(time, "%H")) +
      as.numeric(format(time, "%M"))
meanstepsbyinterval$interval <- time

p2 <- ggplot(meanstepsbyinterval, aes(interval, steps)) + geom_line() +
      scale_x_datetime(labels = date_format("%H:%M"),
                       breaks = date_breaks("1 hour"), minor_breaks=date_breaks("30 min")) + 
      xlab("interval") + ylab("steps") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
p2

p2 <- p + scale_x_datetime(breaks = date_breaks("1 hour"), minor_breaks=date_breaks("30 min"),
                           labels=date_format("%H:%M:%S"), limits=xlim_p2)
