require("ggplot2")
## Get the URL
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipdatafile <- "./datafile.zip" ## create zipdata file
download.file(fileURL, destfile = zipdatafile, mode = "wb") ## download the file
unzip(zipdatafile) ## unzip the file
## Read the data
rawactivity <- read.csv("activity.csv")
## Summarize the data, sum steps by day
sumstepsbyday <- aggregate(steps ~ date, rawactivity, sum, na.action = na.pass)
## Replace NA in steps, by 0 to be showed in the histogram
sumstepsbyday$steps[is.na(sumstepsbyday$steps)] <- 0

## Make a histogram of the steps by day
png("plot_hist.png", width = 800, height = 600) ## initiate png graphic device
## Plot with lines separated by type color
p <- ggplot(sumstepsbyday, aes(x = factor(date), y = steps)) ## Initializes ggplot object
p <- p + geom_histogram(stat = "identity") ## Use histogram
p <- p + ylab("Steps") +  xlab("Day") ##  Show labels
p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p + ggtitle("Steps by day") ## Show the title
dev.off()  ## Close the device (png)

## Return the mean steps by day
meanstepsbyday <- mean(sumstepsbyday$steps, na.rm = TRUE)
## Return the median steps by day
medianstepsbyday <- median(sumstepsbyday$steps, na.rm = TRUE)


