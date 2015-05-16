## Load packages
require("ggplot2")
require("scales")
## Set to english language and time
Sys.setlocale("LC_TIME", "English")
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
p <- ggplot(sumstepsbyday, aes(steps)) ## Initializes ggplot object
p <- p + geom_histogram(col="red", fill="black", alpha = .7, binwidth = 1000)
p <- p + ylab("Frequency") +  xlab("steps") ##  Show labels
p <- p + ggtitle("Histogram of steps taken by day") ## Show the title
p
dev.off()  ## Close the device (png)

## Return the mean steps by day
meanstepsbyday <- mean(sumstepsbyday$steps, na.rm = TRUE)
## Return the median steps by day
medianstepsbyday <- median(sumstepsbyday$steps, na.rm = TRUE)
## Summarize the average steps by interval
meanstepsbyinterval <- aggregate(steps ~ interval, activity, mean)

## Make a a time series plot of the 5-minute interval and the average number of steps taken,
## averaged across all days

## Transform interval in Posix
time1 <- meanstepsbyinterval$interval
time1 <- as.POSIXct(strptime(time1, "%H%M"))
meanstepsbyinterval$interval <- time1

## Find the max steps interval
max_stepsinterval <- meanstepsbyinterval[which(meanstepsbyinterval$steps==max(meanstepsbyinterval$steps)),]
print(max_stepsinterval)


png("plot_line.png", width = 600, height = 500) ## initiate png graphic device
p2 <- ggplot(meanstepsbyinterval, aes(interval, steps)) + geom_line() +
      scale_x_datetime(labels = date_format("%H:%M"),
                       breaks = date_breaks("2 hour"), minor_breaks=date_breaks("30 min")) + 
      xlab("interval") + ylab("steps") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      annotate("text", x = max_stepsinterval$interval, 
               y = max_stepsinterval$steps + 6, label = "interval 08:35 - 206.1698 steps", colour = "red") +
      annotate("pointrange", x = max_stepsinterval$interval, y = max_stepsinterval$steps, 
               ymin = 206, ymax = 206, colour = "red", size = 1)
p2
dev.off()  ## Close the device (png)

## Find the sum of NA in the steps intervals
sum_na <- length(which(is.na(rawactivity$steps)))
print(sum_na)
## Replace NA with mean values of intervals
## Make the means by interval without Posix
rawmeansbyinterval <- aggregate(steps ~ interval, rawactivity, mean)
## Merge the raw DF with the meaned DF by interval
newactivity <- merge(rawactivity, rawmeansbyinterval, by = "interval", suffixes = c("", ".n"))
## Get the NA rows
NAs <- is.na(newactivity$steps)
## Replace the NA with the means in rawmeansbyinterval
newactivity$steps[NAs] <- newactivity$steps.n[NAs]
## Remove the column steps.n
newactivity <- newactivity[,c(1:3)]
## ## Summarize the new filled data, sum steps by day
newsumstepsbyday <- aggregate(steps ~ date, newactivity, sum)

png("plot_NEWhist.png", width = 600, height = 500) ## initiate png graphic device
## Plot with lines separated by type color
p3 <- ggplot(newsumstepsbyday, aes(steps)) ## Initializes ggplot object
p3 <- p3 + geom_histogram(col="red", fill="black", alpha = .7, binwidth = 1000)
p3 <- p3 + ylab("Frequency") +  xlab("steps") ##  Show labels
p3 <- p3 + ggtitle("Histogram of steps taken by day with NAs replaced by interval means") ## Show the title
p3
dev.off()  ## Close the device (png)

## Return the mean steps by day with the filled data
newmeanstepsbyday <- mean(newsumstepsbyday$steps, na.rm = TRUE)
print(newmeanstepsbyday)
## Return the median steps by day with the filled data
newmedianstepsbyday <- median(newsumstepsbyday$steps, na.rm = TRUE)
print(newmedianstepsbyday)
## Transform the date column in the filled data in Posix
newactivity$date <- strptime(newactivity$date, "%Y-%m-%d")
## Create a column with the weekdays
newactivity$weekdays <- weekdays(newactivity$date)
## Replace the days names by weekend or weekday
newactivity$weekdays[newactivity$weekdays %in% c("Sunday", "Saturday")] <-"weekend"
newactivity$weekdays[newactivity$weekdays %in% 
                           c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")] <-"weekday"
## Transform the weekdays column in factor
newactivity$weekdays <- factor(newactivity$weekdays, levels = c("weekday", "weekend"),
                               labels = c("weekday", "weekend"))
## Average by interval and weekday
newmeanstepbyint_week <- aggregate(steps ~ interval + weekdays, newactivity, mean)
## Create a plot with activity patterns between weekdays and weekends
png("plot_line_weekday.png", width = 600, height = 500) ## initiate png graphic device
p4 <- ggplot(newmeanstepbyint_week, aes(interval, steps)) + geom_line(colour = "blue") +     
      xlab("interval") + ylab("steps") +
      ggtitle(expression(bold("Activity patterns between weekdays and weekends"))) +
      facet_wrap(~ weekdays, ncol = 1)      
p4
dev.off()  ## Close the device (png)
## Mean of steps by weekday
meanstepsbyweekday <- aggregate(steps ~ weekdays, newactivity, mean)
