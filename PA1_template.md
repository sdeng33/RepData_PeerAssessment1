Preliminary Process
-------------------

This section includes all the code that is needed before the Loading and
Preprocessing section of the project, to include checking/creating a
folder, setting a working directory, checking for the existence of the
zip file in question, and the downloading and unziping of the file as
necessary.

    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

    zipfile.name <-"Dataset.zip"

    # Checks to see if the required file has been downloaded.  If not,  
    # it downloads the file. #
    if(!file.exists(zipfile.name)){
            download.file(fileUrl, destfile = zipfile.name , method = "curl")
    }

    filename <- "activity.csv"

    # Checks to see if the file name exists.  If not, it unzips the required 
    # file. #
    if(!file.exists(filename)){
            unzip(zipfile.name)
    }

Loading and Preprocessing
-------------------------

### Task: 1. Load the data (i.e. read.csv())

    amd <- read.csv(filename, header = TRUE)

    head(amd)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

### Task: 2. Process/transform the data (if necessary) into a format suitable for your analysis.

Processing and transforming the data was not necessary to conduct the
analysis that I did.

What is mean total number of steps taken per day?
-------------------------------------------------

For this part of the assignment, you can ignore the missing values in the dataset.
----------------------------------------------------------------------------------

### Task: 1. Calculate the total number of steps taken per day.

    steps.per.day <- tapply(amd$steps, INDEX = amd$date, FUN = sum, na.rm = TRUE)

    steps.per.day

    ## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
    ##          0        126      11352      12116      13294      15420      11015 
    ## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
    ##          0      12811       9900      10304      17382      12426      15098 
    ## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
    ##      10139      15084      13452      10056      11829      10395       8821 
    ## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
    ##      13460       8918       8355       2492       6778      10119      11458 
    ## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
    ##       5018       9819      15414          0      10600      10571          0 
    ## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
    ##      10439       8334      12883       3219          0          0      12608 
    ## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
    ##      10765       7336          0         41       5441      14339      15110 
    ## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
    ##       8841       4472      12787      20427      21194      14478      11834 
    ## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
    ##      11162      13646      10183       7047          0

### Task: 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

    # Library needed to use qplot. #
    library(ggplot2)

    # Using the Freedman-Diaconis rule to select the widths of the bins to be used
    # in the histogram we plot: #

    steps.per.day.range <- range(steps.per.day)

    steps.per.day.IQR <- IQR(steps.per.day)

    binwidth <- 2 * steps.per.day.IQR * (61)^(-1/3)

    # Histogram of the total number of steps taken each day #
    qplot(steps.per.day, geom="blank", main = "Total Number of Steps Taken Each Day"
          , xlab = "Steps", ylab = "Counts") + geom_histogram(color="black", 
          fill = "white", breaks = seq(steps.per.day.range[1], 
          steps.per.day.range[2], binwidth), closed = "right")

![](PA1_template_files/figure-markdown_strict/Histogram%201-1.png)

### Task: 3. Calculate and report the mean and median of the total number of steps taken per day.

    mean.steps.per.day <- mean(steps.per.day)

    median.steps.per.day <- median(steps.per.day)

The mean of the total number of steps taken per day is 9354.2295082,
while the median is 10395.

What is the average daily activity pattern?
-------------------------------------------

### Task: 1. Make a time series plot(i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

    mean.steps.per.interval <- tapply(amd$steps, INDEX = amd$interval, 
                                       FUN = mean, na.rm = TRUE)

    intervals <- as.numeric(names(mean.steps.per.interval))

    adap <- data.frame(intervals = intervals, mspi = mean.steps.per.interval)

    plot(adap$intervals, adap$mspi, type = "l", 
         main = "Average Number of Steps per Time Interval", 
         xlab = "5-minute intervals", 
         ylab = "Average Number of Steps Taken Across All Days")

![](PA1_template_files/figure-markdown_strict/Time%20Series%20Plot-1.png)

### Task: 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

    maxnum.steps.interval <- adap$intervals[adap$mspi == max(adap$mspi)]

The 5-minute interval which contains the maximum number of steps is 835.

Imputing missing values
-----------------------

### Task: 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows of NAs)

    total.number.NAs <- sum(is.na(amd$steps))

The total number of missing values in the dataset is 2304.

### Task: 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for the day, or the mean for that 5-minute interval, etc.

    # Finds the indices of the steps column that have a value of NA. #
    ind <- which(is.na(amd$steps))

    # Fills the missing values in the dataset with the mean for that 5-minute #
    # interval. #
    amd$steps[ind] <- mean.steps.per.interval

    steps <- amd$steps

### Task: 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

    new.amd <- data.frame(steps = steps, date = amd$date, interval = amd$interval)

    head(new.amd)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

### Task: 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

    steps.per.day.no.na <- tapply(new.amd$steps, INDEX = new.amd$date, FUN = sum)

    # Once more, using the Freedman-Diaconis rule to select the widths of the bins 
    # to be used in the histogram we plot: #

    spdn.range <- range(steps.per.day.no.na)

    spdn.IQR <- IQR(steps.per.day.no.na)

    spdn.binwidth <- 2 * spdn.IQR * (61)^(-1/3)

    qplot(steps.per.day.no.na, geom="blank", main = 
          "Total Number of Steps Taken Each Day", xlab = "Steps", ylab = "Counts") + 
          geom_histogram(color="black", fill = "white", 
          breaks = seq(spdn.range[1], spdn.range[2], spdn.binwidth), 
          closed = "right")

![](PA1_template_files/figure-markdown_strict/Histogram%202-1.png)

    mean.spdna <- mean(steps.per.day.no.na)

    median.spdna <- median(steps.per.day.no.na)

    first.frag <- "The mean of the total number of steps taken per day is "
    mid.frag <- ", while the median is also "
    period <- "."

    cat(first.frag, mean.spdna, mid.frag, median.spdna, period, sep = "")

    ## The mean of the total number of steps taken per day is 10766.19, while the median is also 10766.19.

### Question: Do these values differ from the estimates from the first part of the assignment?

Answer: The mean and median total number of steps taken per day which
includes imputing missing values is greater than than the estimates from
the first part of the assignment as is expected.

### Question: What is the impact of imputing missing data on the estimates of the total daily number of steps?

    steps.per.day.no.na

    ## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
    ##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00   11015.00 
    ## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
    ##   10766.19   12811.00    9900.00   10304.00   17382.00   12426.00   15098.00 
    ## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
    ##   10139.00   15084.00   13452.00   10056.00   11829.00   10395.00    8821.00 
    ## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
    ##   13460.00    8918.00    8355.00    2492.00    6778.00   10119.00   11458.00 
    ## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
    ##    5018.00    9819.00   15414.00   10766.19   10600.00   10571.00   10766.19 
    ## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
    ##   10439.00    8334.00   12883.00    3219.00   10766.19   10766.19   12608.00 
    ## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
    ##   10765.00    7336.00   10766.19      41.00    5441.00   14339.00   15110.00 
    ## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
    ##    8841.00    4472.00   12787.00   20427.00   21194.00   14478.00   11834.00 
    ## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
    ##   11162.00   13646.00   10183.00    7047.00   10766.19

Answer: As is shown, all the previous intervals which had values of
zero, now all have non-zero estimates.

Question: Are there differences in activity pattern between weekdays and weekends?
----------------------------------------------------------------------------------

### Task: 1. Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

    days <- weekdays(as.Date(amd$date))

    weekends <- c("Saturday", "Sunday")

    days.of.week <- factor(is.element(days, weekends), levels = c(TRUE, FALSE), 
                           labels = c("weekend", "weekday"))

    head(days.of.week)

    ## [1] weekday weekday weekday weekday weekday weekday
    ## Levels: weekend weekday

### Task: 2. Make a panel plot containing a time series plot (i.e. type = "l) of the 5-minute inteval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

    newest.amd <- cbind(new.amd, days.of.week)

    amd.wk <- aggregate(steps ~ interval + days.of.week , data = newest.amd, 
                        FUN = mean)

    # Library needed for lattice plotting system of R. #
    library(lattice)

    # Panel plot. #
    xyplot(steps ~ interval|days.of.week, layout = c(1,2), xlab = 
           "5-minute intervals", ylab = "Average Number of Steps Taken", type = "l",
           lty = 1, data = amd.wk)

![](PA1_template_files/figure-markdown_strict/Panel%20Plot-1.png)

Answer: There is generally a higher activity pattern on the weekends
than on weekdays.
