## Introduction

This study analizes the daily steps taken by an individual via a mobile
device. The analisis consist of assessments of the mean total number of
steps, the daily activity pattern, both without missing data and filling
the “NA” entries with statistical data, and finally identifying the
existance of different patterns during weekdays and weekend.

## Libraries

I’ve used the readr, dplyr, ggplot2 and lubridate libraries for this
study, if you don’t have them installed please do so before running the
code.

    library(readr)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(ggplot2)
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

## Loading data and processing

The dataset to be ised can be find in the same direction as a .csv file
named “activisy.csv” inside a .zip file named “activity.zip”. The first
step is to upload this data and read it.

    # Specify the path to the zip file
    zip_file <- "activity.zip"

    # Specify the file within the zip archive
    csv_file_inside_zip <- "activity.csv"

    # Unzip and read the CSV file
    # This assumes the CSV file is directly inside the zip without any subfolders
    unzipped_csv_path <- unzip(zip_file, files = csv_file_inside_zip, exdir = tempdir())

    # Now read the CSV file into R
    activity_data <- read_csv(file.path(tempdir(), csv_file_inside_zip))

    ## Rows: 17568 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (2): steps, interval
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Mean Total Number of Steps Taken Per Day

### /This section was performed ignoring the missing values./

In this section we are calculating the total steps per day, building and
printing an histogram of this data and printing the mean and median
steps per day.

    library(dplyr)
    library(ggplot2)

    #Calculate the Total Number of Steps Taken Per Day
    daily_steps <- activity_data %>%
      group_by(date) %>%
      summarise(total_steps = sum(steps))

    #Setting a dataset that ignores NAs
    daily_steps_no_na <- daily_steps[!is.na(daily_steps$total_steps), ]

    print(daily_steps_no_na)

    ## # A tibble: 53 × 2
    ##    date       total_steps
    ##    <date>           <dbl>
    ##  1 2012-10-02         126
    ##  2 2012-10-03       11352
    ##  3 2012-10-04       12116
    ##  4 2012-10-05       13294
    ##  5 2012-10-06       15420
    ##  6 2012-10-07       11015
    ##  7 2012-10-09       12811
    ##  8 2012-10-10        9900
    ##  9 2012-10-11       10304
    ## 10 2012-10-12       17382
    ## # ℹ 43 more rows

    #Build the histogram
    ggplot(daily_steps_no_na, aes(x = total_steps)) +
      geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
      theme_classic() +
      labs(title = "Histogram of Daily Steps", x = "Total Steps per Day", y = "Frequency") +
      ylim(0, 10)  # This will limit the y-axis but also might cut off bars taller than 10

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    # Calculate and report mean and median
    mean_daily_steps <- mean(daily_steps_no_na$total_steps)
    median_daily_steps <- median(daily_steps_no_na$total_steps)

    # Print the mean and median

    print(paste("Mean of daily steps:", round(mean_daily_steps, 3)))

    ## [1] "Mean of daily steps: 10766.189"

    print(paste("Median of daily steps:", median_daily_steps))

    ## [1] "Median of daily steps: 10765"

We can see that the mean of daily steps is 10,766.189 and the median is
10,765.

## Average daily activity pattern

### /For this section we will continue ignoring the NA values of the dataset/

Now we proceed by Calculating the average number of steps per interval
and build the time series. We also dettermine and print the maximum
interval.

    # Calculate the average number of steps per interval
    average_interval_steps <- activity_data %>%
      group_by(interval) %>%
      summarise(average_steps = mean(steps, na.rm = TRUE))

    # Create the plot
    average_daily_activity_plot <- ggplot(average_interval_steps, aes(x = interval, y = average_steps)) +
      geom_line(color = "blue") +
      theme_classic() +
      labs(title = "Average Daily Activity Pattern",
           x = "5-minute Interval",
           y = "Average Number of Steps")

    # Display the plot
    print(average_daily_activity_plot)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    # Identify the interval with the maximum average steps
    max_interval <- average_interval_steps[which.max(average_interval_steps$average_steps), ]
    print(max_interval)

    ## # A tibble: 1 × 2
    ##   interval average_steps
    ##      <dbl>         <dbl>
    ## 1      835          206.

Now we know that the maximum interval is the 835 minutes with an average
of 206.1698 steps.

## Adjusting the NA values

For this section, first we calculate the total ammount of missing values

    # Calculate total number of missing values
    missing_values <- sum(is.na(activity_data$steps))
    print(missing_values)

    ## [1] 2304

With this we know that there are 2304 missing values.

To fill them, we will use the simple method of replacing with the mean
steps per 5-minute interval and repeat the histogram and mean/median
calculation of the previous steps:

    activity_data$steps[is.na(activity_data$steps)] <- average_interval_steps$average_steps[match(activity_data$interval[is.na(activity_data$steps)], average_interval_steps$interval)]

    ggplot(daily_steps, aes(x = total_steps)) +
      geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
      theme_classic() +
      labs(title = "Histogram of Daily Steps", x = "Total Steps per Day", y = "Frequency") +
      ylim(0, 10)  # This will limit the y-axis but also might cut off bars taller than 10

    ## Warning: Removed 8 rows containing non-finite values (`stat_bin()`).

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    # Calculate and report mean and median
    new_mean_daily_steps <- mean(daily_steps$total_steps)
    new_median_daily_steps <- median(daily_steps$total_steps)

    # Print the mean and median

    print(paste("Mean of daily steps:", round(mean_daily_steps, 3)))

    ## [1] "Mean of daily steps: 10766.189"

    print(paste("Median of daily steps:", median_daily_steps))

    ## [1] "Median of daily steps: 10765"

In this new iteration we have the same Mean and Median, 10,766.189 and
10,765 respectively. This are the benefits of doing so:

Enable Complete Data Analysis: We are able to utilize the whole dataset,
instead of compromising information.

Reduces Bias: I case the data is missing not at random, the exclution of
it can provoke bias in the dataset.

Increases Statistical Power: By maximizing the sample size, which can
increase the confidence in analysis.

## Differences in Activity Patterns Between Weekdays and Weekends

### /For this part we will use again the filled dataset from the previous section/

Now we want to identify significant differences between the weekdays and
weekends data.

For this we first add a variable “day\_type” to differentiate both
categories

    # Create a new column 'day_type'
    activity_data <- activity_data %>%
      mutate(day_of_week = weekdays(date),
             day_type = ifelse(day_of_week %in% c("Saturday", "Sunday"), "weekend", "weekday")) %>%
      mutate(day_type = as.factor(day_type))

    #Calculate the Average Number of Steps for Each Interval by Day Type
    average_steps_by_day_type <- activity_data %>%
      group_by(interval, day_type) %>%
      summarise(average_steps = mean(steps, na.rm = TRUE))

    ## `summarise()` has grouped output by 'interval'. You can override using the
    ## `.groups` argument.

    #Create a Panel Plot
    p <- ggplot(average_steps_by_day_type, aes(x = interval, y = average_steps)) +
      geom_line() +
      facet_wrap(~day_type, ncol = 1, scales = "free_y") +
      theme_classic() +
      labs(title = "Activity Patterns: Weekday vs. Weekend",
           x = "Interval",
           y = "Average Number of Steps")

    print(p)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-7-1.png) As
we can see, the week days present a higher peak at the beggining of the
day, but lower movement later on. This suggests that the subject walks
more at a certain time of the day during weekdays, but during weekends
they have more movement along the day.
