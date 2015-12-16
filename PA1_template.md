# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
activity <- read.csv(file = 'activity.csv', stringsAsFactors = FALSE)

require(magrittr)
```

```
## Loading required package: magrittr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## What is mean total number of steps taken per day?

The prompt says that we can `ignore missing values`.  I am taking that to mean that we can `na.rm` when summing by day.

### steps per day

```r
per_day <- activity %>%
  dplyr::group_by(date) %>%
  dplyr::summarize(
    total_steps = sum(steps, na.rm = TRUE)
  ) 

per_day
```

```
## Source: local data frame [61 x 2]
## 
##          date total_steps
##         (chr)       (int)
## 1  2012-10-01           0
## 2  2012-10-02         126
## 3  2012-10-03       11352
## 4  2012-10-04       12116
## 5  2012-10-05       13294
## 6  2012-10-06       15420
## 7  2012-10-07       11015
## 8  2012-10-08           0
## 9  2012-10-09       12811
## 10 2012-10-10        9900
## ..        ...         ...
```

### histogram

```r
per_day$total_steps %>% 
  hist(
    breaks = seq(0, 25000, 2500), 
    main = 'Steps per Day'
  )
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

### mean

```r
per_day$total_steps %>% mean()
```

```
## [1] 9354.23
```

### median

```r
per_day$total_steps %>% median()
```

```
## [1] 10395
```

## What is the average daily activity pattern?


```r
per_interval <- activity %>%
  dplyr::group_by(interval) %>%
  dplyr::summarize(
    total_steps = mean(steps, na.rm = TRUE)
  )

per_interval
```

```
## Source: local data frame [288 x 2]
## 
##    interval total_steps
##       (int)       (dbl)
## 1         0   1.7169811
## 2         5   0.3396226
## 3        10   0.1320755
## 4        15   0.1509434
## 5        20   0.0754717
## 6        25   2.0943396
## 7        30   0.5283019
## 8        35   0.8679245
## 9        40   0.0000000
## 10       45   1.4716981
## ..      ...         ...
```


```r
plot(
  per_interval$interval, per_interval$total_steps, type='l',
  main = 'Average Steps per 5 min Daily Interval', 
  xlab = 'Interval', ylab = 'Average Steps', 
  col = 'hotpink', lwd = 2,
)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

### largest average interval
The interval with the largest (maximum) number of steps is interval 835, with 206.2 steps.

```r
per_interval %>% arrange(-total_steps)
```

```
## Source: local data frame [288 x 2]
## 
##    interval total_steps
##       (int)       (dbl)
## 1       835    206.1698
## 2       840    195.9245
## 3       850    183.3962
## 4       845    179.5660
## 5       830    177.3019
## 6       820    171.1509
## 7       855    167.0189
## 8       815    157.5283
## 9       825    155.3962
## 10      900    143.4528
## ..      ...         ...
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
