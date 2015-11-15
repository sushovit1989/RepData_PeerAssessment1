---
output: html_document
---
```{r}
library(lattice)


#Reading the data and organizing the data
data <- read.csv("activity.csv",colClasses =c("numeric", "character", "numeric"))
data$date <- as.Date(data$date, "%Y-%m-%d")
str(data)
```

```{r}
#calculating the total and mean
dailysteps <- aggregate(steps ~ date, data = data, sum, na.rm = T)
```

```{r}
#plotting the total steps and mean
hist(dailysteps$steps, main = "total steps taken by day", xlab = "steps per day", ylab = "frequency of days")
abline(v=mean(dailysteps$steps),col="red",lwd=10)
```

```{r}
#the mean of the steps:
mean(dailysteps$steps)
#the median of the steps:
median(dailysteps$steps)
```

```{r}
#time series and average number...
tseries <- tapply(data$steps, data$interval, mean, na.rm =T)
#plotting the time series data
plot(row.names(tseries), tseries, type = "l", xlab = "five minutes int", ylab = "average steps all days", main = "average steps taken", col="red")
```

```{r}
#looking at the maximum steps
maximumsteps <- which.max(tseries)
names(maximumsteps)
```

```{r}
#looking at the NA values
navalues <-sum(is.na(data))
navalues
```

```{r}
#looking and replacing the NA values in the data set

meansteps <- aggregate(steps ~ interval, data = data, FUN = mean )
fill <- numeric()
for (k in 1:nrow(data)) {
  data_use <- data[k, ]
  if (is.na(data_use$steps)) {
    steps <- subset(meansteps, interval == data_use$interval)$steps
  } else {
    steps <-data_use$steps
  }
  fill <-c (fill, steps)
  }
```

```{r}
#refilling the NAs with the data calculated above
new_data <- data
new_data$steps <- fill
```

```{r}
totalsteps <- aggregate(steps ~ date, data = new_data, sum, na.rm = T)
hist(totalsteps$steps, main = "total no of steps taken each day", xlab = "day")
#with the mean and median as:
abline(v=mean(totalsteps$steps), col="blue", lwd=8)
```

```{r}
#mean total steps is:
mean(totalsteps$steps)
```

```{r}
# median total steps is:
median(totalsteps$steps)
```

```{r}
#creating a factor variable 
data$day <- ifelse(as.POSIXlt(data$date)$wday %in% c(0,6), 'weekend', 'weekday')
stepsday <-aggregate(steps ~ interval + day, data=data, mean)
```

```{r}
library(ggplot2)
ggplot(stepsday, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(day ~ .) + 
  xlab("5 min interval") +
  ylab("averaged across weekends and weekdays")


