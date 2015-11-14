---
output: html_document
---
```{r}
#Reading the data and organizing the data
data <- read.csv("activity.csv", stringsAsFactors = FALSE)
data$date <- as.Date(data$date, "%Y-%m-%d")
str(data)
```

```{r}

```