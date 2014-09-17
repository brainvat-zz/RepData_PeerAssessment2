# Peer Assessment 2
Allen Hammock  
September 16, 2014  

# Synopsis

This study addresses two questions:

1. Across the United States, which types of events (as indicated in the `EVTYPE` variable) are most harmful with respect to population health?

2. Across the United States, which types of events have the greatest economic consequences?



# Data Processing


```r
# Pre-load all required libraries
library(dplyr)
library(ggplot2)

# Download and process data
setwd("~/Google Drive/projects/r/coursera/RepData_PeerAssessment2")

myfile.zip <- "StormData.csv.bz2"
myfile.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if (!file.exists(myfile.zip)) {
    download.file(url = myfile.url, destfile = myfile.zip, method = "curl")    
}

if (file.exists(myfile.zip)) {
    myfile.df <<- read.csv(bzfile(myfile.zip))
} else {
    stop("ABORT. Input file missing, unable to download or decompress original.")
}

# Create summary table
myfile.df.summary <- myfile.df %>% 
    select(STATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, CROPDMG) %>% 
    mutate(TOTALDMG = PROPDMG + CROPDMG, TOTALHARMED = FATALITIES + INJURIES) %>% 
    group_by(STATE, EVTYPE) %>% 
    summarize(People.Harmed = sum(TOTALHARMED), Economic.Damage = sum(TOTALDMG))

str(myfile.df.summary)
```

```
## Classes 'grouped_df', 'tbl_df', 'tbl' and 'data.frame':	4258 obs. of  4 variables:
##  $ STATE          : Factor w/ 72 levels "AK","AL","AM",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ EVTYPE         : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 17 19 30 34 37 48 54 90 95 115 ...
##  $ People.Harmed  : num  0 50 12 0 0 0 3 0 0 0 ...
##  $ Economic.Damage: num  0 869 201 0 0 ...
##  - attr(*, "vars")=List of 1
##   ..$ : symbol STATE
##  - attr(*, "drop")= logi TRUE
```


# Results


