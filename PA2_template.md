# Effects of Severe Weather Events on Human Health and Property from January 1996 - December 2010

# Synopsis

As a matter of public policy, we are interested in studying severe weather events in an effort
to make sound investments that might minimize injuries, fatalities, and economic damage to 
property and crops as much as possible. This study addresses two questions:

1. Which types of events are most harmful with respect to population health?

2. Which types of events have the greatest economic consequences?

[Observations of severe weather storms](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2)
collected from the 
[National Weather Service Storm Data Base Program](http://www.nws.noaa.gov/wsom/manual/archives/NF429405.HTML)
across the United States between January 1996 and December 2010 were considered for the study. 
Since not all severe weather events occur every year or in every state, we chose to look at the 
impacts of the most damaging storms collectively by region 
([defined by National Geographic magazine](http://education.nationalgeographic.com/education/maps/united-states-regions/?ar_a=1)) 
excluding U.S. territories.

# Data Processing

First, we extract the storm data observations from the origianal National Weather Service source
and append the regional information we need to conduct the study.


```r
# Pre-load all required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(xtable)
library(maps)
```


```r
# Download and process data
setwd("~/Google Drive/projects/r/coursera/RepData_PeerAssessment2")

myfile.zip <- "StormData.csv.bz2"
myfile.url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

if (!file.exists(myfile.zip)) {
    download.file(url = myfile.url, destfile = myfile.zip, method = "curl")    
}

if (file.exists(myfile.zip)) {
    # useful during development to force the data frame to
    # be global scope and only load once, like a Singleton
    if (!exists("myfile.df")) {
        myfile.df <- read.csv(bzfile(myfile.zip))    
    }
} else {
    stop("ABORT. Input file missing, unable to download or decompress original.")
}
```

The source data contains a total of **902,297 observations** 
across **37 variables**.  We are interested only in the following derived variables:

* total sum of all economic damage caused to property and crops
* total number of people harmed through injury or fatality
* the variances of these measurements by region and severe weather event


```r
# Prepare regions
# http://education.nationalgeographic.com/education/maps/united-states-regions/?ar_a=1
regions <- rbind(data.frame(REGION = character(), STATE = character(), stringsAsFactors = FALSE), 
                 cbind(REGION = "West", 
                       STATE = c("WA", "OR", "ID", "MT", "WY", "CA", "NV", 
                                 "UT", "CO", "HI", "AL")),
                 cbind(REGION = "Southwest", 
                       STATE = c("AZ", "NM", "TX", "OK")),
                 cbind(REGION = "Midwest", 
                       STATE = c("ND", "SD", "NE", "KS", "MO", "IA", "MN", 
                                 "WI", "IL", "IN", "OH", "MI")),
                 cbind(REGION = "Southeast", 
                       STATE = c("AR", "LA", "MS", "AL", "GA", "FL", "SC", 
                                 "NC", "TN", "KY", "WV", "VA", "DE")),
                 cbind(REGION = "Northeast", 
                       STATE = c("MD", "PA", "NJ", "CT", "RI", "NY", "MA", 
                                 "NH", "VT", "ME"))
                 )

data("state")
us.states <- data.frame(STATE = state.abb, STATE.NAME = state.name)

# convenience tables to convert units
PROP.conversions <- rbind(
        data.frame(PROPDMGEXP = "", PROPDMGMULT = 1),
        data.frame(PROPDMGEXP = "K", PROPDMGMULT = 1000),
        data.frame(PROPDMGEXP = "M", PROPDMGMULT = 1000000),
        data.frame(PROPDMGEXP = "B", PROPDMGMULT = 1000000000)
    )

CROP.conversions <- rbind(
        data.frame(CROPDMGEXP = "", CROPDMGMULT = 1),
        data.frame(CROPDMGEXP = "K", CROPDMGMULT = 1000),
        data.frame(CROPDMGEXP = "M", CROPDMGMULT = 1000000),
        data.frame(CROPDMGEXP = "B", CROPDMGMULT = 1000000000)
    )

# Create summary table
# Break down by state, weather event type, region, and date components
# Calculate the sum of all property and crop damages as well as
# all individuals injured or killed by weather
myfile.df.summary <- myfile.df %>% 
    select(STATE,  # U.S. state
           EVTYPE, # weather event type
           FATALITIES, # number of fatalities
           INJURIES, # number of injuries
           PROPDMG, # Property Damage in abbreviated dollar amounts according to WSOM Chapter F-42
           PROPDMGEXP, # exponent for property damages
           CROPDMG, # Crop Damage in abbreviated dollar amounts according to WSOM Chapter F-42
           CROPDMGEXP, # exponent for crop damages
           BGN_DATE # Y/M/D HH:MM:SS in local time
           ) %>%
    filter(STATE %in% regions$STATE) %>%
    left_join(regions) %>%
    left_join(us.states) %>%
    left_join(PROP.conversions) %>%
    left_join(CROP.conversions) %>%
    mutate(TOTALDMG =  (PROPDMG * PROPDMGMULT) + (CROPDMG * CROPDMGMULT), 
           TOTALHARMED = FATALITIES + INJURIES,
           BEGIN_YEAR = year(mdy_hms(BGN_DATE))
           ) %>%
    # look at only recent data where there were harms or damages
    filter(BEGIN_YEAR >= 1996 & BEGIN_YEAR <= 2010 & TOTALDMG > 0 & TOTALHARMED > 0) %>%
    # get rid of the original metrics
    select(-PROPDMG, -CROPDMG, -FATALITIES, -INJURIES, -BGN_DATE) %>%
    group_by(BEGIN_YEAR, 
             REGION,
             STATE,
             STATE.NAME,
             EVTYPE) %>% 
    summarize(People.Harmed = sum(TOTALHARMED), Economic.Damage = sum(TOTALDMG))
```

We now have a smaller data set that is easier to work with.

To address the research questions, we calculate two more summary tables.  First, we
look at the effects of severe weather on public health and the economy by region.  We use
the following definitions:

* **people harmed** is defined as the total number of people who suffered injury or were killed
* **economic damage** is measured in terms of the total dollar value of damages to crops and property

To sort out which types of severe weather have the most impact, we'll look at the average amount of 
harm that each weather type did in the years where the weather was observed.  For example, if we 
look at 15 years of data, but a hurricane appears in the data in three of those years, we'll take 
the average hurricane damage as the total number of people harmed divided by 3 (not 15).

That way, weather types that do not appear every year are not misrepresented in our survey
of damaging storm types.

We'll look only at the 20 most harmful storm types out of the 
985 different classifications available.


```r
# Find the top most harmful storm types
top.harms <- myfile.df.summary %>% 
    select(EVTYPE, BEGIN_YEAR, STATE, STATE.NAME, People.Harmed) %>%
    group_by(EVTYPE, BEGIN_YEAR) %>% 
    summarize(Total.People.Harmed = sum(People.Harmed)) %>%
    group_by(EVTYPE) %>%
    summarize(Years.In.Range = n(),
              Total.People.Harmed = sum(Total.People.Harmed),
              Avg.Annual.People.Harmed = round(Total.People.Harmed / Years.In.Range, 0)) %>%
    arrange(-Avg.Annual.People.Harmed) %>%
    head(n = MAX_HARMS)

# Find all severe events by region
# Dollars in units of one thousand
myfile.df.summary.by.harms <- myfile.df.summary %>% 
    filter(EVTYPE %in% top.harms$EVTYPE) %>%
    select(REGION, STATE, EVTYPE, BEGIN_YEAR, People.Harmed) %>%
    group_by(REGION, EVTYPE, STATE) %>% 
    summarize(Total.People.Harmed = sum(People.Harmed)) %>%
    group_by(REGION, EVTYPE) %>%
    summarize(Years.In.Range = n(),
              Total.People.Harmed = sum(Total.People.Harmed),
              Avg.Annual.People.Harmed = round(Total.People.Harmed / Years.In.Range, 0)) %>%
    arrange(-Total.People.Harmed)
```

Overall, the storm type **TORNADO** has the biggest impact nationwide on
population health affecting more people in the **Southeast**
more than any other region.


```r
# Find the top most damaging storm types
# Measured in millions of dollars
top.damages <- myfile.df.summary %>% 
    select(EVTYPE, BEGIN_YEAR, STATE, STATE.NAME, Economic.Damage) %>%
    group_by(EVTYPE, BEGIN_YEAR) %>% 
    summarize(Total.Economic.Damage = sum(Economic.Damage)) %>%
    group_by(EVTYPE) %>%
    summarize(Years.In.Range = n(),
              Total.Economic.Damage = round(sum(Total.Economic.Damage) / 1000000, 0),
              Avg.Annual.Economic.Damage = round(Total.Economic.Damage / Years.In.Range, 0)) %>%
    arrange(-Avg.Annual.Economic.Damage) %>%
    head(n = MAX_HARMS)

# Find all severe events by region
# Dollars in units of one thousand
myfile.df.summary.by.damages <- myfile.df.summary %>% 
    filter(EVTYPE %in% top.damages$EVTYPE) %>%
    select(REGION, STATE, EVTYPE, BEGIN_YEAR, Economic.Damage) %>%
    group_by(REGION, EVTYPE, STATE) %>% 
    summarize(Total.Economic.Damage = round(sum(Economic.Damage) / 1000000, 0)) %>%
    group_by(REGION, EVTYPE) %>%
    summarize(Years.In.Range = n(),
              Total.Economic.Damage = sum(Total.Economic.Damage),
              Avg.Annual.Economic.Damage = round(Total.Economic.Damage / Years.In.Range, 0)) %>%
    arrange(-Total.Economic.Damage)
```

When we look at the economic impacts of dangerous storms, **HURRICANE/TYPHOON** does the
does the most damage to property and crops, affecting more people in the 
**Southeast** more than any other region.

# Results

Our goal in processing the data in this way is to be able to answer these two research questions

1. Which types of events are most harmful with respect to population health?

2. Which types of events have the greatest economic consequences?

