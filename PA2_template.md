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
           BGN_DATE, # Y/M/D HH:MM:SS in local time
           END_DATE # Y/MD/D HH:MM:SS in local time
           ) %>%
    filter(STATE %in% regions$STATE) %>%
    left_join(regions) %>%
    left_join(PROP.conversions) %>%
    left_join(CROP.conversions) %>%
    mutate(TOTALDMG =  (PROPDMG * PROPDMGMULT) + (CROPDMG * CROPDMGMULT), 
           TOTALHARMED = FATALITIES + INJURIES,
           BEGIN_YEAR = year(mdy_hms(BGN_DATE)),
           BEGIN_MONTH = month(mdy_hms(BGN_DATE)),
           END_YEAR = year(mdy_hms(END_DATE)),
           END_MONTH = month(mdy_hms(END_DATE)),
           BEGIN_YEARMONTH = as.factor(paste(BEGIN_YEAR, 
                                             formatC(BEGIN_MONTH, width = 2, flag = "0"), 
                                             sep = "-")),
           END_YEARMONTH = as.factor(paste(END_YEAR, 
                                           formatC(END_MONTH, width = 2, flag = "0"), 
                                           sep = "-")
           )) %>%
    # look at only recent data where there were harms or damages
    filter(BEGIN_YEAR >= 1996 & END_YEAR <= 2010 & TOTALDMG > 0 & TOTALHARMED > 0) %>%
    # get rid of the original metrics
    select(-PROPDMG, -CROPDMG, -FATALITIES, -INJURIES, -BGN_DATE, -END_DATE) %>%
    group_by(BEGIN_YEAR, 
             BEGIN_MONTH, 
             END_YEAR, 
             END_MONTH, 
             BEGIN_YEARMONTH, 
             END_YEARMONTH,
             REGION,
             STATE, 
             EVTYPE) %>% 
    summarize(People.Harmed = sum(TOTALHARMED), Economic.Damage = sum(TOTALDMG))

# pull out a couple of examples to illustrate
tornado.summary <- myfile.df.summary %>%
    filter(grepl(pattern = "tornado", x = EVTYPE, ignore.case = TRUE)) %>%
    group_by(BEGIN_YEAR) %>%
    summarize(Total.Cost = round(sum(Economic.Damage) / 1000, 0)) %>%
    mutate(Total.Cost.Pretty = paste("$", formatC(Total.Cost, format="d", big.mark=','), sep = ""))
tornado.worst.year <- tornado.summary %>%
    arrange(desc(Total.Cost), BEGIN_YEAR) %>%
    select(BEGIN_YEAR) %>%
    head(n = 1)
names(tornado.summary) <- c("Year", "Total.Cost", "Damages (Thousands of Dollars)")

arizona.summary <- myfile.df.summary %>%
    filter(STATE == "AZ") %>%
    group_by(EVTYPE) %>%
    summarize(Total.Cost = round(sum(Economic.Damage) / 1000, 0)) %>%
    mutate(Total.Cost.Pretty = paste("$", formatC(Total.Cost, format="d", big.mark=','), sep = "")) %>%
    arrange(desc(Total.Cost), EVTYPE)
arizona.worst.weather <- arizona.summary %>%
    head(n = 1) %>%
    select(EVTYPE)
names(arizona.summary) <- c("Weather Type", "Total.Cost", "Damages (Thousands of Dollars)")
```

We now have a smaller data set that is easier to work with.  For example, we can see that
tornados did the most economic damage in 1999: 


```r
print(xtable(tornado.summary[,c(1,3)], 
             caption = "Tornado Damages", display = c("d", "d", "s")), 
             type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Sat Sep 20 22:30:33 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Tornado Damages </CAPTION>
<TR> <TH> Year </TH> <TH> Damages (Thousands of Dollars) </TH>  </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> $626,719 </TD> </TR>
  <TR> <TD align="right"> 1997 </TD> <TD> $634,837 </TD> </TR>
  <TR> <TD align="right"> 1998 </TD> <TD> $1,572,986 </TD> </TR>
  <TR> <TD align="right"> 1999 </TD> <TD> $1,824,680 </TD> </TR>
  <TR> <TD align="right"> 2000 </TD> <TD> $364,290 </TD> </TR>
  <TR> <TD align="right"> 2001 </TD> <TD> $503,823 </TD> </TR>
  <TR> <TD align="right"> 2002 </TD> <TD> $634,264 </TD> </TR>
  <TR> <TD align="right"> 2003 </TD> <TD> $997,791 </TD> </TR>
  <TR> <TD align="right"> 2004 </TD> <TD> $318,714 </TD> </TR>
  <TR> <TD align="right"> 2005 </TD> <TD> $260,071 </TD> </TR>
  <TR> <TD align="right"> 2006 </TD> <TD> $574,504 </TD> </TR>
  <TR> <TD align="right"> 2007 </TD> <TD> $1,547,807 </TD> </TR>
  <TR> <TD align="right"> 2008 </TD> <TD> $1,366,671 </TD> </TR>
  <TR> <TD align="right"> 2009 </TD> <TD> $393,450 </TD> </TR>
  <TR> <TD align="right"> 2010 </TD> <TD> $740,023 </TD> </TR>
   </TABLE>

Or, we can see that **hail** did the most 
damage of all severe weather types in the state.


```r
print(xtable(arizona.summary[,c(1,3)], 
             caption = "Arizona Weather", display = c("d", "d", "s")), 
             type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Sat Sep 20 22:30:33 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Arizona Weather </CAPTION>
<TR> <TH> Weather Type </TH> <TH> Damages (Thousands of Dollars) </TH>  </TR>
  <TR> <TD> HAIL </TD> <TD> $1,818,000 </TD> </TR>
  <TR> <TD> TSTM WIND </TD> <TD> $193,814 </TD> </TR>
  <TR> <TD> FLASH FLOOD </TD> <TD> $13,585 </TD> </TR>
  <TR> <TD> THUNDERSTORM WIND </TD> <TD> $6,431 </TD> </TR>
  <TR> <TD> TORNADO </TD> <TD> $1,000 </TD> </TR>
  <TR> <TD> WILDFIRE </TD> <TD> $560 </TD> </TR>
  <TR> <TD> DUST STORM </TD> <TD> $454 </TD> </TR>
  <TR> <TD> TSTM WIND/HAIL </TD> <TD> $100 </TD> </TR>
  <TR> <TD> WINTER WEATHER </TD> <TD> $100 </TD> </TR>
  <TR> <TD> LIGHTNING </TD> <TD> $50 </TD> </TR>
  <TR> <TD> HIGH WIND </TD> <TD> $42 </TD> </TR>
  <TR> <TD> DRY MICROBURST </TD> <TD> $5 </TD> </TR>
  <TR> <TD> STRONG WIND </TD> <TD> $5 </TD> </TR>
  <TR> <TD> URBAN/SML STREAM FLD </TD> <TD> $1 </TD> </TR>
   </TABLE>

To address the research questions, we calculate two more summary tables.  First, we
look at the effects of severe weather on public health and the economy by region.  We use
the following definitions:

* **people harmed** is defined as the total number of people who suffered injury or were killed
* **economic damage** is measured in terms of the total dollar value of damages to crops and property

First, we summarize these measurements by weather type and region.


```r
# Find all severe events by region
# Dollars in units of one thousand
myfile.df.summary.by.region <- myfile.df.summary %>% 
    select(REGION, EVTYPE, BEGIN_YEAR, People.Harmed, Economic.Damage) %>%
    group_by(REGION, EVTYPE) %>% 
    summarize(Years.In.Range = n(),
              Total.People.Harmed = sum(People.Harmed),
              Avg.Annual.People.Harmed = Total.People.Harmed / Years.In.Range,
              Total.Economic.Damage = sum(Economic.Damage) / 1000,
              Avg.Annual.Economic.Damage = (Total.Economic.Damage / Years.In.Range)/1000) %>%
    select(-Years.In.Range) %>%
    arrange(-Total.Economic.Damage, -Total.People.Harmed)
```

Here are the first 10 rows of our summary table:


```r
print(xtable(myfile.df.summary.by.region[1:10,], 
             caption = "Health and Economic Harms by Region<br />Damages in Thousands of Dollars",
             display = c("s", "s", "s", "d", "d", "d", "d")), 
             type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Sat Sep 20 22:30:33 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Health and Economic Harms by Region<br />Damages in Thousands of Dollars </CAPTION>
<TR> <TH> REGION </TH> <TH> EVTYPE </TH> <TH> Total.People.Harmed </TH> <TH> Avg.Annual.People.Harmed </TH> <TH> Total.Economic.Damage </TH> <TH> Avg.Annual.Economic.Damage </TH>  </TR>
  <TR> <TD> Southeast </TD> <TD> HURRICANE/TYPHOON </TD> <TD align="right"> 974 </TD> <TD align="right">  74 </TD> <TD align="right"> 32517480 </TD> <TD align="right"> 2501 </TD> </TR>
  <TR> <TD> Southeast </TD> <TD> TORNADO </TD> <TD align="right"> 7930 </TD> <TD align="right">  21 </TD> <TD align="right"> 5466739 </TD> <TD align="right">  14 </TD> </TR>
  <TR> <TD> Southwest </TD> <TD> TROPICAL STORM </TD> <TD align="right">  29 </TD> <TD align="right">   7 </TD> <TD align="right"> 5442480 </TD> <TD align="right"> 1360 </TD> </TR>
  <TR> <TD> Southwest </TD> <TD> STORM SURGE/TIDE </TD> <TD align="right">  11 </TD> <TD align="right">  11 </TD> <TD align="right"> 4000000 </TD> <TD align="right"> 4000 </TD> </TR>
  <TR> <TD> Southeast </TD> <TD> HURRICANE </TD> <TD align="right">  59 </TD> <TD align="right">   4 </TD> <TD align="right"> 3864540 </TD> <TD align="right"> 276 </TD> </TR>
  <TR> <TD> Midwest </TD> <TD> TORNADO </TD> <TD align="right"> 3894 </TD> <TD align="right">  17 </TD> <TD align="right"> 3362803 </TD> <TD align="right">  15 </TD> </TR>
  <TR> <TD> West </TD> <TD> WILDFIRE </TD> <TD align="right"> 522 </TD> <TD align="right">  16 </TD> <TD align="right"> 3131954 </TD> <TD align="right"> 101 </TD> </TR>
  <TR> <TD> Southeast </TD> <TD> HIGH WIND </TD> <TD align="right"> 218 </TD> <TD align="right">   4 </TD> <TD align="right"> 2681120 </TD> <TD align="right">  52 </TD> </TR>
  <TR> <TD> Southwest </TD> <TD> HAIL </TD> <TD align="right"> 115 </TD> <TD align="right">   5 </TD> <TD align="right"> 2294648 </TD> <TD align="right"> 114 </TD> </TR>
  <TR> <TD> Southwest </TD> <TD> HURRICANE/TYPHOON </TD> <TD align="right">   9 </TD> <TD align="right">   4 </TD> <TD align="right"> 2260380 </TD> <TD align="right"> 1130 </TD> </TR>
   </TABLE>

Then, we look at the total effects of each weather type across the country year by year.


```r
# Find all severe events by year
# Dollars in units of one thousand

myfile.df.summary.by.event <- myfile.df.summary %>% 
    select(BEGIN_YEAR, EVTYPE, People.Harmed, Economic.Damage) %>%
    group_by(BEGIN_YEAR, EVTYPE) %>%
    summarize(Total.People.Harmed = sum(People.Harmed),
              Total.Economic.Damage = sum(Economic.Damage) / 1000) %>%
    arrange(BEGIN_YEAR, -Total.Economic.Damage)
```

Here are the first 10 rows of our summary table (dollars reported in thousands):


```r
print(xtable(myfile.df.summary.by.event[1:10,], 
             caption = "Harmful Weather Events Across the Country<br/>Damages in Thousands of Dollars",
             display = c("d", "d", "s", "d", "d")), 
             type = "html", include.rownames = FALSE)
```

<!-- html table generated in R 3.1.1 by xtable 1.7-3 package -->
<!-- Sat Sep 20 22:30:33 2014 -->
<TABLE border=1>
<CAPTION ALIGN="bottom"> Harmful Weather Events Across the Country<br/>Damages in Thousands of Dollars </CAPTION>
<TR> <TH> BEGIN_YEAR </TH> <TH> EVTYPE </TH> <TH> Total.People.Harmed </TH> <TH> Total.Economic.Damage </TH>  </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> HURRICANE </TD> <TD align="right">  22 </TD> <TD align="right"> 1306200 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> TORNADO </TD> <TD align="right"> 797 </TD> <TD align="right"> 626719 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> FLOOD </TD> <TD align="right">  39 </TD> <TD align="right"> 590940 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> HIGH WIND </TD> <TD align="right"> 106 </TD> <TD align="right"> 183042 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> FLASH FLOOD </TD> <TD align="right">  87 </TD> <TD align="right"> 172195 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> WILD/FOREST FIRE </TD> <TD align="right">  31 </TD> <TD align="right"> 45150 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> TROPICAL STORM </TD> <TD align="right">   1 </TD> <TD align="right"> 44600 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> TSTM WIND </TD> <TD align="right"> 308 </TD> <TD align="right"> 43344 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> BLIZZARD </TD> <TD align="right"> 179 </TD> <TD align="right"> 39415 </TD> </TR>
  <TR> <TD align="right"> 1996 </TD> <TD> HAIL </TD> <TD align="right">  81 </TD> <TD align="right"> 37131 </TD> </TR>
   </TABLE>

# Results


