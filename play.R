#example assignment
#http://www.rpubs.com/rdpeng/13396
library(dplyr)
#Download data
if (!file.exists("data")){
    dir.create("data")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists(".\\data\\repdata-data-StormData.csv.bz2")){
    download.file(fileUrl, destfile = ".\\data\\repdata-data-StormData.csv.bz2", 
                  mode = "wb")
    dateDownloaded <- date()
    dateDownloaded
}

#Read in data
storms <- read.csv(".\\data\\repdata-data-StormData.csv.bz2")
str(storms)
dim(storms)

#make events all uppercase
storms$EVTYPE <- toupper(storms$EVTYPE)
table(storms$EVTYPE)

#take only those events that cause injuries or fatalities
harmful <- select(storms,
                  EVTYPE, 
                  FATALITIES, 
                  INJURIES) %>%
    filter(FATALITIES > 0 |
               data$INJURIES > 0) %>%
    group_by(EVTYPE) %>%
    summarise(Fatalities = sum(FATALITIES), 
              Injuries = sum(INJURIES)) %>%
    arrange(desc(Fatalities), desc(Injuries))
head(harmful)

harmful2 <- group_by(harmful, EVTYPE,) %>%
    summarise(Fatalies = sum(FATALITIES))



#902297 rows if not specified
data <- read.csv("repdata-data-StormData.csv.bz2")

str(data)
names(data)
length(data$STATE__) == length(data$STATE)
table(data$EVTYPE)
table(data$FATALITIES)
hist(data$FATALITIES)
table(data$INJURIES)
harm <- filter(data, FATALITIES > 0 |
                 data$INJURIES > 0)
table(harm$FATALITIES, harm$INJURIES)
events <- table(harm$EVTYPE)
arrange(harm, desc(FATALITIES), desc(INJURIES))


#####################
harmful <- select(data,
                  EVTYPE, 
                  FATALITIES, 
                  INJURIES) %>%
    filter(FATALITIES > 0 |
               data$INJURIES > 0) %>%
    arrange(desc(FATALITIES), desc(INJURIES))

harmful$EVTYPE <- tolower(harmful$EVTYPE)
harmful2 <- group_by(harmful, EVTYPE) %>%
    summarise(Fatalies = sum(FATALITIES))

Fat <- table(harmful$EVTYPE, harmful$FATALITIES)
