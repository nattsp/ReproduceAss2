#example assignment
#http://www.rpubs.com/rdpeng/13396
library(dplyr)
library(ggplot2)
library(tidyr)
options(scipen=9)
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
dateDownloaded

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
               INJURIES > 0) %>%
    group_by(EVTYPE) %>%
    summarise(Fatalities = sum(FATALITIES), 
              Injuries = sum(INJURIES)) %>%
    arrange(desc(Fatalities), desc(Injuries))
head(harmful)

harmful10 <- harmful[1:10, ]

harmfultall <- gather(harmful10, TypeOfHarm, Numbers, Fatalities:Injuries)
harmfultall$EVTYPE <- factor(harmfultall$EVTYPE,
                           levels = harmful10$EVTYPE, ordered = TRUE)


g <- ggplot(data = harmfultall, aes(x = EVTYPE, y = Numbers, fill = TypeOfHarm))
g <- g + geom_bar(stat="identity", position="dodge")
g <- g + coord_cartesian(ylim=c(0,20000))
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g <- g + labs(title = "Bar chart of deaths and injuries by event type")
g <- g + labs(x = "Event Type", y = "Number of injuries over 20,000 not shown")

g




harmful2 <- group_by(harmful, EVTYPE,) %>%
    summarise(Fatalies = sum(FATALITIES))

#Across the United States, 
#which types of events have the greatest economic consequences?

table(storms$PROPDMGEXP)
table(storms$CROPDMGEXP)

convertPROPDMGEXP <- data.frame(PROPDMGEXP = c("H", "K", "M", "B", ""), 
                           PROPDMGfactor = c(100, 1000, 1000000, 1000000000, 0))
convertCROPDMGEXP <- data.frame(CROPDMGEXP = c("H", "K", "M", "B", ""), 
                                CROPDMGfactor = c(100, 1000, 1000000, 1000000000, 0))

damage <- select(storms,
                EVTYPE,
                PROPDMG,
                PROPDMGEXP,
                CROPDMG,
                CROPDMGEXP) %>%
    mutate_each(funs(toupper), PROPDMGEXP, CROPDMGEXP) %>%
    filter(PROPDMGEXP %in% c("H", "K", "M", "B") |
                         CROPDMGEXP %in% c("H", "K", "M", "B")) %>%
    filter(PROPDMGEXP %in% c("H", "K", "M", "B", "") &
                     CROPDMGEXP %in% c("H", "K", "M", "B", "")) %>%
    left_join(convertPROPDMGEXP, c("PROPDMGEXP")) %>%
    left_join(convertCROPDMGEXP, c("CROPDMGEXP")) %>%
    mutate(PropertyCosts = PROPDMG * PROPDMGfactor,
           CropCosts = CROPDMG * CROPDMGfactor,
           TotalDamage = PropertyCosts + CropCosts) %>%
    arrange(desc(TotalDamage), desc(PropertyCosts), desc(CropCosts))


head(damage)

damageEVTYPE <- damage %>%
    select(EVTYPE, TotalDamage) %>%
    group_by(EVTYPE) %>%
    summarize(TotalDamage = sum(TotalDamage)) %>%
    arrange(desc(TotalDamage))



EVTYPETop10 <- damageEVTYPE[1:10, ]$EVTYPE

damageTotalTop10 <- damageEVTYPE[1:10, ]

g <- ggplot(data = damageTotalTop20, aes(x = EVTYPE, y = TotalDamage))
g <- g + geom_bar(stat="identity")
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g <- g + labs(title = "Bar chart showing  costs for 10 events with greatest economic consequency")
g <- g + labs(x = "Event Type", y = "Log of costs")

g

##Look at the range of costs for the largest damages
damageTop10 <- filter(damage, EVTYPE %in% EVTYPETop10) %>%
    select(EVTYPE, PropertyCosts, CropCosts)

damageTop10$EVTYPE <- factor(damageTop10$EVTYPE,
                             levels = EVTYPETop10, ordered = TRUE)

damageTop10 <- gather(damageTop10, DamageType, Dollars, PropertyCosts:CropCosts)


#distinct(select(damage, EVTYPE))



g <- ggplot(data = damageTop10, aes(x = EVTYPE, y = log10(Dollars + 1), fill = DamageType))
g <- g + geom_boxplot()
#g <- g + coord_flip()
g <- g + theme(axis.text.x = element_text(angle = 90, hjust = 1))
g <- g + labs(title = "Box plot showing distribution of Log of damage costs for 10 events with greatest economic consequency")
g <- g + labs(x = "Event Type", y = "Log of costs")

g



g <- ggplot(data = damage, aes(x = "const", y = log10(TotalDamage + 1)))
g <- g + geom_boxplot()
g <- g + coord_flip()
g


table(damage$PROPDMGEXP)
table(damage$CROPDMGEXP)


filter(damage, PROPDMGEXP >= 0, PROPDMGEXP < 9)
filter(damage, PROPDMGEXP =="?")




summary(damage)



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
