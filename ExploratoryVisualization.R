require(partykit)
require(lubridate)
require(dplyr)
require(ggplot2)

mydata = read.csv("~/Desktop/train.csv", header=TRUE)

#take out hour, month, and year
mydata = mydata %>% 
  mutate(Time = ymd_hms(Dates)) %>%
  mutate(Hour = hour(Time),
         HourF = as.factor(hour(Time)), 
         Day = as.factor(day(Time)),
         Week = week(Time),
         Month = as.factor(month(Time)), 
         Year = year(Time))

#remove description, resolution address, Date, Time
mydata = mydata[,-c(1,3,6,7)]

#examine only data since 2010
recentdata = filter(mydata, Year > 2010)

#examining only major crime categories
topcats = mydata %>% group_by(Category) %>% summarise(Count = n()) %>% top_n(13, Count)
trimmedcats = mydata %>% filter(Category %in% topcats$Category)

#plotting wrt district
ggplot(data = recentdata, aes(x=PdDistrict)) + geom_histogram(aes(fill=PdDistrict))

#plotting wrt x
ggplot(data = recentdata, aes(x=X)) + geom_histogram()

#plotting wrt y
ggplot(data = recentdata, aes(x=Y)) + geom_histogram()

#plotting wrt hour
ggplot(data = trimmedcats, aes(x=Hour)) + geom_histogram(binwidth=1) + coord_cartesian(xlim = c(1, 24))  + facet_wrap(~Category)

#plotting wrt week
ggplot(data = recentdata, aes(x=Week)) + geom_bar(stat="bin", binwidth=2) + geom_line(stat="bin", binwidth=2, aes(color=Year))  + coord_cartesian(xlim = c(1, 52))

#plotting top categories by year
ggplot(data = trimmedcats, aes(x=Year)) + geom_line(stat="bin", binwidth = 1) + geom_point(stat="bin", binwidth = 1) + coord_cartesian(xlim = c(2003, 2015)) + facet_wrap(~Category)

#plotting top categories by year, separated by district
ggplot(data = trimmedcats, aes(x=Year, color=Category)) + geom_line(stat="bin", binwidth = 1) + geom_point(stat="bin", binwidth = 1) + coord_cartesian(xlim = c(2003, 2015)) + facet_wrap(~PdDistrict) + theme(axis.ticks = element_blank(), axis.text.x = element_blank())
