require(partykit)
require(lubridate)
require(dplyr)
require(ggplot2)

#move out two directories since data file is too large for github
mydata = read.csv("~/College/Computational Statistics/Project/train.csv", header=TRUE)

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
topcats = mydata %>% group_by(Category) %>% summarise(Count = n()) %>% top_n(10, Count)
trimmedcats = mydata %>% filter(Category %in% topcats$Category)

#plotting wrt district
ggplot(data = recentdata, aes(x=PdDistrict)) + geom_histogram(aes(fill=PdDistrict))

#plotting wrt hour
ggplot(data = recentdata, aes(x=Hour)) + geom_line(stat="bin", binwidth=1) + coord_cartesian(xlim = c(1, 24)) 

#plotting wrt week
ggplot(data = recentdata, aes(x=Week)) + geom_bar(stat="bin", binwidth=2) + geom_line(stat="bin", binwidth=2, aes(color=Year))  + coord_cartesian(xlim = c(1, 52))

#plotting top categories by year
ggplot(data = trimmedcats, aes(x=Year, color=Category)) + geom_line(stat="bin", binwidth = 1) + geom_point(stat="bin", binwidth = 1) + coord_cartesian(xlim = c(2003, 2015))

#plotting top categories by year, separated by district
ggplot(data = trimmedcats, aes(x=Year, color=Category)) + geom_line(stat="bin", binwidth = 1) + geom_point(stat="bin", binwidth = 1) + coord_cartesian(xlim = c(2003, 2015)) + facet_wrap(~PdDistrict)
