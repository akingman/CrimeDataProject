require(Formula)
require(partykit)
require(lubridate)
require(dplyr)

data= read.csv("train.csv")

#take out hour, month, and year
data = data %>% 
  mutate(Time = ymd_hms(Dates)) %>%
  mutate(hour = as.factor(hour(Time)), 
         month = as.factor(month(Time)), 
         year=year(Time))

#remove description, resolution adress, Date, Time
data = data[,-c(1,3,6,7,10)]

#only data from 2006 and before
data = data %>%
  filter(year >2005)
data$year = as.factor(data$year)



train= data[seq(1:500),]
test = data[seq(501:1000),]
t = ctree(Category~., data = train)
predict(t, test)
plot(t)


# use only data from 2006 and on
#remove description, resolution adress
#time of day: categorical by hour
#month
#year
#keep district, dayofweek, x, y as is