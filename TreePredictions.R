require(Formula)
require(partykit)
require(lubridate)
require(dplyr)

#move out a directory since data file is to large for github
data= read.csv("../train.csv")

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


#small tests
set.seed(100)
trainobs = sample(seq(1, nrow(data)), 1000)
set.seed(107)
testobs = sample(seq(1, nrow(data)), 100)


#large test
#set.seed(105)
#trainind = sample(seq(1,nrow(data)), floor(nrow(data)*(2/3)))

train= data[trainobs,]
test = data[testobs,]
t = ctree(Category~., data = train)

save.image()

plot(t)


pred = predict(t, test)
tab = table(pred, test$Category)
error = (nrow(test) - sum(diag(tab)))/nrow(test)
error

save.image()

