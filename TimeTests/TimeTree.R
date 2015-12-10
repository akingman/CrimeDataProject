require(Formula)
require(partykit)
require(lubridate)
require(dplyr)
require(ggplot2)


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


treeTimes = data.frame()
for (samplesize in seq(100,1000,100)){
  print(samplesize)
  times = c()
  for (trial in seq(1,3)){
    print(trial)
    trainingsize = 0.70*samplesize
    trainind = sample(seq(1, nrow(data)), samplesize)
    testind = trainind[trainingsize+1:samplesize]
    trainind = trainind[1:trainingsize]

    train= data[trainind,]
    test = data[testind,]
    
    before = Sys.time()
    t = ctree(Category~., data = train)
    total=Sys.time()-before
    times = c(times, total)
  }
  treeTimes = rbind(treeTimes, c(samplesize,mean(times)))
}

names(treeTimes) = c("samplesize", "time")
treeTimes %>% ggplot(aes(x=samplesize, y=time)) + geom_point() + ylab("minutes")
