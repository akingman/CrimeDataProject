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

#Only include LARCENY/THEFT, NON-CRIMINAL, DRUG/NARCOTIC
#These are the top three excluding OTHER. 
#mark everything else as OTHER
levels(data$Category) = c(levels(data$Category), "OTHER")
data$Category[!(data$Category %in% 
    c("LARCENY/THEFT", "NON-CRIMINAL", "ASSAULT"))] = "OTHER"
data$Category = droplevels(data$Category)


#train and test
set.seed(100)
trainind = sample(seq(1:nrow(data)), 50000)

train= data[trainind,]
test = data[-trainind,]

#maketree
before = Sys.time()
t = ctree(Category~., data = train)
totalTime = Sys.time() - before
totalTime


plot(t)

#predict
pred = predict(t, test)
tab = table(pred, test$Category)
error = (nrow(test) - sum(diag(tab)))/nrow(test)
error


#calculate logloss
probpred = predict(t, test, type="prob")
N = nrow(test)
probs = rep(1,N)
for (i in seq(1,N)){
  class = test$Category[i]
  prob = probpred[i, class]
  prob = max(min(prob,1-10^(-15)), 10^(-15))
  probs[i] = log(prob)
}
logloss = -sum(probs)/N
logloss

save.image()

