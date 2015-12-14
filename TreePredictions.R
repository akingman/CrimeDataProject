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


#save a copy of the data with all of the original categories
origdata = data

#Include all categories with more than 10,000 observations.
#Make everything else OTHER

include = c("LARCENY/THEFT", "NON-CRIMINAL", "ASSAULT", "DRUG/NARCOTIC", 
            "VANDALISM", "WARRANTS", "VEHICLE THEFT", "BURGLARY", 
            "SUSPICIOUS OCC", "MISSING PERSON", "ROBBERY", "FRAUD")

levels(data$Category) = c(levels(data$Category), "OTHER")
data$Category[!(data$Category %in% include)] = "OTHER"
data$Category = droplevels(data$Category)


#train and test
set.seed(100)
trainsize = 50000
trainind = sample(seq(1:nrow(data)), trainsize)

train= data[trainind,]
test = data[-trainind,]

#maketree
before = Sys.time()
t = ctree(Category~., data = train)
totalTime = Sys.time() - before
totalTime


plot(t)

###### Calculations on test data with condenses # of categories


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





####### Calculations on test data with original categies
#error
origtest = origdata[-trainind, ]
levels(origtest$Category) = c(levels(origtest$Category), "OTHER")
origtest$Category[origtest$Category == "OTHER OFFENSES"] =  "OTHER"
origtest= droplevels(origtest)
origpred = predict(t, origtest)
levels(origpred) = levels(origtest$Category)
origtab = table(origpred, origtest$Category)
origerror = (nrow(origtest) - sum(diag(origtab)))/nrow(origtest)
origerror

#logloss
probpred = predict(t, origtest, type="prob")
N = nrow(origtest)
probs = rep(1,N)
for (i in seq(1,N)){
  class = origtest$Category[i]
  class = droplevels(class)
  if (class %in% include){
    prob = probpred[i, class]
  }
  else{
    prob = 0
  }
  prob = max(min(prob,1-10^(-15)), 10^(-15))
  probs[i] = log(prob)
}
origlogloss = -sum(probs)/N
origlogloss



### Save
filename = paste(trainsize, "_13cats.RData", sep="")
save.image(file=filename)



