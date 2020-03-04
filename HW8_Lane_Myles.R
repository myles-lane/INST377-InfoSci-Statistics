# INST 314 Homework 8 
# Myles Lane
# this data comes from: http://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
# learn more about it and interesting R examples also here: https://rstudio-pubs-static.s3.amazonaws.com/98475_c0697af6a9b045239a367f9190fdb12d.html 
library(car)

# load data
bike <- read.csv(file.choose(), header=T)  # load day.csv

# use starting with Q1a
# un-normalize select variables
bike$atemp.u <- bike$atemp *50
bike$temp.u <- bike$temp *41
bike$hum.u  <- bike$hum *100
bike$wind.u <- bike$windspeed *67
# drop normalized variables so you don't accidentally use them later.
bike[,c("atemp", "temp", "hum", "windspeed")] <- NULL


# create seasonal subsets for use with question Q1e
bike.spring <- subset(bike, bike$season==1)
bike.summer <- subset(bike, bike$season==2)
bike.fall <- subset(bike, bike$season==3)
bike.winter <- subset(bike, bike$season==4)

# for Q1k
bike[469,]


#1A
reg.bike <- lm(bike$cnt ~ factor(bike$season) + bike$mnth + bike$holiday + bike$workingday
               + bike$temp.u + bike$hum.u + bike$wind.u)
summary(reg.bike)

#1C
plot(reg.bike)

#1D 
lm.bike <- lm(bike$cnt ~ factor(bike$season) + bike$mnth + bike$holiday + bike$workingday
              + bike$temp.u + bike$hum.u + bike$wind.u)
round(vif(lm.bike),1)
#remove season
lm.bike <- lm(bike$cnt ~ bike$mnth + bike$holiday + bike$workingday
              + bike$temp.u + bike$hum.u + bike$wind.u)
round(vif(lm.bike),1)

#1E 
#spring
reg.bikespring <- lm(bike.spring$cnt ~ factor(bike.spring$mnth) + bike.spring$holiday + bike.spring$workingday
                     + bike.spring$temp.u + bike.spring$hum.u + bike.spring$wind.u)
summary(reg.bikespring)
#summer
reg.bikesummer <- lm(bike.summer$cnt ~ factor(bike.summer$mnth) + bike.summer$holiday + bike.summer$workingday
                     + bike.summer$temp.u + bike.summer$hum.u + bike.summer$wind.u)
summary(reg.bikesummer)
#fall
reg.bikefall <- lm(bike.fall$cnt ~ factor(bike.fall$mnth) + bike.fall$holiday + bike.fall$workingday
                     + bike.fall$temp.u + bike.fall$hum.u + bike.fall$wind.u)
summary(reg.bikefall)
#winter
reg.bikewinter <- lm(bike.winter$cnt ~ factor(bike.winter$mnth) + bike.winter$holiday + bike.winter$workingday
                     + bike.winter$temp.u + bike.winter$hum.u + bike.winter$wind.u)
summary(reg.bikewinter)

#1J
total = 5193 + (-145 * 1) +  (-949 * 1) + (-247 * 0) + (182 *19) + (-49 * 61) + (-67 * 18)
total

#1K
totalK = 5193 + (-145 * 1) +  (-949 * 0) + (-247 * 1) + (182 *22) + (-49 * 83) + (-67 * 7)
totalK

