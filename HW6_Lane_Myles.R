#Myles Lane HW6

library(summarytools)
library(DescTools)
library(car)
library(sjstats)
diamonds <- read.csv(file.choose())

#Q4a
#Get to know data
str(diamonds)
summarytools::descr(diamonds$Carat)
summarytools::freq(diamonds$Colour)

#Test Normality
qqnorm(diamonds$Carat[diamonds$Colour=="D"])
qqline(diamonds$Carat[diamonds$Colour=="D"])
hist(diamonds$Carat[diamonds$Colour=="D"])

qqnorm(diamonds$Carat[diamonds$Colour=="F"])
qqline(diamonds$Carat[diamonds$Colour=="F"])
hist(diamonds$Carat[diamonds$Colour=="F"])

qqnorm(diamonds$Carat[diamonds$Colour=="G"])
qqline(diamonds$Carat[diamonds$Colour=="G"])
hist(diamonds$Carat[diamonds$Colour=="G"])

qqnorm(diamonds$Carat[diamonds$Colour=="H"])
qqline(diamonds$Carat[diamonds$Colour=="H"])
hist(diamonds$Carat[diamonds$Colour=="H"])

qqnorm(diamonds$Carat[diamonds$Colour=="I"])
qqline(diamonds$Carat[diamonds$Colour=="I"])
hist(diamonds$Carat[diamonds$Colour=="I"])

#levene 
leveneTest(diamonds$Carat~diamonds$Colour, center = mean)
leveneTest(diamonds$Carat~diamonds$Colour, center = median)
#mean & variance
aggregate(Carat~Colour, data=diamonds, FUN=mean)
aggregate(Carat~Colour, data=diamonds, FUN=sd)
#ANOVA
analysis.carats <- aov(diamonds$Carat~diamonds$Colour)
summary(analysis.carats)

qf(p=0.05, df1=5, df2=302, lower.tail = F) # Find F critical value
pf(q=2.11, df1=5, df2=302, lower.tail = F) # Find F p-value


#Tukey
TukeyHSD(analysis.carats)
#Effect size
eta_sq(analysis.carats)


#Q4b

#Normality
qqnorm(diamonds$Carat[diamonds$Clarity=="IF"])
qqline(diamonds$Carat[diamonds$Clarity=="IF"])
hist(diamonds$Carat[diamonds$Clarity=="IF"])

qqnorm(diamonds$Carat[diamonds$Clarity=="VVS1"])
qqline(diamonds$Carat[diamonds$Clarity=="VVS1"])
hist(diamonds$Carat[diamonds$Clarity=="VVS1"])

qqnorm(diamonds$Carat[diamonds$Clarity=="VVS2"])
qqline(diamonds$Carat[diamonds$Clarity=="VVS2"])
hist(diamonds$Carat[diamonds$Clarity=="VVS2"])

qqnorm(diamonds$Carat[diamonds$Clarity=="VS1"])
qqline(diamonds$Carat[diamonds$Clarity=="VS1"])
hist(diamonds$Carat[diamonds$Clarity=="VS1"])

qqnorm(diamonds$Carat[diamonds$Clarity=="VS2"])
qqline(diamonds$Carat[diamonds$Clarity=="VS2"])
hist(diamonds$Carat[diamonds$Clarity=="VS2"])

#levene 
leveneTest(diamonds$Carat~diamonds$Clarity, center = mean)
leveneTest(diamonds$Carat~diamonds$Clarity, center = median)
#ANOVA
analysis.clarity <- aov(diamonds$Carat~diamonds$Clarity)
summary(analysis.clarity)

qf(p=0.05, df1=4, df2=303, lower.tail = F) # Find F critical value
pf(q=15.22, df1=4, df2=303, lower.tail = F) # Find F p-value

#Tukey
TukeyHSD(analysis.clarity)
#Effect
eta_sq(analysis.clarity)


#5c-5e
movies <- read.csv(file.choose())
str(movies)

movies_sub <- subset(movies, movies$title_year==2013 |
                       movies$title_year==2014 |
                     movies$title_year==2015 |
                     movies$title_year==2016)

analysis.moviesAOV <- aov(movies_sub$budget~as.factor(movies_sub$title_year))
summary(analysis.moviesAOV)
#Levene
LeveneTest(movies_sub$budget~as.factor(movies_sub$title_year), center = mean)
LeveneTest(movies_sub$budget~as.factor(movies_sub$title_year), center = median)
#Normality
qqnorm(movies_sub$budget[movies_sub$title_year==2013])
qqline(movies_sub$budget[movies_sub$title_year==2013])
qqnorm(movies_sub$budget[movies_sub$title_year==2014])
qqline(movies_sub$budget[movies_sub$title_year==2014])
qqnorm(movies_sub$budget[movies_sub$title_year==2015])
qqline(movies_sub$budget[movies_sub$title_year==2015])
qqnorm(movies_sub$budget[movies_sub$title_year==2016])
qqline(movies_sub$budget[movies_sub$title_year==2016])
#mean & variance
aggregate(budget~title_year, data=movies_sub, FUN=mean)
aggregate(budget~title_year, data=movies_sub, FUN=sd)
#Kruskal
kruskal.test(movies_sub$budget~as.factor(movies_sub$title_year))
#Dunn
DunnTest(movies_sub$budget, as.factor(movies_sub$title_year), method="bonferroni")
