#HW5 Myles Lane
#INST413

#Q1
regular <- c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
premium <- c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
t.test(regular, premium, "greater", paired=T)

#Q2
comicMovies <- read.csv(file.choose())
attach(comicMovies)
summarytools::descr(Worldwide[Studio=="DC"])
summarytools::descr(Worldwide[Studio=="Marvel"])
summarytools::descr(Worldwide)
t.test(Worldwide[Studio=="DC"], Worldwide[Studio=="Marvel"], conf.level = 0.9, var.equal = T )
cohen.d(Worldwide[Studio=="DC"],Worldwide[Studio=="Marvel"] )

#Q3
summarytools::descr(Review[Studio=="Marvel"&Year<=2009]) # Pre Disney
summarytools::descr(Review[Studio=="Marvel"&Year>2009])  # Post Disney
summarytools::descr(Review[Studio=="Marvel"])   # total
t.test((Studio=="Marvel"&Year<=2009),(Studio=="Marvel"&Year>2009), paired=F, conf.level = 0.9, var.equal = T )

#Q4
pwr.t2n.test(n1=63, n2=63,d=0.5, sig.level = 0.05, power = NULL, "two.sided")