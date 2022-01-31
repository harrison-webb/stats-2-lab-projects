#Week 3, ANOVA
#Multiple groups where we want to compare means
#Must make sure that variance doesnt depend on group


#check assumptions
#normality - qqplot
#shapiro.test
shapiro.test(iris$Sepal.Width)

#equal variances
boxplot(Sepal.Width~Species, data = iris)
#looks like equal spread, good to go

#oneway.test
oneway.test(Sepal.Width~Species, data = iris, var.equal = TRUE)

#aov()
res = aov(Sepal.Width~Species, data = iris)
summary(res) #do this in order to look at p value

#lm()
model1 = lm(Sepal.Width~Species, data = iris)
summary(model1)



#Finding the difference
TukeyHSD(res, conf.level = 0.9) #gives us a bunch of confidence intervals
plot(TukeyHSD(res))
