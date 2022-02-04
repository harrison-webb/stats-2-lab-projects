#3080 lab 4

#Statistics Intervals

#Confidence Intervals
#range of values that when computed on 100 sets of data, the true parameter
#will be in that range in 95 at least
#"interval is random"
#interval narrows with more data

#Credible Intervals
#take a bayesian perspective
#Assume that the "parameter is random"
#We can be 95% sure the parameter is in a given confidence interval


#Prediction Interval
#Calculate an interval based on observations x1, x2, ..., x_n
#Want to be 95% sure that x_n+1 is in this interval
#very dependent on the distribution of the data
#interval does not go to zero with more data

#Normal Example


#tolerance interval
#interval that contains 100K% fo the data with confidence 100C%
install.packages("tolerance")

#call with normtol.int(data, alpha, proportion of population, size #)