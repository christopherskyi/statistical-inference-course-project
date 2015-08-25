###############################################################################################

## Coursera Statistical Inference Course Project
## Christopher Skyi
## Sunday, August 23, 2015, New York City, [86-74]F, 60% chance of thunderstorms by evening

# simulation.R Description:

# With this R script, I will investigate the exponential distribution in R and compare it with 
# the Central Limit Theorem. 
# 
# PART 1 - SIMULATIONs:

# The exponential distribution will be simulated in R with rexp(n, lambda) 
# where lambda is the rate parameter. 
# The mean & standard deviation of the exponential distribution is 1/lambda.
# 
# I'll set lambda = 0.2 for all of the simulations. 
# 
# I  will investigate the distribution of averages of 40 exponentials. 
# 
# Note: I will do a thousand simulations.
# Note: I will use set.seed(20) to ensures reproducibility of the sequence of random numbers
#       generated in all the simulations

# Illustrate via simulation and associated explanatory text 
# the properties of the distribution of the mean of 40 exponentials.  
# You should
# 1. Show the sample mean and compare it to the theoretical mean of the distribution.
# 2. Show how variable the sample is (via variance) and compare it to the theoretical variance of the distribution.
# 3. Show that the distribution is approximately normal.

###############################################################################################

########################################

# PART 1 - SIMULATION EXERCISE:

########################################
library(ggplot2)

nosim <- 1000                  # number of samples (of size 40)
lambda = 0.2                   # rate parameter
exp.mean = exp.sd = 1/lambda   # theorectial exponential mean and standard deviation = 5
sample.size = 40               # sample size

# Simulate 1000 means from 1000 sample sizes of size 40
set.seed(20) # ensures reproducibility of the sequence of random numbers  
sample.means = NULL
for (i in 1 : nosim) sample.means = c(sample.means, mean(rexp(sample.size, lambda)))

# get the average of those 1000 means
mean.of.sample.means = sum(sample.means)/nosim

# get the variance of these 1000 sample means
# Note: code below gives same result as var(sample.means)
diff.sqr <- (sample.means - mean.of.sample.means)^2
sample.var = sum(diff.sqr)/(length(diff.sqr)-1) # 0.5919712

# From p. 37 in course book,
# the variance of the sample mean is: VAR(sample.means) = exp.sd^2/sample.size
# > sample.var
# [1] 0.5919712
# > var(sample.means) = sample.var
# [1] 0.5919712
# > sd(sample.means)
# [1] 0.7693967
# > exp.sd/sqrt(sample.size)
# [1] 0.7905694
# > exp.sd^2/sample.size
# [1] 0.625

# get the sd of this distribution: it should be theoretical (pop) sd / sqrt(sample.size)
sd(sample.means) #  0.7693967
exp.sd/sqrt(sample.size) # 0.7905694

# Sample Variance (sample.var = 0.5919712) versus Theoretical Variance (std. error of the mean, p. 37)
# is the variance of the exponential distribution, 25, divided by the sample size of a sample mean, 40:
# 25/40 = 0.625
# round(sample.var,1) = 0.6 = round(25/40,1)

# Regarding the distribution of the mean of 40 exponentials. 
# Did the student show how variable it is and compare it to 
# the theoretical variance of the distribution?

# plot the distribution of 1000 sample means (of sample size = 40)

g <- ggplot(data.frame(x = sample.means), aes(x = x))
g <- g + geom_histogram(position="identity", binwidth = 0.1, fill="green", color="black", alpha=0.2, 
                        aes(y= ..density..))
g <- g + stat_function(fun = dnorm, colour = "red", args=list(mean=5))
g <- g + scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9), limits=c(1, 9))
g <- g + scale_y_continuous(breaks=c()) 
g <- g + theme(plot.title = element_text(size=12, face="bold", vjust=2, hjust=0.5))
g <- g + labs(title="Histogram Distribution of 1000 Simuluated Sample Means (size=40)")
g <- g + labs(x = "Simuluated Sample Means", y = "Frequency")
g <- g + geom_vline(xintercept = exp.mean, lwd = 1, col = "blue")
g <- g + geom_vline(xintercept = round(mean.of.sample.means,3), lwd = 1, col = "black")
g <- g + annotate("text", x = 7.3, y = 0.6, col = "black", label = paste("Avg Of Sample Means (",round(mean.of.sample.means,3),")"))
g <- g + annotate("text", x = 7.3, y = 0.57, col = "blue", label = "Theoretical Mean (5)")
g <- g + annotate("text", x = 7.3, y = 0.54, col = "red", label = "Standard Normal Curve")
g



#############################################################################

# Write Histo to file

#############################################################################

# write barplot to png file, in the same folder as this script
png(file="distofmeans.png",width=480,height=480)

g <- ggplot(data.frame(x = sample.means), aes(x = x))
g <- g + geom_histogram(position="identity", binwidth = 0.1, fill="green", color="black", alpha=0.2, 
                        aes(y= ..density..))
g <- g + stat_function(fun = dnorm, colour = "red", args=list(mean=5))
g <- g + scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9), limits=c(1, 9))
g <- g + scale_y_continuous(breaks=c()) 
g <- g + theme(plot.title = element_text(size=12, face="bold", vjust=2, hjust=0.5))
g <- g + labs(title="Histogram Distribution of 1000 Simuluated Sample Means (size=40)")
g <- g + labs(x = "Simuluated Sample Means", y = "Frequency")
g <- g + geom_vline(xintercept = exp.mean, lwd = 1, col = "blue")
g <- g + geom_vline(xintercept = round(mean.of.sample.means,3), lwd = 1, col = "black")
g <- g + annotate("text", x = 7.3, y = 0.6, col = "black", label = paste("Avg Of Sample Means (",round(mean.of.sample.means,3),")"))
g <- g + annotate("text", x = 7.3, y = 0.57, col = "blue", label = "Theoretical Mean (5)")
g <- g + annotate("text", x = 7.3, y = 0.54, col = "red", label = "Standard Normal Curve")
g

dev.off()
