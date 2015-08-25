# load libraries
library(datasets)
library(ggplot2)
library(reshape2)
library(dplyr)

# load ToothGrowth dataset into the workspace:
# The response is the length of odontoblasts (teeth) in each of 10 guinea pigs at each of three dose levels of Vitamin C (0.5, 1, and 2 mg) with each of two delivery methods (orange juice or ascorbic acid).
data(ToothGrowth) 
# See help(ToothGrowth)
# > str(ToothGrowth)
# 'data.frame':	60 obs. of  3 variables:
#   $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
# $ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
# $ dose: num  0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 0.5 ...
# > unique(ToothGrowth$dose)
# [1] 0.5 1.0 2.0

# save the data into a data frame table
tdata <- ToothGrowth

# convert dose to a factor
tdata$dose <- as.factor(ToothGrowth$dose)
# > str(data)
# 'data.frame':	60 obs. of  3 variables:
# $ len : num  4.2 11.5 7.3 5.8 6.4 10 11.2 11.2 5.2 7 ...
# $ supp: Factor w/ 2 levels "OJ","VC": 2 2 2 2 2 2 2 2 2 2 ...
# $ dose: Factor w/ 3 levels "0.5","1","2": 1 1 1 1 1 1 1 1 1 1 ...


# Basic Summary Statistic: Toothgrowth as a Function of Supplement and Dose 

# Did the student perform an exploratory data analysis of at least a single plot 
# or table highlighting basic features of the data?

# Main Effect of Supplement on Tooth Growth: Orange Juice or Ascorbic Acid 
tdata %>% 
  group_by(supp) %>%
  summarize(obs = n(), min = min(len),max = max(len), average = mean(len))

# On average, it appears Orange Juice promotes more growth than Ascorbic Acid 
supp.data <- tdata %>% 
  group_by(supp) %>% 
  summarize(avg = mean(len)) 

# barplot  
barplot(supp.data$avg, xlab="Supplement", ylim = c(0, 25), col = "green", main = "Average Tooth Growth as a Function of Supplement",
        ylab="Average Tooth Growth", names.arg=c("Orange Juice", "Ascorbic Acid"))

# t-test
group.oj <- filter(tdata, supp == "OJ")
group.vc <- filter(tdata, supp == "VC")
t.test(group.oj$len - group.vc$len, paired = FALSE, var.equal = FALSE)

# Main Effect of Dose on Tooth Growth: 0.5, 1, and 2 mg 
tdata %>% 
  group_by(dose) %>%
  summarize(obs = n(), min = min(len),max = max(len), average = mean(len))

# On average, it appears that greater dose levels promote more tooth growth
dose.data <- tdata %>% 
  group_by(dose) %>% 
  summarize(avg = mean(len))

# barplot  
barplot(dose.data$avg, xlab="Does", ylim = c(0, 30), col = "green", main = "Average Tooth Growth as a Function of Dose",
        ylab="Average Tooth Growth (mm)", names.arg=c("0.5mg", "1mg", "2mg"))

# t-test
group.05 <- filter(tdata, dose == 0.5)
group.10 <- filter(tdata, dose == 1)
group.20 <- filter(tdata, dose == 2)
t.test(group.10$len - group.05$len, paired = FALSE, var.equal = FALSE)
t.test(group.20$len - group.05$len, paired = FALSE, var.equal = FALSE)
t.test(group.20$len - group.10$len, paired = FALSE, var.equal = FALSE)

# Interaction of Supplement and Dose on Tooth Growth
tdata %>% 
  group_by(supp,dose) %>%
  summarize(obs = n(), min = min(len),max = max(len), average = mean(len))

# On average, the table data suggests that Orange Juice is more effective than Ascorbic Acid in promoting more growth at for each dose level except for the 2.0mg level 

# boxplot 
boxplot(len~supp*dose, data=tdata, notch=FALSE, 
        col=(c("gold","darkgreen")),
        main="Tooth Growth", xlab="Supplement.Dose")


# perform some relevant confidence intervals and/or tests?
# treat this a paired data, like on p. 84: t.test with paired = T

# t-test
group.oj.05 <- filter(tdata, dose == 0.5 & supp == "OJ")
group.vc.05 <- filter(tdata, dose == 0.5 & supp == "VC")
group.oj.1 <- filter(tdata, dose == 1 & supp == "OJ")
group.vc.1 <- filter(tdata, dose == 1 & supp == "VC")
group.oj.2 <- filter(tdata, dose == 2 & supp == "OJ")
group.vc.2 <- filter(tdata, dose == 2 & supp == "VC")
t.test(group.oj.05$len - group.vc.05$len, paired = FALSE, var.equal = FALSE)
t.test(group.oj.1$len - group.vc.1$len, paired = FALSE, var.equal = FALSE)
t.test(group.oj.2$len - group.vc.2$len, paired = FALSE, var.equal = FALSE)

