rm(list=ls()) # clear all objects from R memory

# load relevant packages
library(DoseFinding) # for loading data example 1
library(HistData)    # for loading data example 2
library(timereg)     # for loading data example 3
library(MESS)        # for wally plots 

# Load first data example (Data on doses)
data(migraine)

# visualize the first line of the data
head(migraine)

# make a barplot (of counts)
barplot(migraine$ntrt,
        col=1:nrow(migraine),
        names=migraine$dose,
        xlab="Dose (mg)",
        ylab="n. of patients")

# make a barplot (of frequencies)
barplot(100*migraine$ntrt/sum(migraine$ntrt),
        col=1:nrow(migraine),
        names=migraine$dose,
        xlab="Dose (mg)",
        ylab="Patients (%)" )


# Load Second data example (Data on height)
data(Galton)
# visualize the first line of the data
head(Galton)
# print summary of the data
summary(Galton)

# Update data to change unit from inches to meters
Galton$child <- Galton$child*0.0254
Galton$parent <- Galton$parent*0.0254
child2 <- jitter(Galton$child) # add a tiny bit of "measurements error" (just for the present example)

# Histogram: version with y-axis showing the density
hist(child2,main="",xlab="height (m)",col="grey",freq=FALSE)

# Histogram: version with y-axis showing the frequency
hist(child2,main="",xlab="height (m)",col="grey")

# Load third data example (Data on tumor thickness)
data(melanoma)

# Histogram: version with y-axis showing the frequency
hist(melanoma$thick,xlab="Tumor thickness (1/100 mm)",main="",col="grey")

# computation of mean and standard deviation
mean(melanoma$thick)
sd(melanoma$thick)

# Same with log of tumor  thickness instead of tumor thickness
hist(log(melanoma$thick),xlab="Log of tumor thickness in 1/100 mm",main="",col="grey")
mean(log(melanoma$thick))
sd(log(melanoma$thick))

# normal range of log-thickness (rounded, 2 digits)
round(c(mean(log(melanoma$thick)) - 2*sd(log(melanoma$thick)),
        mean(log(melanoma$thick)) + 2*sd(log(melanoma$thick))),2)

# normal range back transformed
exp(round(c(mean(log(melanoma$thick)) - 2*sd(log(melanoma$thick)),
            mean(log(melanoma$thick)) + 2*sd(log(melanoma$thick))),2))

# quantiles
quantile(melanoma$thick) # min, Q1, Q2=median, Q3, max
median(melanoma$thick)   # median only
quantile(melanoma$thick,probs=0.25) # we can specify any quantile we want (here 25%).


# boxplot
boxplot(melanoma$thick~factor(melanoma$ulc,levels=c(0,1),labels=c("no","yes")),
        xlab="Ulceration",
        ylab="Tumor thickness (1/100 mm)")

# QQplot
qqnorm(scale(melanoma$thick),main="QQplot of tumor thickness:\n raw data")
abline(0,1,col="red",lty=2,lwd=3)
# Note: scale does substract the mean and devide by sd, i.e. it standardizes.

# Similar QQplot for log-thickness
qqnorm(scale(log(melanoma$thick)),main="QQplot of log-tumor thickness:\n log-transformed data")
abline(0,1,col="red",lty=2,lwd=3)

# Wally plot
lm0 <- lm(log(melanoma$thick)~1)
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...) ; abline(a=0, b=1) } #  Define function to plot a QQplot with an identity line
wallyplot(lm0, FUN=qqnorm.wally, main="",hide=FALSE,col="blue")


# Load fourth data example (Data on cell length and doses)
data(ToothGrowth)

# print summary of the data
summary(ToothGrowth)
# Make dose a factor variable (i.e. do not interpret 0.5, 1 and 2 as numeric values but as levels)
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
# print summary of the updated data (to check that the dose variable has been correctly updated)
summary(ToothGrowth)
# Create a new data set with observations from only one specific supplement type ("OJ")
d <- ToothGrowth[ToothGrowth$supp=="OJ",]

# stripchart
stripchart(d$len~d$dose,
           vertical=TRUE,method="jitter",
           pch=c(15,17,19),col=c("red","blue","forestgreen"),
           xlab="Dose (mg/day )",ylab="Cell length (microns)")


# confidence interval for a prevalence when observing 12 cases out of 1071 
binom.test(x=12,n=1071)

# quantile of the t distribution, for specific degrees of freedom
qt(p=0.975,df=9) # 97.5% with 9 degrees of freedom


# 95% confidence interval for population mean (Tumor thickness data)
predict(lm(log(melanoma$thick)~1),interval="confidence")[1,]      # of log-thickness of the tumor
t.test(log(melanoma$thick)) # just a computational trick to compute the same
exp(predict(lm(log(melanoma$thick)~1),interval="confidence")[1,]) # back-transformed
# same computation "by hand"
xbar <- mean(log(melanoma$thick)) # compute mean
s <- sd(log(melanoma$thick))      # compute sd
n <- length(log(melanoma$thick))  # sample size n
t <- qt(0.975,df=n-1)             # t: appropriate quantile of the t distribution
c(xbar,xbar - t*s/sqrt(n), xbar + t*s/sqrt(n))      # mean with 95% confidence interval (log-thickness)
exp(c(xbar,xbar - t*s/sqrt(n), xbar + t*s/sqrt(n))) # back-transformed


# 95% prediction interval
predict(lm(log(melanoma$thick)~1),interval="prediction")[1,] # log-thickness
exp(c(xbar - t*s*sqrt(1+1/n), xbar + t*s*sqrt(1+1/n)))       # back-transformed
c(xbar - t*s*sqrt(1+1/n), xbar + t*s*sqrt(1+1/n))  # log-thickness, "by hand"
