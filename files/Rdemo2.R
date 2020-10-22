rm(list=ls()) # clear all objects from R memory

# load relevant packages
library(nlme) # for loading data example 1 and 2
library(coin) # for loading data example 3 and to use the wilcox_test() function

# Example data 1: new dataset with only observations at 12 weeks after  
# calving from  cows fed with either lupins or barley.
d <- Milk[which(Milk$Time==12 & Milk$Diet %in% c("barley","lupins")),c("protein","Cow","Diet")]
d$Diet <- droplevels(d$Diet) # technicality: forget levels no longer present in the data.

# print a summary of the data
summary(d)

# mean, sd and n per Diet (rounded, 2 digits)
round(tapply(d$protein,d$Diet,mean),2)
round(tapply(d$protein,d$Diet,sd),2)
table(d$Diet)

# Save the observations for each Diet in two vectors
Prot.Barley <- d$protein[d$Diet=="barley"]
Prot.Lupins <- d$protein[d$Diet=="lupins"]

# trick to compute 95% of the population mean for each diet
t.test(Prot.Barley)
t.test(Prot.Lupins)

# sample mean difference
mean(Prot.Barley)-mean(Prot.Lupins) 

# t-test to compare the (population) mean in the two groups,
# with 95% confidence interval of the difference and corresponding p-value.
t.test(Prot.Barley,Prot.Lupins)

# Histogram of the observations per Diet
MyCols <- c(rgb(1,0,0,alpha=0.5),rgb(0,1,0,alpha=0.5)) # define two (transparent) colors
# Plot the first histogram (Diet=Barley)
hist(Prot.Barley,
     col=MyCols[1],
     xlim=c(2.5,4.25),
     ylim=c(0,10),
     main="",
     xlab="Protein level (%)",
     axes=FALSE)
# Add the second (Diet=Lupins)
hist(Prot.Lupins,col=MyCols[2],add=TRUE)
# Add a legend
legend("right",
       fill=MyCols,
       legend=c("Barley","Lupin"),
       bty="n",
       border="white")
# Add x-axis and y-axis
axis(1)
axis(2,las=2)

# NOT OFTEN RECOMMENDED: "classical" Student's test assuming equal standard deviation in both groups
t.test(Prot.Barley,Prot.Lupins,var.equal=TRUE)


# Example Data 2: new dataset with only observations of the difference
## in protein level of the milk at 1 and 12 weeks after calving from  cows fed with  barley.
d <- data.frame(Cow=Milk[which(Milk$Diet=="barley" & Milk$Time==1),"Cow"],
                Diff=Milk[which(Milk$Diet=="barley" & Milk$Time==1),"protein"]-Milk[which(Milk$Diet=="barley" & Milk$Time==12),"protein"])
# visualize the "head" of the dataset
head(d)

# One-sample t-test (p-value & 95% CI of the mean)
t.test(d$Diff)

# mean, sd and number of observations n
mean(d$Diff)
sd(d$Diff)
nrow(d)

# For academic purpose only: computation of 95% CI of the mean by hand (rounded, 2 digits) 
round(c(mean(d$Diff) - qt(0.975,df=nrow(d)-1)*sd(d$Diff)/sqrt(nrow(d)),
        mean(d$Diff) + qt(0.975,df=nrow(d)-1)*sd(d$Diff)/sqrt(nrow(d))),2)


# Sample size calculation when assuming sd in both groups equal to 1,
# a difference in mean equal to 0.5 and 80% power.
power.t.test(power = .80, delta = 0.5) # n=64


# With a sample size of n=40 (i.e. 20 per group), what
# is the smallest difference I can hope to show with 80% power if
# the the sd in both groups equals to 1?
power.t.test(n=20,sd=1,power=0.80) # delta=0.91

# What is the power to detect a mean difference of 2 with n=74 subjects,
# i.e. 37 per group,  assuming that the sd in both groups equal to 4?
power.t.test(sd=4,delta=2,n=37) # power=56%


# Example of 6 (unadjusted)p-values
punadj <- c(0.005,0.009,0.1,0.15,0.3,0.6)
p.adjust(punadj,method="bonferroni") # corresponding Bonferroni adjusted p-values
p.adjust(punadj,method="holm")       # corresponding Bonferroni-Holm adjusted p-values

# Example of computation of a Bonferroni adjusted (i.e. simultaneous)
# 95% confidence interval if K=6 coinfidence intervals and tests are computed.
K <- 6
SignLevel <- 0.05/K        # Adjusted significance level
ConfLevel <- 1 - SignLevel # adjusted confidence level
t.test(Prot.Barley,Prot.Lupins,conf.level = 0.95)  # non adjusted, with conf.level=0.95 (default)
t.test(Prot.Barley,Prot.Lupins,conf.level = ConfLevel) # adjusted

# To control the FDR for "exploratory" studies using the method of Benjamini-Hochberg (1995).
p.adjust(punadj,method="fdr")

# Example data 3: gene expression
data(alpha) # load  data from coin package
alpha <- alpha[alpha$alength!="intermediate",] # we do not keep observation with intermediate allele length
alpha$alength <- droplevels(alpha$alength)  # technicality: forget levels no longer present in the data.

# summary of the data
summary(alpha)

# stripchart to visually compare the data from the two groups
set.seed(123)
stripchart(alpha$elevel~alpha$alength,
           vertical=TRUE,method="jitter",
           pch=c(19,19),col=c("red","blue"),
           xlab="Allele length",ylab="Expression levels of alpha synuclein mRNA",
           axes=FALSE)
axis(1,at=1:2,c("short (n=24)","long (n=15)"))
axis(2,las=TRUE)

# Exact Wilcoxon Test to compare the distribution of the observations in the two groups.
wilcox_test(elevel~alength,data=alpha, distribution='exact')

