rm(list=ls()) # clear all objects from R memory

library(Publish)


load(url("http://paulblanche.com/files/dalteparin.rda")) 
head(dalteparin) 

# Cross tabulate Treatment and Outcome
Tab1 <- table(dalteparin$outcome,dalteparin$trt)
Tab1

# Same table with a better  ordering of the levels (from best to worst)
Tab1 <- Tab1[c("Healed", "Improved", "Unchanged","Impaired", "Amputation"),]
Tab1

# How to make the same table "from scratch"
Tab1 <- cbind(c(14,15,7,5,2),
              c(9,11,9,5,8))
rownames(Tab1) <- c("Healed","Improved","Unchanged","Impaired","Amputation")
colnames(Tab1) <- c("Dalteparin","Placebo")
Tab1


# create a binary outcome from the categorical outcome
dalteparin$Y <- factor(dalteparin$outcome,
                       levels=c("Healed", "Improved", "Unchanged","Impaired", "Amputation"),
                       labels=c("better", "better", "worse","worse", "worse"))
# 2 by 2 table
TabMain <- table(Treatment=dalteparin$trt,Outcome=dalteparin$Y)
TabMain

# Same with new order of rows and columns
TabMain <- TabMain[2:1,2:1]
TabMain

# exact confidence interval
binom.test(x=7,n=43)

# compute "by hand" the confidence interval with the normal approximation
# FOR ACADEMIC PURPOSE ONLY! (see below function table2x2 below for getting the same result more easily)
x <- 7
n <- 43
hatp <- x/n
lower <- hatp - 1.96*sqrt(hatp*(1-hatp)/n)
upper <- hatp + 1.96*sqrt(hatp*(1-hatp)/n)
Results <- c(Est=hatp,lower=lower,upper=upper)
Results
round(Results*100,1)

# Transform to proportion (per treatment group)
prop.table(Tab1,margin=2)
round(100*prop.table(Tab1,margin=2)) # same but in percentages and rounded

# barplot of counts
t(Tab1) # print the "transposed table"
barplot(t(Tab1),
        beside=TRUE,
        col=c("black","white"),
        ylab="Number of observations",
        xlab="Outcome",
        legend=TRUE
        )

# barplot of percentages
barplot(t(100*prop.table(Tab1,margin=2)),
        beside=TRUE,
        col=c("black","white"),
        ylab="Proportions (%)",
        xlab="Outcome",
        legend=TRUE
        )

# Example data for group comparison
Tab2 <- rbind(c(22,42-22),c(14,43-14))
rownames(Tab2) <- c("Placebo","Dalteparin")
colnames(Tab2) <- c("Worse","Better")

# Compute risk difference, RR  and OR, together with 95% CI,
table2x2(Tab2,stats = c("table", "rd", "rr", "or"))

# Fisher's exact test; always works (default choice!)
fisher.test(Tab2)

# Pearson's Chi-square test; fine with large samples
chisq.test(Tab2,correct=FALSE)

# Pearson's Chi-square test with Yates' continuity correction; NO LONGER USEFUL...
chisq.test(Tab2,correct=TRUE)


# Load data for larger table examples
load(url("http://paulblanche.com/files/SCD.rda"))
d <- SCD # shorter name, just for convenience

# Create categorical variables BMI
d$BMI <- d$weight/(d$height/100)^2
d$BMIgroup <- cut(d$BMI,
                  breaks=c(0,18.5,25,30,Inf),
                  labels=c("underweight","normal","overweight","obese"),
                  include.lowest=TRUE,right=FALSE)
table(d$BMIgroup)

# Create categorical variables Age
d$ageGroup <- cut(d$age,
                  breaks=c(16,25,30,67),
                  include.lowest=TRUE,right=FALSE)
table(d$ageGroup,useNA="always")

# Example of BMI and SCD
Tab3 <- table(SCD=d$SCD,BMI=d$BMIgroup)
Tab3
fisher.test(Tab3)

# Example of BMI and age groups
Tab4 <- table(Age=d$ageGroup,BMI=d$BMIgroup)
Tab4
fisher.test(Tab4)

#------ Power and sample size calculation -------

# Sample size calculation example
power.prop.test(p1 = 0.5, p2 = 0.25, power=0.8)

# Power calculation example
power.prop.test(n=58, p1 = 0.35, p2 = 0.5)

# Least detectable difference example
power.prop.test(n=75, p1 = 0.8, power=0.8)


#------ Analysis of paired binary data ----------------------

library(exact2x2)                     # load a useful package
tab <- rbind(c(1,19),c(2,2))          # 2 by 2 table
mcnemar.exact(tab)                    # exact McNemar test
binom.test(x=sum(tab[,2]),n=sum(tab)) # sensitivity for PCR-test (95%-CI)
binom.test(x=sum(tab[2,]),n=sum(tab)) # sensitivity for BC-test  (95%-CI)
