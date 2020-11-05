rm(list=ls()) # clear all objects from R memory

library(Publish)

# exact confidence interval
binom.test(x=7,n=43)

# compute "by hand" the confidence interval with the normal approximation
x <- 7
n <- 43
hatp <- x/n
lower <- hatp - 1.96*sqrt(hatp*(1-hatp)/n)
upper <- hatp + 1.96*sqrt(hatp*(1-hatp)/n)
Results <- c(Est=hatp,lower=lower,upper=upper)
Results
round(Results*100,1)

# Dalteparin example data
Tab1 <- cbind(c(14,15,7,5,2),
                    c(9,11,9,5,8)
                    )
rownames(Tab1) <- c("Healed","Improved","Unchanged","Impaired","Amputation")
colnames(Tab1) <- c("Dalteparin","Placebo")
Tab1

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

# Fisher's exact test
fisher.test(Tab2)

# Pearson's Chi-square test
chisq.test(Tab2,correct=FALSE)

# Pearson's Chi-square test with Yates' continuity correction
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
Tab3 <- table(d$SCD,d$BMIgroup)
Tab3
fisher.test(Tab3)

# Example of BMI and age groups
Tab4 <- table(d$ageGroup,d$BMIgroup)
Tab4
fisher.test(Tab4)

# Sample size calculation example
power.prop.test(p1 = 0.5, p2 = 0.25, power=0.8)

# Power calculation example
power.prop.test(n=58, p1 = 0.35, p2 = 0.5)

# Least detectable difference example
power.prop.test(n=75, p1 = 0.8, power=0.8)
