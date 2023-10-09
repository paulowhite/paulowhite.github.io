#' ---
#' title: "Exercise 1 - solution"
#' author: "Paul Blanche"
#' output: pdf_document
#' fontsize: 12pt
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
rm(list=ls())

#' 
#' ## Question 1
#' 
#' We load the data and have a look a the first lines. 
## -----------------------------------------------------------------------------
load(url("http://paulblanche.com/files/SCD.rda"))
d <- SCD # shorter name, just for convenience
head(d)

#' 
#' We get summary statistics for all variables.
## -----------------------------------------------------------------------------
summary(d)

#' 
#' The variables HCT, Creat and Hb have missing values.
#' 
#' ## Question 2
#' 
## -----------------------------------------------------------------------------
table(d$sex)

#' There are 82 women and 94 men.
#' 
## -----------------------------------------------------------------------------
table(d$SCD)

#' There are 88 subjects with and 88 without Sickle Cell Disease.
#' 
#' ## Question 3
#' 
## ---- fig.height=5------------------------------------------------------------
boxplot(d$Psys~factor(d$SCD,levels=c(0,1),labels=c("no","yes")),
                      xlab="Sickle Cell Disease",
                      ylab="Systolic pressure (mmHg)")

#' 
#' 
#' **Interpretation:** 
#' 
#' * Box:
#'    + middle line: median Q2 (50%)
#'    + bottom: first quartile Q1 (25%)
#'    + top: third quartile Q3 (75%)
#' * Whiskers:  size is 1.5 times the height of the box, but not exceeding minimum and maximum.
#' * Dots: observations beyond whiskers
#' 
#' The overall interpretation seems to make sense: lower values are observed for subjects with SCD (median for SCD patients is approximately Q1 for subjects without SCD).
#' 
#' 
#' **Note:** some **experienced** students might prefer to use an alternative code, using the packages **dplyr** and **ggplot2**. See appendix for such a code.
#' 
#' 
#' ## Question 4
#' First we create the BMI variable and add it to the data.
## -----------------------------------------------------------------------------
d$BMI <- d$weight/(d$height/100)^2

#' Note that we divide the height by 100 because we want the height unit to be meters, not centimeters.
## ---- fig.height=5------------------------------------------------------------
hist(d$BMI,xlab="Body Mass Index",main="")

#' 
#' BMI does not look normally distributed because the distribution does not seem symmetric around the mean/median.
#' 
#' Mean of BMI:
## -----------------------------------------------------------------------------
mean(d$BMI)

#' Sdandard deviation of BMI:
## -----------------------------------------------------------------------------
sd(d$BMI)

#' Minimum of BMI:
## -----------------------------------------------------------------------------
min(d$BMI)

#' Maximum of BMI:
## -----------------------------------------------------------------------------
max(d$BMI)

#' First and third quartiles of BMI (25% and 75%):
## -----------------------------------------------------------------------------
quantile(d$BMI)

#' 
#' or:
## -----------------------------------------------------------------------------
quantile(d$BMI, 0.25)
quantile(d$BMI, 0.75)

#' 
#' 
#' Median of BMI has already been computed (50% above). Alternatively:
## -----------------------------------------------------------------------------
median(d$BMI)

#' Because the variable is far from being normally distributed, it is generally more informative to report the median and the first and third quartiles rather than mean and sd. 
#' We now compute the frequencies for each BMI group.
## -----------------------------------------------------------------------------
d$BMIgroup <- cut(d$BMI,
                  breaks=c(0,18.5,25,30,Inf),
                  include.lowest=TRUE,right=FALSE)
table(d$BMIgroup)

#' We observe 32 subjects "underweight", 112 "normal", 23 "overweight" and 9 "obese". Graphically:
## ---- fig.height=4------------------------------------------------------------
barplot(table(d$BMIgroup),xlab="Body Mass Index",ylab="Number of subjects")

#' 
#' We now do the same, but separately for subjects with and without SCD. However, we now report proportions instead of counts, to facilitate the comparison between the two groups. We make sure that the y-axis is the same for both plots to facilitate the comparison.
## ---- fig.width=10------------------------------------------------------------
par(mfrow=c(1,2))
barplot(table(d$BMIgroup[d$SCD==0])/sum(table(d$BMIgroup[d$SCD==0])),
        main="No SCD",
        ylim=c(0,0.7))
barplot(table(d$BMIgroup[d$SCD==1])/sum(table(d$BMIgroup[d$SCD==1])),
        main="SCD",
        ylim=c(0,0.7))


#' We observe somewhat lower BMI for subjects with SCD (less overweight and less obese subjects).
#' 
#' ## Question 5
#' We compute the mean and sd for subjects with (SCD=1) and without (SCD=0) SCD.
## -----------------------------------------------------------------------------
mean(d$Pdias[d$SCD==1])
mean(d$Pdias[d$SCD==0])
sd(d$Pdias[d$SCD==1])
sd(d$Pdias[d$SCD==0])

#' 
#' 
#' 
#' Next, we use the t.test function to compute 95% confidence interval for the mean diastolic pressure in the two populations of subjects with and without SCD.
## -----------------------------------------------------------------------------
t.test(d$Pdias[d$SCD==1])
t.test(d$Pdias[d$SCD==0])

#' The confidence intervals are [60.3;63.3] and [67.8;72.4] for subjects with and without SCD, respectively. They do not overlap, hence we can conclude to a significant difference in mean diastolic pressure between the two groups (at the usual 5% level for the type-I error control). 
#' 
#' We now compute the 95% **prediction** interval for the diastolic pressure in the two populations of subjects with and without SCD.
## -----------------------------------------------------------------------------
predict(lm(d$Pdias[d$SCD==0]~1),interval="prediction")[1,]
predict(lm(d$Pdias[d$SCD==1]~1),interval="prediction")[1,]

#' Under the assumption that the diastolic pressure is (approximately) normally distributed in each population (which should be checked using e.g. a QQplot!) then we can expect that (close to) 95% of the subjects without SCD (and similar to that of the study) have a diastolic pressure between 48.4 and 91.7. For subjects with SCD, between 47.9 and 75.7.
#' 
#' 
#' ## Question 6
#' 
## -----------------------------------------------------------------------------
t.test(d$Psys[d$SCD==1])
t.test(d$Psys[d$SCD==0])

#' The confidence intervals are [115.9;120.8] and [124.1;129.7]. They do not overlap, hence a significant difference.
## -----------------------------------------------------------------------------
t.test(d$Psys[d$SCD==1 & d$sex==2])
t.test(d$Psys[d$SCD==0 & d$sex==2])

#' The confidence intervals are [113.9;119.9] and [119.9;127.2]. They do overlap (although very little), hence **we cannot rigorously say whether there is a significant difference**. We need to perform a two-sample t-test to rigorously conclude (see course Day 2).
#' 
#' We now produce a dotplot to display the individual observations.
## ---- fig.width=5-------------------------------------------------------------
stripchart(d$Psys[d$sex==2]~factor(d$SCD[d$sex==2],
           levels=c(0,1),labels=c("No SCD","SCD")),
           vertical=TRUE,
           method="jitter",
           xlab="",
           ylab="Systolic blood pressure (mmHg)",
           pch=19,
           col=c("forestgreen","red"))

#' 
#' Now we produce QQplots for each group.
## ---- fig.width=10------------------------------------------------------------
par(mfrow=c(1,2))
qqnorm(scale(d$Psys[d$SCD==1 & d$sex==2]),main="SCD")
abline(0,1,col="red",lty=2,lwd=3)
qqnorm(scale(d$Psys[d$SCD==0 & d$sex==2]),main="No SCD")
abline(0,1,col="red",lty=2,lwd=3)

#' 
#' We count the number of women with and without SCD. 
## -----------------------------------------------------------------------------
table(d$SCD[d$sex==2])

#' 
#' Because the QQplot looks good (observations close to the diagonal, no major deviation) and because the sample size of each group is "large enough" (n=47) we can trust the computation of the 95% confidence intervals. Hence, we can interpret the results quite confidently, without limitations about the appropriateness of the statistical method.
#' 
#' ## Question 7
#' We first create (and add to the data) a new variable "MAP" to define the Mean Arterial
#' Pressure.
## -----------------------------------------------------------------------------
d$MAP <- d$Pdias + (1/3)*(d$Psys-d$Pdias)

#' 
#' We make a histogram and a QQplot to visually assess whether the distribution of the Mean
#' Arterial Pressure looks normally distributed, among subjects without SCD
## ---- fig.width=10------------------------------------------------------------
par(mfrow=c(1,2))
hist(d$MAP[d$SCD==0])
qqnorm(scale(d$MAP[d$SCD==0]))
abline(0,1,col="red",lty=2,lwd=3)

#' It looks "fine", but it is, as always, difficult to say without "references" to compare to. Small sample random variation is difficult to understand. Hence the "Wally plot" can help.
## ---- fig.width=7,fig.height=7------------------------------------------------
library(MESS)
lm0 <- lm(d$MAP[d$SCD==0]~1)
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...) ; abline(a=0, b=1) } 
wallyplot(lm0, FUN=qqnorm.wally, main="",hide=FALSE,col="blue")

#' The "Wally plot" helps us to conclude that the QQplot of our data does not look bad: it does not look very different from the QQplot of random samples (of same sample size) drawn from a normal distribution.
## ---- fig.width=7,fig.height=7------------------------------------------------
predict(lm(d$MAP[d$SCD==0]~1),interval="prediction")[1,]

#' We estimate the "normal range" of MAP for patients without SCD as [66.9;111.1], which is very close to what is reported on wikipedia.
#' 
#' ## Additional challenge
#' 
## ---- fig.width=6,fig.height=6------------------------------------------------
# First, create a binary variable for hypotension
d$Hypo <- cut(d$Pdias,breaks=c(0,60,Inf),
              include.lowest=TRUE,right=FALSE)
# create two data sets, with subjects with and without hypotension
dHypo <- d[d$Hypo=="[0,60)",]
dNoHypo <- d[d$Hypo=="[60,Inf]",]
# set seed for reproducibility (jitter)
set.seed(123)
# First plot observations with hypotension
stripchart(dHypo$Pdias~dHypo$SCD,
           vertical=TRUE,method="jitter",
           col="red",
           pch=19,
           ylim=c(min(d$Pdias),max(d$Pdias)),
           xlab="Sickle Cell Disease",
           ylab="Diastolic pressure (mmHg)",
           axes=FALSE)
# add axes
axis(1,at=1:2,c("Absent","Present"))
axis(2,las=2)
# add horizontal line at the threshold defining hypotension
abline(h=60,lwd=2,lty=2)
# Plot observations without hypotension on top
stripchart(dNoHypo$Pdias~dNoHypo$SCD,
           vertical=TRUE,
           method="jitter",
           col="forestgreen",pch=19,add=TRUE)
# Add a legend
legend("topright",
       fill=c("forestgreen","red"),
       legend=c("No Hypotension","Hypotension"),
       ncol=2)

#' 
#' **Note:** some **experienced** students might prefer to use an alternative code, using the packages **dplyr** and **ggplot2**. See appendix for such a code.
#' 
#' 
#' # Appendix
#' 
#' ## Alternative code for Question 3
#' 
## ---- message = FALSE---------------------------------------------------------
library(dplyr)
library(ggplot2)

d %>% 
        mutate(SCD = factor(SCD, levels=c(0,1), labels=c("no","yes"))) %>% 
        ggplot(., aes(y = Psys, x = SCD)) +
                geom_boxplot() + 
                xlab("Sickle Cell Disease") +
                ylab("Diastolic pressure (mmHg)")


#' 
#' ## Alternative code for the additional challenge
#' 
## -----------------------------------------------------------------------------
set.seed(123)

d %>% 
mutate(Hypo = case_when(Pdias < 60 ~ "Hypotension",
                        Pdias >= 60 ~ "no Hypotension"),
       SCD = case_when(SCD == 0 ~ "Absent",
                       SCD == 1 ~ "Present")) %>% 
ggplot(., aes(x=SCD, y=Pdias, color = Hypo)) + 
        geom_jitter(width = 0.15, height = 0) + #only horizontal jitter
        xlab("Sickle Cell Disease") + #x-axis label
        ylab("Diastolic pressure (mmHg)") + #y-axis label
        scale_color_manual(values = c("red", "forestgreen")) + #set colour
        theme_classic() + #remove grid + gray background 
        theme(legend.title = element_blank()) + #remove legend title
        geom_hline(yintercept = 60, linetype = "dashed") #horizontal line

#' 
#' 
#' 
#' 
