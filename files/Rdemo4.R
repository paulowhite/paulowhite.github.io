rm(list=ls())

library(DoseFinding)
library(multcomp)
library(sandwich)


#---- First part, one-way ANOVA --------------

# Load example data 
data(IBScovars)
# Keep only observations from women for doses 0-3
d <- IBScovars[which(IBScovars$gender==2 & IBScovars$dose!=4 ),]
# summary of the data
summary(d)

# stripchart plot: response vs dose
set.seed(12587) # fix the random seed, just to make the plot fully reproducible
thecols <- c("red","blue","forestgreen","orange")
stripchart(d$resp~d$dose,
           vertical=TRUE,method="jitter",
           pch=c(15:19),col=thecols,
           xlab="Dose",ylab="Response")

# Descriptive statistics: mean, sd and n per dose
tapply(d$res, d$dose, mean)
tapply(d$res, d$dose, sd)
table(d$dose)


# Whelch's t-test to compare dose 0 and dose 1
t.test(d$resp[d$dose==0],d$resp[d$dose==1])

# QQplots for observations from Dose 0 and 1
qqnorm(scale(d$resp[d$dose==0]),main="",col=thecols[1],pch=19)
abline(0,1,col="black",lty=2,lwd=3)
qqnorm(scale(d$resp[d$dose==1]),main="",col=thecols[2],pch=19)
abline(0,1,col="black",lty=2,lwd=3)

# Recommended analysis: all-pairwise comparisons with multiple testing adjustment
d$dosefact <- factor(d$dose)       # make a factor variable
fitlm <- lm(resp~dosefact, data=d) # fit a linear model (here a one-way ANOVA) 
glht.all <- glht(fitlm, mcp(dosefact="Tukey"), vcov=vcovHC) # all-pairwise comparisons in heteroscedastic model
summary(glht.all) # print adjusted p-values (min-P method)
confint(glht.all) # print adjusted 95% confidence intervals (min-P method)


# Recommended analysis: many-to-one comparisons (Placebo vs others)
glht.Dunnett <- glht(fitlm, mcp(dosefact="Dunnett"), vcov=vcovHC)
summary(glht.Dunnett) # print adjusted p-values (min-P method)
confint(glht.Dunnett) # print adjusted 95% confidence intervals (min-P method)

# Traditional ANOVA: F-test (homogeneity, i.e. same sd in each group)
anova(fitlm) # shortest code
oneway.test(resp~dosefact, data=d, var.equal =TRUE) # An equivalent code (for comparison with below)

# Now F-test NOT assuming the same sd in each group
oneway.test(resp~dosefact, data=d, var.equal=FALSE) # Note:  var.equal=FALSE is the default option

# Default model checking plots
par(mfrow=c(1,2)) # split plot window in two
plot(fitlm, which=c(1,2),ask = FALSE,qqline=FALSE,col="blue")
abline(0,1,col="grey",lty=2)
par(mfrow=c(1,1)) # restore normal plot window

# Visualize the usual software parametrization
summary(fitlm)

# Same with Dose 1 as reference
d$dosefact <- relevel(d$dosefact,ref="1") # change the reference level in the factor variable
fitlm <- lm(resp~dosefact, data=d) # Re-fit the linear model
summary(fitlm) # print summary of re-fitted model


#---- Second part, two-way ANOVA --------------

rm(list=ls()) # clear all objects from R memory

library(multcomp)
library(sandwich)
library(HSAUR2)

# visualize first lines of the example data
head(weightgain)

# boxplot 
boxplot(weightgain~source,
        data=weightgain,
        outline=FALSE, # do not plot outliers, because we will add all dots later
        col="white",
        border=c("red","forestgreen"),        
        xlab="Source of protein",
        ylab="Weight gain (g)",ylim=c(50,120),xlim=c(0.5,2.5))
# add the dotplot on top
stripchart(weightgain~source,
           data=weightgain,
           vertical=TRUE,
           method="jitter",
           col=c("red","forestgreen"),
           pch=19,
           axes=TRUE,add=TRUE,ylim=c(50,120))

# Descriptive statistics: mean, sd and n per source of protein
tapply(weightgain$weightgain, weightgain$source, mean)
tapply(weightgain$weightgain, weightgain$source, sd)
table(weightgain$source)

# Two-way anova model (to ajdust on the amount of protein, when comparing source of proteins)
TwoWayRes <- lm(weightgain~type+source,data=weightgain)

# Print model estimates
summary(TwoWayRes)

#----
# For illustrated the comparison of  3 groups instead of 2,
# we now artificially create additional data: 20 more observations
# from rats fed with Fish, 10 receive a Low amount of protein, 10 a High amount.
weightgain2 <- rbind(weightgain,
                     data.frame(source=factor(rep("Fish",20),levels=c("Beef","Cereal","Fish")),
                                type=factor(rep(c("Low","High"),each=10),levels=c("High","Low")),
                                weightgain=c(79, 87, 72, 69, 53, 76, 59, 92, 81, 72,
                                             92, 71, 102, 62, 70, 90, 80, 95, 84, 105)
                                ))
#----


# Traditional ANOVA: F-test (assuming homogeneity, i.e. same sd in each group)
Full.lm <- lm(weightgain~source+type, data=weightgain2) # "full" model (same as TwoWayRes, but now fitted with new data)
Cons.lm <- lm(weightgain~type, data=weightgain2)        # "constrained" model 
anova(Cons.lm,Full.lm)                                  # F-test (compares the 2 models)


# Recommended analysis: all-pairwise comparisons of source of protein,
# ajdusted on amount of protein, with multiple testing adjustment.
TwoWay.mc <- glht(Full.lm, linfct = mcp(source = "Tukey"))
summary(TwoWay.mc) # print adjusted p-values (min-P method)
confint(TwoWay.mc) # print adjusted 95% confidence intervals (min-P method)


# Default model checking plots
par(mfrow=c(1,2)) # split plot window in two
plot(Full.lm, which=c(1,2),ask = FALSE,qqline=FALSE,col="blue")
abline(0,1,col="grey",lty=2)
par(mfrow=c(1,1)) # restore normal plot window
