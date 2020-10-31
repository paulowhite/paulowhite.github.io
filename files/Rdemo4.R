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
set.seed(12587)
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

# visualize example data
immer

# rename data (for simplicity)
d <- immer
d$Y <- (d$Y1 + d$Y2)/2 # create outcome as the average of the two years


# stripchart plot: yoeld vs variety
stripchart(d$Y~d$Var,
           vertical=TRUE,method="jitter",
           pch=19,
           xlim=c(0.5,5.5),
           xlab="Variety",ylab="Yield")

# Descriptive statistics: mean, sd and n per variety
tapply(d$Y, d$Var, mean)
tapply(d$Y, d$Var, sd)
table(d$Var)

# Two-way anova model
TwoWayRes <- lm(Y~ Var + Loc, data = d)

# Print model estimates
summary(TwoWayRes)


# Traditional ANOVA: F-test (assuming homogeneity, i.e. same sd in each group)
anova(lm(Y~ Loc, data = d), lm(Y~ Var + Loc, data = d))

# Recommended analysis: all-pairwise comparisons (ajdusted on Location) with multiple testing adjustment
TwoWay.mc <- glht(TwoWayRes, linfct = mcp(Var = "Tukey"))
summary(TwoWay.mc) # print adjusted p-values (min-P method)
confint(TwoWay.mc) # print adjusted 95% confidence intervals (min-P method)


# Default model checking plots
par(mfrow=c(1,2)) # split plot window in two
plot(TwoWayRes, which=c(1,2),ask = FALSE,qqline=FALSE,col="blue")
abline(0,1,col="grey",lty=2)
par(mfrow=c(1,1)) # restore normal plot window

