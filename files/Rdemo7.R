rm(list=ls()) # clear all objects from R memory

load(url("http://paulblanche.com/files/vitaminD.rda")) 
head(vitaminD)

#---- Data management----

# First, format/create some variables
vitaminD <- vitaminD[vitaminD$bmi >= 18.5,]      # remove underweight
vitaminD$Country <- factor(vitaminD$country,     # new variable with better labels
                           levels=c(1,2,4,6),
                           labels=c("Denmark","Finland","Ireland","Poland"))
vitaminD$bmigroup <- factor(as.numeric(vitaminD$bmi > 25)) # BMI group: 1 if BMI > 25, 0 otherwise
vitaminD$bmi5 <- vitaminD$bmi/5 # BMI by unit of 5
vitaminD$sunexp <- factor(vitaminD$sunexp,       # new variable with better labels
                          levels=c(1,2,3),
                          c("avoid","sometimes","prefer"))
# Second, define some subsets
# subset of Irish women 
irlwomen <- vitaminD[which(vitaminD$Country == "Ireland" & vitaminD$category == 2),]
# subset of Irish and Polish women 
irlpolwomen <- vitaminD[which(vitaminD$Country %in% c("Ireland","Poland") & vitaminD$category == 2),] 
# subset of all women 
dwomen <- vitaminD[which(vitaminD$category == 2),]
# subset of Irish, Finnish and Polish women 
IrFinPo <- vitaminD[vitaminD$Country%in%c("Finland","Ireland","Poland"),] 
IrFinPo$Country <- droplevels(IrFinPo$Country) # delete unused level

#----- Table 1 making---
library(Publish)
Tab1ex <- univariateTable(Country~age+Q(bmi)+Q(vitdintake)+sunexp,
                          data=IrFinPo,
                          compare.groups = FALSE,
                          show.totals = FALSE)
Tab1ex


#---- Analyses--------

# Frist model
lm1 <- lm(log10(vitd)~bmigroup,data=irlwomen)
summary(lm1)
confint(lm1)
publish(lm1)
# compute mean per group
tapply(log10(irlwomen$vitd), irlwomen$bmigroup, mean)
# t-test assuming equal variances
t.test(log10(irlwomen$vitd) ~ irlwomen$bmigroup,var.equal=TRUE) 
# compare mean and median per group
rbind(tapply(log10(irlwomen$vitd), irlwomen$bmigroup, mean),
      tapply(log10(irlwomen$vitd), irlwomen$bmigroup, median))

# Second model
lm2 <- lm(log10(vitd) ~  Country + bmigroup, data = irlpolwomen)
summary(lm2)
confint(lm2)
publish(lm2)

# Third model
lm3 <- lm(log10(vitd) ~ bmi5 + Country, data = irlpolwomen)
summary(lm3)
confint(lm3)
publish(lm3)

# Fourth model
lm4 <- lm(log10(vitd) ~ bmi5 + Country, data = dwomen)
summary(lm4)
publish(lm4)
# F-test for comparing countries adjusting on BMI
anova(lm(log10(vitd) ~ bmi5, data = dwomen),
      lm(log10(vitd) ~ bmi5 + Country, data = dwomen))
# All pairwise comparisons between countries,adjusting on BMI and for multiple testing  
library(multcomp)
Res4 <- glht(lm4, mcp(Country="Tukey")) # all-pairwise comparisons
summary(Res4) # print adjusted p-values (min-P method)
confint(Res4) # print adjusted 95% confidence intervals (min-P method)

# Fifth model: using interaction
lm5 <- lm(log10(vitd) ~  Country * bmi5, data = irlpolwomen)
summary(lm5)
publish(lm5)


#---- Model checking plots: ANCOVA example -------

par(mfrow=c(1,2)) # split the plot window in two
plot(lm3, which=c(1,2),ask = FALSE,qqline=FALSE,col="blue")
abline(0,1,col="grey",lty=2)
par(mfrow=c(1,1)) # back to usual plot window


# Wally plot
library(MESS)
permsr <- function(n) {sample(x=residuals(lm3),size=n,replace=FALSE)}
permsrplot <- function(x, y, ...) {
    plot(x, y,ylab="Residuals",xlab="Fitted values",  ...) ;
    abline(h=0,lty=3,col="grey")
    lines(lowess(x, y), lty=1,col="red")}
# Now create the wally residual plot based on the above technical details
wallyplot(predict(lm3),residuals(lm3), FUN=permsrplot,simulateFunction=permsr,hide=FALSE,col="blue")
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...) ; abline(a=0, b=1) } #  Define function to plot a QQplot with an identity line

# Now create the wally QQplot based on the above technical details
wallyplot(lm3, FUN=qqnorm.wally, main="",hide=FALSE,col="blue")


#---- Graphical representations (ANCOVA example)------

# plot observations
plot(log10(vitd) ~ bmi,
     data = irlpolwomen,
     col= ifelse(irlpolwomen$country == 4,"blue","red"),
     pch=19,
     xlab = "BMI",
     ylab = expression(paste(log[10],"(vitamin D)"))
     )
# We create two dataset containing the observations of covariates for which we want the
# corresponding estimated mean outcome (one for each country)
ireland <- expand.grid(bmi=irlpolwomen$bmi[which(irlpolwomen$Country == "Ireland")],
                       Country=c("Ireland"))
ireland$bmi5 <- ireland$bmi/5
poland <- expand.grid(bmi=irlpolwomen$bmi[which(irlpolwomen$Country == "Poland")],
                      Country=c("Poland"))
poland$bmi5 <- poland$bmi/5
# add the estimated means
lines(ireland$bmi, predict(lm3, ireland),col="blue",lwd=2)
lines(poland$bmi, predict(lm3, poland),col="red",lwd=2)
legend("topright",pch=19,col=c("blue","red"),legend=c("Ireland","Poland"))


## We now add 95% CI and prediction intervals to the plot (for ireland)
# First, order rows by bmi
ireland <- ireland[order(ireland$bmi),]
# Second, compute 95% CI and prediction intervals
predIpi <- predict(lm3, ireland,interval='pred')
predIci <- predict(lm3, ireland,interval='conf')
# Third add them to the plot
lines(ireland$bmi,predIci[,"upr"] ,col="blue",lwd=1) # upper ci
lines(ireland$bmi,predIci[,"lwr"] ,col="blue",lwd=1) # lower ci
lines(ireland$bmi,predIpi[,"upr"] ,col="blue",lwd=2,lty=2) # upper pi
lines(ireland$bmi,predIpi[,"lwr"] ,col="blue",lwd=2,lty=2) # lower pi
