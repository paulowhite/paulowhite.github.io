rm(list=ls()) # clear all objects from R memory

library(MESS)

# Load the data example 1 via internet
load(url("http://paulblanche.com/files/tetrahymena.rda"))
# Alternatively, you have first saved "tetrahymena.rda" in a folder in your own laptop
# and you can now load the data by indicating where the data are located,
# by replacing "your/path/to/" in the command line below by the appropriate path to the data.
# load("~/your/path/to/tetrahymena.rda")

# Print list of object in memory
ls() # here you should see the object "th", which contains the data.

# print a summary of the data
summary(th)

# Create two new variables: the log2 transformed concentration and diameter.
# This makes sense for our case study, but, in general, do not systematically
# log-transform without a good reason to do so!
th$log2conc <- log2(th$concentration)
th$log2diam <- log2(th$diameter)

# print a summary of the updated data
summary(th)

# Scatter plot of the data
plot(x=th$log2conc,
     y=th$log2diam,
     ylab='log2(Diameter)',
     xlab='log2(Concentration)',col="red",pch=19)

# Add regression line
abline(lm(log2diam~log2conc,data=th),col="blue",lwd=3,lty=1)

# Fit a linear regression model
lm1 <- lm(log2diam~log2conc,data=th)
# Print the summary of the fit
summary(lm1)

# estimates and confidence intervals... on double logarithmic-scale
cbind(coef(lm1), confint(lm1)) 


# back-transform by 2^x for a better interpretation, then:
# - intercept is median response to concentration=1 on the original scale (extrapolated)
# - slope is relative reduction in (median) diameter by factor XXX when concentration is doubled
2^(cbind(coef(lm1), confint(lm1)))

# to express slope as %-wise reduction in diameter when concentration is doubled use:
100*(1-2^(cbind(coef(lm1), confint(lm1))))['log2conc',]

# Prediction for a concentration of 250,000 cells/ms
x0 <- log2(250000)
predict(lm1,newdata=data.frame(log2conc=x0))
# Compute the predicted value "by hand"
lm1$coef[1]+x0*lm1$coef[2]
# back-transformed
2^(predict(lm1,newdata=data.frame(log2conc=x0)))


# prediction interval and confidence interval for regression line (log2 scale)
# 1. Create dataset with all possible log2 concentrations from 5 to 20, by increment of 0.1.
th.new <- data.frame(log2conc=seq(5,20,by=0.1))
head(th.new) # visualize the result
# 2. Create dataset containing all the concentration, the predicted value and the prediction interval
th.pi <- cbind(th.new, predict(lm1, th.new, interval='pred'))
head(th.pi) # visualize the result
# 3. Create dataset containing all the concentration, the estimated (mean) value and the 95% confidence interval
th.ci <- cbind(th.new, predict(lm1, th.new, interval='conf'))
head(th.ci) # visualize the result
# 4. Plot the results
plot(th$log2conc, th$log2diam, ylim=c(4.23,4.81),col="red",pch=19,ylab='log2(Diameter)',xlab='log2(Concentration)') # First plot the raw data
abline(lm1,col="blue",lwd=3,lty=1)  # Add fitted line (i.e. predicted/estimated mean values)
lines(th.pi$log2conc, th.pi$upr, lty=2, lwd=3) # Add upper limit of 95% prediction interval
lines(th.pi$log2conc, th.pi$lwr, lty=2, lwd=3) # Add lower limit of 95% prediction interval
lines(th.ci$log2conc, th.ci$upr, col='blue') # Add upper limit of 95% confidence interval
lines(th.ci$log2conc, th.ci$lwr, col='blue') # Add lower limit of 95% confidence interval
# Add legend
legend("topright",
       legend=c("Fitted line","95% Confidence interval","95% Prediction interval"),
       col=c("blue","blue","black"),
       lty=c(1,1,2),lwd=c(2,1,3))

# Plot back-transformed for a maybe nicer picture
plot(th$concentration, th$diameter, ylim=c(18,28),col="red",pch=19,ylab='Diameter (micrometer)',xlab='Concentration (100,000 cells/ml)') 
lines(2^th.pi$log2conc, 2^th.pi$fit, lty=1, lwd=3,col="blue")
lines(2^th.pi$log2conc, 2^th.pi$upr, lty=2, lwd=3)
lines(2^th.pi$log2conc, 2^th.pi$lwr, lty=2, lwd=3)
lines(2^th.ci$log2conc, 2^th.ci$upr, col='blue') 
lines(2^th.ci$log2conc, 2^th.ci$lwr, col='blue') 
legend("topright",
       legend=c("Fitted line","95% Confidence interval","95% Prediction interval"),
       col=c("blue","blue","black"),
       lty=c(1,1,2),lwd=c(2,1,3))

# Model checking: Residual plot and QQplot
par(mfrow=c(1,2)) # split the plot window in two
plot(lm1, which=c(1,2),ask = FALSE,qqline=FALSE,col="blue")
abline(0,1,col="grey",lty=2)
par(mfrow=c(1,1)) # back to usual plot window


# Wally plot
permsr <- function(n) {sample(x=residuals(lm1),size=n,replace=FALSE)}
permsrplot <- function(x, y, ...) {
    plot(x, y,ylab="Residuals",xlab="Fitted values",  ...) ;
    abline(h=0,lty=3,col="grey")
    lines(lowess(x, y), lty=1,col="red")}
# Now create the wally plot based on the above technical details
wallyplot(predict(lm1),residuals(lm1), FUN=permsrplot,simulateFunction=permsr,hide=FALSE,col="blue")


# Model fit and check without log2 transformations
lmOrScale <- lm(diameter~concentration,data=th) # Fit linear model on the original scales
par(mfrow=c(1,2)) # split the plot window in two
# plot raw data (original scales)
plot(th$concentration,
     th$diameter,
     ylab='Diameter (micrometer)',
     xlab='Concentration (100,000 cells / ml)',
     col="red",pch=19,
     ylim=c(18,28)
     )
# add regression line
abline(lmOrScale,col="blue",lwd=2)
# Residual plot
plot(lmOrScale, which=1,ask = FALSE,col="blue")


# Load the data example 2 via internet
load(url("http://paulblanche.com/files/ckd.rda"))
# Alternatively, you have first saved "ckd.rda" in a folder in your own laptop
# and you can now load the data by indicating where the data are located,
# by replacing "your/path/to/" in the command line below by the appropriate path to the data.
# load("~/your/path/to/ckd.rda")

# Print list of object in memory
ls() # here you should see the object "ckd", which contains the data.

# Pearson correlation coefficient
cor.test(ckd$pwv0,ckd$aix0)
# Spearman correlation coefficient
cor.test(ckd$pwv0,ckd$aix0,method="spearman")
