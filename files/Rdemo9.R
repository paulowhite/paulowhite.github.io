rm(list=ls()) # clear all objects from R memory

# load the relevant packages
library(survival)
library(prodlim)
library(survRM2)
library(timeEL)

# load  data of first case study (pregnancy in subfertile women) 
load(url("http://paulblanche.com/files/subfertile.rda"))
head(subfertile)

# Kaplan-Meier estimation
KM1 <- prodlim(Hist(time,status)~1,data=subfertile)

# Plot the Kaplan-Meier survival curve
plot(KM1,marktime=TRUE) # Note: marktime=TRUE is to add "ticks" to display the censored observations

# Print the survival probability estime and 95%-CI at time t=5
summary(KM1,time=5)

# Print the median survival time estime and 95%-CI
quantile(KM1)

# load  data of second case study (carcinoma randomized clinical trial data) 
load(url("http://paulblanche.com/files/carcinoma.rda"))
head(carcinoma)

# Data management: make factor variables
carcinoma$trt <- factor(carcinoma$trt)
carcinoma$Tsize <- factor(carcinoma$T<=2,
                          levels=c(TRUE,FALSE),
                          labels=c("<=4cm",">4cm"))
carcinoma$disability <- factor(carcinoma$cond==1,
                               levels=c(TRUE,FALSE),
                               labels=c("No","Yes"))


# Kaplan-Meier estimation for each treatment group
KM2 <- prodlim(Hist(time,status)~trt,data=carcinoma)

# Default plot of the Kaplan-Meier survival curves
plot(KM2)

# More fine-tuned plot of the same Kaplan-Meier survival curves
plot(KM2,
     timeconverter="days2years", # just to conveniently change to the time unit
     legend=FALSE,
     atRisk.labels=c("Standard","Experimental"),
     atRisk.title="N. of subjects at risk:",
     marktime=TRUE,
     col=c("blue","orange")
     )

# Print the survival probability estime and 95%-CI at a time t=2 years in each group
summary(KM2,time=2*365) # Note: beware of the time unit!

# ----- Compute the estimated survival probability difference with 95%-CI and p-value.----
# Here we use the timeEL package for convenience. 
carcinoma$timeNum <- as.numeric(carcinoma$time) # Just because this package needs the time to be numeric
ResDiffSurv <- TwoSampleKaplanMeier(time=carcinoma$timeNum,  # 'time' variable
                                    status=carcinoma$status, # 'status' variable
                                    group=carcinoma$trt,     # 'group' variable, needs to be coded 0 or 1
                                    t=2*365,                 # follow-up time at which to compute the difference
                                    contr = list(method = "Wald")) # techincal details to make "usual" computation
print(ResDiffSurv,what="Diff") # Note: show the risk difference (NOT the difference in survival, but it is just the same with opposite sign!)
# -------------

# log-rank test
ResLogRank <- survdiff(Surv(time,status)~trt,data=carcinoma)
print(ResLogRank,digits=6) # Note: the digits option is to print a sufficient number of digits for the p-value

# Univariate Cox model
cox1 <- coxph(Surv(time,status)~trt,data=carcinoma)
summary(cox1) # print summary
print(summary(cox1),digits=8) # same with more digits, can be useful to read the p-value shown at the line "Score (logrank) test"


# Restricted Mean Survival Time (RMST) analysis
ResRMST <- rmst2(time=carcinoma$time/365, # trick: divide by 365 to have a time unit in years
                 status=carcinoma$status,
                 arm=carcinoma$trt,
                 tau=3) # Beware of the time unit!
ResRMST


# multiple cox regression
cox2 <- coxph(Surv(time,status)~trt+ age+ Tsize + disability ,data=carcinoma)
summary(cox2)




###########################################################
#  The understanding of the code examples presented       #
#  below will not be necessary for today's main exercise  #
#  (only needed for the extra part in question 9 and      #
#  additional exrcise B                          )        #
###########################################################


#------ Simple "visual check" of the proportional hazard assumption for this simple cox model-------
# We just want to compare the survival curves estimated via the cox model and Kaplan-Meier.
# Step 1: create "new data" to predict the survival
dnew1 <- data.frame(trt=factor(1))
dnew0 <- data.frame(trt=factor(0))
# Step 2:  predict/estimate the survival probabilities for both groups using the Cox model
scox1 <- survfit(cox1,newdata=dnew1) 
scox0 <- survfit(cox1,newdata=dnew0)
# Step 3: plot Kaplan-Meier curves
plot(KM2,legend=FALSE,col=c("blue","orange"))
# Step 4: add the estimated survival curves from the Cox model
lines(scox0$time,scox0$surv,col="grey50",type="s",lwd=3,lty=2)
lines(scox1$time,scox1$surv,col="black",type="s",lwd=3,lty=2)
# Step 5: add legends
legend("right",legend=c("Experimental","Standard"),lwd=3,lty=2,col=c("black","grey50"),title="Cox model estimates:   ",bty="n")
legend("topright",legend=c("Experimental","Standard"),lwd=2,lty=1,col=c("orange","blue"),title="Kaplan-Meier estimates:",bty="n")
#------------



#---- Estimated survival curves (with 95%-CI) with the multiple Cox model------
dnew0 <- data.frame(trt=factor(0),age=60,Tsize="<=4cm",disability="No")
dnew1 <- data.frame(trt=factor(1),age=60,Tsize="<=4cm",disability="No")
plot(scox0,col="blue",lwd=2,xlab="Time (days)",ylab="Survival probability",
     conf.type = "log-log")
lines(scox1,col="orange",lwd=2,add=TRUE)
title("Age=60, tumor size <=4cm, not disable")
legend("topright",
       legend=c("Experimental treatment","Standard treatment"),
       lwd=2,lty=1,col=c("orange","blue"),bty="n")
#----



#------------- competing risks analysis -------------
load(url("http://paulblanche.com/files/HFactionHosp.rda"))
head(HFactionHosp)

fitAJ <- prodlim(Hist(time,status)~trt,data=HFactionHosp)
plot(fitAJ) # default plot

# More fine-tuned plot of the same Aalen-Johansen estimates of the absolute risk curves (aka cumulative incidence functions)
plot(fitAJ,
     legend.x="topleft",
     legend.cex=0.9,
     legend.legend=c("No training","Training"),
     atRisk.labels=c("No training","Training"),
     atRisk.title="N. of subjects at risk:",
     marktime=TRUE,
     col=c("blue","orange")
     )

# Estimated absolute risk difference  with 95%-CI in each group at time t=3
summary(fitAJ,time=3) # read at "Cause:  1" for the "main" event (status=1), i.e., hospitalization

# ----- Compute the estimated absolute risk difference  with 95%-CI and p-value.----
# Here we use the timeEL package for convenience.
ResDiffRisk <- TwoSampleAalenJohansen(time=HFactionHosp$time,    # 'time' variable
                                      cause=HFactionHosp$status, # 'status' or 'cause' variable
                                      group=HFactionHosp$trt,    # 'group' variable, needs to be coded 0 or 1
                                      t=3,                       # follow-up time at which to compute the difference
                                      contr=list(method="Wald")) # techincal details to make "usual" computation
print(ResDiffRisk, what="Diff")
#-------------
