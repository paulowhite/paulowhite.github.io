#' ---
#' title: "Exercise 9 - solution"
#' author: "Paul Blanche"
#' output: pdf_document
#' fontsize: 12pt
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
rm(list=ls())

#' 
#' # Exercise A
#' 
#' ## Question 1
#' 
#' We first load the data and look at the "summary", as always.
## -----------------------------------------------------------------------------
rm(list=ls())
load(url("http://paulblanche.com/files/colon2.rda")) 
d <- colon2
summary(d)

#' 
#' ## Question 2
#' 
#' We keep only the observations corresponding to patients of these two groups. 
## -----------------------------------------------------------------------------
d <- d[which(d$rx %in% c("Obs","Lev+5FU")),]
d$rx <- droplevels(d$rx)   # drop the level no longer present in the dataset

#' We now look at the number of patients included in each treatment group.
## -----------------------------------------------------------------------------
table(d$rx)

#' We read that 315 did not receive the treatment and 304 did.
#' 
#' 
#' 
#' ## Question 3
#' 
#' Before creating the table, we first create factor variables for all the categorical variables and use appropriate labels.
## -----------------------------------------------------------------------------
d$sex <- factor(d$sex,levels=c(1,0),labels=c("male","female"))
d$obstruct <- factor(d$obstruct,levels=c(0,1),labels=c("no","yes"))
d$perfor <- factor(d$perfor,levels=c(0,1),labels=c("no","yes"))
d$adhere <- factor(d$adhere,levels=c(0,1),labels=c("no","yes"))
d$differ <- factor(d$differ,levels=c(1,2,3),labels=c("well","moderate","poor"))
d$extent <- factor(d$extent,levels=c(1,2,3,4),labels=c("submucosa",
                                                       "muscle",
                                                       "serosa",
                                                       "contiguous structures"))
d$surg <- factor(d$surg,levels=c(0,1),labels=c("short","long"))
d$node4 <- factor(d$node4,levels=c(0,1),labels=c("<=4",">4"))

#' We are now better prepared to create a descriptive table. The aim of
#' this table is to summarize the baseline variable distributions in each
#' treatment group. We report median, first and third quartile for
#' quantitative variables and frequencies and percentages for categorical
#' variables. As recommended by guidelines (e.g. CONSORT), we do not
#' present useless p-values to test for differences between the two
#' groups.
#' 
## -----------------------------------------------------------------------------
library(Publish)
tab1 <- univariateTable(rx~Q(age) + Q(nodes) + sex +
                            obstruct + perfor + adhere +
                            differ + extent + surg + node4,
                        data=d,
                        compare.groups = FALSE,
                        show.totals = FALSE)
tab1

#' 
#' We see that the distributions of all the covariates seem to be fairly
#' similar in the two groups. This is not at all a surprise: this was
#' expected because of the randomization of the treatment
#' allocation. Because of the random allocation to each treatment group,
#' the patients are as old/young and healthy/commorbid in the two
#' groups. The two populations differ only by treatment. Hence, if we
#' observe a survival difference when comparing the two groups (at a
#' later question), we can confidently conclude that this must be a
#' consequence of the treatment.
#' 
#' ## Question 4
#' 
#' We now use the Kaplan-Meier method to estimate the survival curves in
#' each treatment group. We use the `timeD` and `statusD` variables, not
#' `time` and `status`. This is because according to the description of
#' the data, they are those relevant when studying all-cause death as the
#' endpoint.
#' 
## -----------------------------------------------------------------------------
library(survival)
library(prodlim)
KM1 <- prodlim(Surv(timeD,statusD)~rx,data=d)

#' 
#' We can then plot the curves. We use the option
#' `timeconverter="days2years"` to show the years instead of days. The
#' time unit is "days" in the dataset, but the interpretation is easier
#' using "years".  We could of course further fine tune the plot (if
#' relevant) as exemplified in the R-demo.
#' 
## ----fig.height=7,fig.width=7-------------------------------------------------
plot(KM1,timeconverter="days2years")

#' 
#' The plot suggests that the treatment improves the survival chances of
#' the patients. Indeed, the survival curve corresponding to those
#' treated is well above that of those not treated. This is the case
#' everywhere (i.e. for all time points) maybe except at the very
#' beginning (say within the first year), but there is anyway too much
#' uncertainty there (as shown by the confidence intervals) to draw a
#' clear conclusion. The (pointwise) 95% confidence intervals do not
#' really cross after approximately 3 years. This suggests that the data
#' contain rather strong evidence that the treatment work.
#' 
#' 
#' ## Question 5
#' 
#' We now compute the median survival times (with 95%-CI) for each treatment group.
## -----------------------------------------------------------------------------
quantile(KM1)

#' We estimate that, without treatment, half of the patients die within 2083 days (95%-CI=[1548,  2552]). We can convert to years as follows.
## -----------------------------------------------------------------------------
round(c(2083,  1548,  2552)/365,1)

#' 
#' That is 5.7 years (95%-CI=[4.2,7.0]). We could have read
#' (approximately) this results from the Kaplan-Meier plot we previously
#' produced. Indeed, we just need to read the x-values (i.e., the times)
#' at which the survival curve and 95%-CI intersect the y=0.5 horizontal
#' line (see lecture slides).
#' 
#' For the treated, we do not obtain an estimated value for the
#' median. We obtain a value for the lower limit of the confidence
#' interval only (2725 days, i.e. 7.5 years). Therefore, we can only
#' conclude that we are 95% confident that the median survival time is
#' larger than 7.5 years. This is because only the curve of the lower
#' limit of the (pointwise) confidence interval intersect with the y=0.5
#' horizontal line. We do not observe enough deaths to know more about
#' the median survival time for this group.
#' 
#' 
#' ## Question 6
#' 
#' We now report the estimated 7-year survival probabilities in each group, together with a
#' 95%-CI.
## -----------------------------------------------------------------------------
summary(KM1,time=7*365)

#' 
#' We read that we estimated the 7-year survival probability to be 43.5%
#' (95%-CI=[37.3,49.7]) for those who did not receive the treatment,
#' versus 57.7% (95%-CI=[51.6,63.8]) for those who did.
#' 
#' 
#' We now report the estimated difference in 7-year survival probability, together with a
#' 95%-CI and a p-value.
## -----------------------------------------------------------------------------
# First extract (and save) the relevant estimates for each group
KM1.res <- summary(KM1,time=7*365) # results for both groups 
KM10 <- as.matrix(KM1.res[KM1.res$rx=="Obs",c("surv","se.surv")])     # results for group rx="Obs"
KM11 <- as.matrix(KM1.res[KM1.res$rx=="Lev+5FU",c("surv","se.surv")]) # results for group trt="Lev+5FU"
# Second, compute the difference
diffSurv <- KM11[1,"surv"] - KM10[1,"surv"]
# Third, compute the s.e. of the difference
seDiffSurv <- sqrt(KM11[1,"se.surv"]^2 + KM10[1,"se.surv"]^2)
# Now compute the 95% CI 
lowerDiffSurv <- diffSurv - qnorm(1-0.05/2)*seDiffSurv
upperDiffSurv <- diffSurv + qnorm(1-0.05/2)*seDiffSurv
# And the -value
pvalDiffSurv <- 2*(1-pnorm(abs(diffSurv/seDiffSurv)))
# Put all the results together 
ResDiffSurv <- c(Est=diffSurv,
                 lower=lowerDiffSurv,
                 upper=upperDiffSurv,
                 p=pvalDiffSurv)
# print the difference, 95% CI and p-value
round(ResDiffSurv,3)

#' 
#' We read that we estimate a statistically significant difference of
#' 14.2% (95%-CI=[5.5,22.9], p=0.001) in favor of those receiving the
#' treatment. By "statistically significant", we mean that the data
#' contain enough evidence to reject the null hypothesis of no difference
#' in 7-year survival probability.
#' 
#' ## Question 7
#' 
#' Before we move on to the main analysis (at question 11), we first
#' compare the survival curves of the two treatment groups via a simple
#' log-rank test (just to practice and explore the data further). 
#' 
## -----------------------------------------------------------------------------
ResLogRank <- survdiff(Surv(timeD,statusD)~rx,data=d)
print(ResLogRank,digits=6)

#' 
#' We read that the p-value is significant (p=0.002). The null hypothesis
#' of this test is that the survival curves of the two groups are
#' identical (everywhere, i.e. at all times). The small p-value is not
#' surprising, as we have previously seen that the curves are estimated
#' to be very different, with (pointwise) 95%-CI which mostly do not
#' overlap for time points between say 3 and 9 years (approximately).
#' 
#' 
#' ## Question 8
#' 
#' To accompany the p-value of the log-rank test, it is considered good practice to report an
#' estimated "effect size" and 95%-CI. That is, the hazard ratio obtained from a univariate
#' Cox model and its 95%-CI. We use the `coxph` function to compute this as follows.
#' 
## -----------------------------------------------------------------------------
library(survival)
cox1 <- coxph(Surv(timeD,statusD)~rx,data=d)
summary(cox1)

#' 
#' We read that the hazard ratio is estimated to 0.69
#' (95%-CI=[0.55,0.87], p=0.002). Here the p-value corresponds to the
#' null hypothesis of a hazard ratio equal to 1 (i.e. same hazard rate in
#' both groups). We can therefore conclude that the hazard rate (i.e. the
#' instantaneous risk of death among patients still alive at any time
#' during the follow-up) is 0.69 times smaller among the treated patients
#' than among the patients who did not receive the treatment
#' (95%-CI=[0.55,0.87], p=0.002).
#' 
#' We can also report the results in terms of a relative *instantaneous
#' risk* (or rate) reduction, by computing "one minus the hazard ratio".
#' 
## -----------------------------------------------------------------------------
round((1-exp(cox1$coef))*100,1)
round(rev(1-exp(confint(cox1)))*100,1)

#' 
#' Hence, we conclude that patients receiving fluorouracil plus
#' levamisole had a 31.1% reduction in mortality rate
#' (95%-CI=[13.1%,45.4%]). These results approximately match those of
#' Moertel et al (1995), who analyzed an almost identical version of
#' these data and found 33% (95%-CI=[16%,47%]).
#' 
#' 
#' 
#' ## Question 9
#' 
#' We now produce a simple plot to visually compare the survival curves
#' for each treatment group, when they are estimated via either
#' Kaplan-Meier or a univariate Cox model. This is to check the
#' proportional hazards assumption of the Cox model.
#' 
## ---- fig.height=7,fig.width=7------------------------------------------------
# Step 1: create "new data" to predict the survival
dnew1 <- data.frame(rx="Obs")
dnew0 <- data.frame(rx="Lev+5FU")
# Step 2:  predict/estimate the survival probabilities for both groups
#  using the Cox model
scox1 <- survfit(cox1,newdata=dnew1) 
scox0 <- survfit(cox1,newdata=dnew0)
# Step 3: plot Kaplan-Meier curves
plot(KM1,legend=FALSE,col=c("blue","orange"),timeconverter="days2years")
# Step 4: add the estimated survival curves from the Cox model
lines(scox0$time,scox0$surv,col="grey50",type="s",lwd=3,lty=2)
lines(scox1$time,scox1$surv,col="black",type="s",lwd=3,lty=2)
# Step 5: add legends
legend("left",legend=c("Experimental","Standard"),lwd=3,lty=2,
       col=c("black","grey50"),title="Cox model estimates:   ",bty="n")
legend("topright",legend=c("Experimental","Standard"),lwd=2,lty=1,
       col=c("orange","blue"),title="Kaplan-Meier estimates:",bty="n")

#' 
#' The curves estimated via both methods are very similar. We therefore
#' conclude that nothing seems wrong with the proportional hazards
#' assumption of the Cox model. Hence, we can further conclude that the
#' hazard ratio that we have estimated via the Cox model is a sensible
#' "summary" of the difference between the two survival curves.  Had the
#' model fit been bad, this would not have been a good "summary" of the
#' difference.
#' 
#' **Reminder:** The Kaplan-Meier method does not make any assumption about how the
#' curves differ from each other. By contrast, the Cox model does make
#' such an assumption, via the proportional hazards assumption. For
#' instance, the Cox model assumes that the two curves cannot cross
#' (which is a consequence of the proportional hazards assumption).
#' 
#' ## Question 10
#' 
#' Before we move on to the main analysis (at question 11), we first
#' compare the restricted mean survival times (RMST) at t = 7 years for
#' the two treatment groups (just to practice and explore the data
#' further).
#' 
#' We first need to create a 0/1 binary variable for the treatment group
#'   to use the `rmst2` function of the package survRM2.
## -----------------------------------------------------------------------------
d$trt <- as.numeric(d$rx=="Lev+5FU")
table(d$trt)

#' We can now use the `rmst2` function as follows.
#' 
## -----------------------------------------------------------------------------
library(survRM2)
ResRMST <- rmst2(time=d$timeD/365, # trick to have a time unit in years
                 status=d$statusD,
                 arm=d$trt,
                 tau=7)            # Beware of the time unit!
ResRMST

#' 
#' We estimate that, if we treat future patients from the same population
#'     with fluorouracil plus levamisole and follow them for 7 years, the
#'     average time spent alive will be approximately 5.2 years
#'     (95\%-CI=[4.9,5.5]).  By contrast, if we do not treat future
#'     patients from the same population and follow them for 7 years, the
#'     average time spent alive will be approximately 4.6 years
#'     (95\%-CI=[4.4,4.9]).  That is, 0.56 years less
#'     (95\%-CI=[0.17,0.94], p=0.005).
#' 	
#' 	
#' ## Question 11
#' 
#' We now proceed to the main analysis. We fit a multiple Cox regression
#' model to compare the survival chances of a patient who received
#' levamisole plus fluorouracil to that of a patient who did not, when
#' both patients are similar with regards to:
#' 
#'  - obstruction of the colon by the tumor (`obstruct`),
#' 
#'  - number of positive lymph nodes (more or less than 4, `node4`)
#' 
#'  - time from surgery to inclusion into the trial (short vs long,
#'     `surg`)
#'  
#'  - age and sex
#' 
#' We assume that the following was prespecified. We do not model any interaction and age
#' is used in the model as a continuous variable with a (usual/simple) log-linear effect on
#' the hazard rate.
#' 
## -----------------------------------------------------------------------------
cox2 <- coxph(Surv(timeD,statusD)~rx+obstruct+node4+surg+age+sex,data=d)
summary(cox2)

#' 
#' We estimate the hazard ratio to be 0.70 (95%-CI=[0.55,0.88],
#' p=0.003). That is, we estimate that the mortality rate is reduced by
#' 30% (95%-CI=[12%,45%], p=0.003), for a treated patient, as compared to
#' a patient who is not treated, when both have the same age and sex, and
#' are also similar with respect to time from surgery to inclusion into
#' the trial, number of positive lymph nodes and obstruction of the colon
#' by the tumor. The main analysis shows significant results: we obtained
#' a positive finding!
#' 
#' ## Question 12
#' 
#' One could wonder whether modeling a log-linear effect of age on the
#' hazard rate was a good idea. It is, after all and to some extent, an
#' arbitrary choice. It can be interesting to compare the previous
#' conclusions to those obtained when modeling the effect of age via age
#' groups, which does not require to assume a log-linear effect.  For
#' instance, we can use four age groups: 18-50, 50-60, 70-85.
#' 
## -----------------------------------------------------------------------------
d$agec <- cut(d$age,breaks=c(18,50,60,70,85),include.lowest=TRUE)
table(d$agec,useNA="always")
cox2c <- coxph(Surv(timeD,statusD)~rx+obstruct+node4+surg+agec+sex,data=d)
summary(cox2c)

#' 
#' We now estimate the hazard ratio to be 0.68 (95%-CI=[0.54,0.86],
#' p=0.001). The main conclusion does not change at all. This simple
#' "sensitivity analysis" is very reassuring.
#' 
#' # Exercise B
#' 
#' (only the R code is provided)
#' 
#' 
#' ## Question 1
#' 
## ---- fig.height=7,fig.width=7------------------------------------------------
AJ1 <- prodlim(Hist(time,status)~rx,data=d)
plot(AJ1,timeconverter="days2years") 

#' 
#' ## Question 2
#' 
## ---- fig.height=7,fig.width=7------------------------------------------------
KMPFS <- prodlim(Surv(time,status!=0)~rx,data=d)
plot(KMPFS,timeconverter="days2years",ylab="Progression-Free Survival")

#' 
#' 
## -----------------------------------------------------------------------------
cox2PFS <- coxph(Surv(time,status!=0)~rx+obstruct+node4+surg+age+sex,data=d)
summary(cox2PFS)

