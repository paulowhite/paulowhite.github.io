#' ---
#' title: "Exercise 8 - solution"
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
#' We first load the data and look at the first lines.
## -----------------------------------------------------------------------------
rm(list=ls()) # clear R memory
load(url("http://paulblanche.com/files/HFollicles.rda"))
d <- HFollicles
head(d)

#' 
#' ### Question 1.a
#' 
#' We first make the necessary data management to follicle growth at day
#'     6, similarly to what has been done to study follicle growth at day
#'     8 in the R-demo.
#' 	
## -----------------------------------------------------------------------------
whichDay <- 6
d$growth <- d[,paste0("Day",whichDay)]-d$Day0
d$loggrowth <- log(d$growth,base=2)
d$logDay0 <- log(d$Day0,base=2) - log(75,base=2)
d$PatientID <- factor(d$Patient)
d$Treat <- factor(d$Treatment)
head(d,n=10) # quick check

#' 
#' ### Question 1.b
#' 
#' We then fit an appropriate random effect model, similar tho that of
#' the lecture.
## -----------------------------------------------------------------------------
d <- d[!is.na(d$loggrowth),] # Keep only data about follicles alive at that day
library(lmerTest)
fitlmer <- lmer(loggrowth ~ Treat + logDay0 + (1|PatientID), data=d)
summary(fitlmer) 

#' 
#' ### Question 1.c
#' 
#' We then report appropriate effect sizes and 95\%-CI to compare the
#'   efficacy of the four plasma products on the follicle growth. To make
#'   all-pairwise comparisons, we can use the `multcomp` package (here we
#'   do not need/want to adjust for multiple testing; this is exploratory research).
## -----------------------------------------------------------------------------
library(multcomp)
Multc <- glht(fitlmer,mcp(Treat="Tukey")) # make all-pairwise comparisons
summary(Multc,test=adjusted(type = "none")) #  p-values NOT adjusted

#' We then back transform to obtain ratios of means/medians and round (2 digits). 
#' Note that we use `calpha = qnorm(0.975)` (=1.96), the usual 97.5\% quantile of the standard normal distribution, to compute 
#' unadjusted confidence intervals (because we do not want adjustments for multiple testing).
## ----echo = TRUE--------------------------------------------------------------
MainRES <- round(2^confint(Multc, calpha = qnorm(0.975))$confint,2) 
MainRES[order(-MainRES[,1]),]

#' 
#' ### Question 1.d
#' 
#' The results are relatively similar to those of the growth at day 8,
#'   seen in the lecture, in terms of effect sizes (i.e., clinical relevance) 
#'   and statistical significance. The order, with hPL leading to the best 
#'   average growth and HSA leading to the worst, is the same. Indeed, running the same code after changing
#'   `whichDay <- 6` to `whichDay <- 8`, we obtain the following results (shown in the lecture):
## ----echo=FALSE, results = TRUE-----------------------------------------------
d <- HFollicles
whichDay <- 8
d$growth <- d[,paste0("Day",whichDay)]-d$Day0
d$loggrowth <- log(d$growth,base=2)
d$logDay0 <- log(d$Day0,base=2) - log(75,base=2)
d$PatientID <- factor(d$Patient)
d$Treat <- factor(d$Treatment)
#---
d <- d[!is.na(d$loggrowth),] # Keep only data about follicles alive at that day
fitlmer <- lmer(loggrowth ~ Treat + logDay0 + (1|PatientID), data=d)
Multc <- glht(fitlmer,mcp(Treat="Tukey")) # make all-pairwise comparisons
MainRES <- round(2^confint(Multc, calpha = qnorm(0.975))$confint,2) 
MainRES[order(-MainRES[,1]),]

#' 
#' ### Question 1.e
#' We first read the estimated values of the Between and Within variance
#' components in the output of `summary(fitlmer)`. We read that they are
#' $0.02584=0.1608^2$ and $0.18373=0.4286^2$. The estimated intra-class
#' correlation is then computed as follows,
## -----------------------------------------------------------------------------
omegaB <-0.1608
tauW <- 0.4286
rho <- omegaB^2/(omegaB^2 + tauW^2)
rho

#' 
#' And we read that it is estimated as 0.12, slightly smaller than when
#' analyzing the growth at day 8 in the lecture (it was 0.15).  For the
#' interpretation, we can repeat that the correlation quantifies how
#' similar is the log-growth of two random follicles of the same woman as
#' compared to that of two random follicles of two different woman, when
#' comparing two follicles grown with the same plasma product and of the
#' same baseline size. The larger the correlation and the more similar
#' the log-growth of the follicles. Because the correlation is estimated
#' slightly smaller, we can say that the log-growth is estimated less
#' similar within woman as compared to between woman, when looking at the
#' earlier timepoint 6 days instead of 8 days. However, the difference is
#' very small and it could be due to random sampling (i.e, statistical
#' uncertainty).
#' 
#' 
#' ### Question 2
#' 
#' 
#' After running a similar code as above (essentially changing 6 by 2, 4
#' or 8 in `whichDay <- 6`), we obtain all the results for the growth at
#' all days. They are summarized in the "Forest plot" below. The
#' estimated intra-class correlation for each day ("rho") is also
#' provided in the legend for completeness.
#' 
#' Overall, we can see that the results are in the same direction at 4, 6
#' and 8 days, but the longer the time to grow and the larger the
#' differences in mean growth between the different plasma product
#' conditions. At day 2, the differences are substantially smaller; maybe
#' because 2 days is not long enough to study follicle growth. This
#' conclusion is consistent with Figure 3.B in Cristina's paper, where we
#' can see that the differences between the estimated median growths are
#' larger at larger times.
#' 
#' We note that the intra-class correlation is estimated to increase over
#' time. 
#' 
## ----echo=FALSE, fig.height=7,fig.width=7-------------------------------------
rm(list=ls()) # clear R memory
spedec <- function(x,k=0) format(round(x,digits=k),nsmall=k)
load(url("http://paulblanche.com/files/HFollicles.rda"))
d <- HFollicles
lag <- -0.35
allt <- rev(c(2,4,6,8))
xat <- c("0.5","0.75","1","1.5","2")
orderToPlot <- c("hPL - FBS",
                 "UCP - FBS",
                 "UCP - HSA",
                 "HSA - FBS",
                 "UCP - hPL", 
                 "HSA - hPL")
d$logDay0 <- log(d$Day0,base=2) - log(75,base=2)
d$PatientID <- factor(d$Patient)
d$Treat <- factor(d$Treatment)
#--
par(mai=c(1,1.5,0.5,0.5))
plot(1,1,xlim=c(0.5,3),ylim=c(0.5,6.5),xlab="Ratio of means (or medians) of diameter growth",ylab="",col="white",axes=FALSE)
abline(v=seq(from=0.5,to=2,by=0.1),col="grey80",lty=1)
abline(v=1,col="grey10",lty=2,lwd=2)
allrho <- rep(NA,length(allt))
for(whichDay in allt){
    i <- which(whichDay==allt)
    lagx <- (i-1)*(-lag/2) + lag
    d$growth <- d[,paste0("Day",whichDay)]-d$Day0
    d$loggrowth <- log(d$growth,base=2)
    #---
    dx <- d[!is.na(d$loggrowth),] # Keep only data about follicles alive at that day
    fitlmer <- lmer(loggrowth ~ Treat + logDay0 + (1|PatientID), data=dx)
    Multc <- glht(fitlmer,mcp(Treat="Tukey")) # make all-pairwise comparisons
    MainRES <- round(2^confint(Multc, calpha = qnorm(0.975))$confint,2) 
    MainRES <- MainRES[orderToPlot,]
    points(x=MainRES[,"Estimate"],y=(6:1 - lagx),pch=19,col=i)
    segments(x0=MainRES[,"lwr"],
             x1=MainRES[,"upr"],
             y0=(6:1 - lagx),
             y1=(6:1 - lagx),lwd=2,col=i
             )
    #---
    ToPrint <- apply(MainRES,1,function(x) paste0(spedec(x[1],2),
                                                  " [",spedec(x[2],2),",",spedec(x[3],2),"]"))
    text(x=2.2,y=(6:1 - lagx),col=i,labels=ToPrint,pos=4)
    #-- intra-class correlation
    ResSD <- as.data.frame(VarCorr(fitlmer))
    omegaB <- ResSD[1,5]
    tauW <- ResSD[2,5]
    rho <- omegaB^2/(omegaB^2 + tauW^2)
    allrho[i] <- rho
}
axis(2,at=6:1,labels=orderToPlot,las=2)
axis(1,at=as.numeric(xat),labels=xat)
legend("bottom",fill=1:length(allt),legend=paste("day", allt," ; rho=",spedec(allrho,3)),bty="n")

#' 
#' # Exercise B
#' 
#' ## Question 1
#' 
#' We first load the data and visualize the first lines.
#' 
## -----------------------------------------------------------------------------
load(url("http://paulblanche.com/files/kneeSurgery.rda"))
d <- kneeSurgery
head(d)

#' 
#' We then create a baseline table to summarize the distribution of the
#'   important variables, per arm.
#' 
## -----------------------------------------------------------------------------
library(Publish)
Tab1 <- univariateTable(arm~site+Q(age)+sex+Oxford.pre, 
                        data=d,
                        compare.groups = FALSE,
                        show.totals = FALSE)
Tab1

#' 
#' There are no substantial difference between the two arms. This is a
#' consequence of the randomization. We can notice that the baseline
#' Oxford score (i.e., pre-surgery score) was slighty better in arm 1
#' than in arm 2, on average (1 point). There are aproximately as many
#' men and women and about half of the patients are aged between 60 and
#' 75. As expected, the baseline Oxford scores are rather low (indication
#' for surgey), but there is substantial variablity from patient to
#' patient (SD=6).
#' 
#' ## Question 2
#' 
#' Let's now have a quick look at the evolution of the scores, for a few
#' random patients. 
#' 
#' ### Question 2.a
#' 
#' We first select 20 random patients, to have approximately 10 of each
#' arm. We select just a few patients because the plots are often
#' difficult to read with too many patients and because a few patients is
#' usually sufficient to get a feel of the data.
#' 
## -----------------------------------------------------------------------------
d20 <- d[101:120,]

#' ### Question 2.b
#' 
#' To use the `xyplot()` function of the lattice" package to produce a
#'     spaghetti plot, we first need to create a long format version of
#'     the data. We can do it with the `reshape()` function.
#' 
## -----------------------------------------------------------------------------
thetimes <- c(0,1,6,12,24) # times of repeated measures
long20 <- reshape(d20, 
                varying = c( "Oxford.pre", "Oxford.01", "Oxford.06", 
                            "Oxford.12", "Oxford.24"), 
                v.names = "Oxford",
                timevar = "time",
                times=thetimes,
                direction = "long")
long20 <- long20[order(long20$id),] # reorder by subject id
rownames(long20) <- NULL            # delete row names
head(long20,n=20)                   # quick check

#' 
#' ### Question 2.c
#' 
#' We are now almost ready to call the `xyplot()` function of the
#' "lattice" package to produce a spaghetti plot. We just need to make
#' the variable `arm` a factor variable before.
#' 
## -----------------------------------------------------------------------------
library(lattice)
long20$arm <- factor(long20$arm,levels=1:2,
                     labels=c("arm=1","arm=2"))
xyplot(Oxford~time | arm,        # show score per time, for each arm
       data=long20,              # data in long format
       group=id,                 # patient id
       type='b',                 # both points and lines are drawn
       xlab="Time since surgery (months)",
       ylab="Oxford knee score",
       scales=list(x=list(at=thetimes))) # set values to show on x-axis

#' 
#' 
#' ## Question 3
#' 
#' Overall, the score of each patient is improving over time. However,
#' sometimes is goes down a bit before going up again. This will
#' typically happen if e.g., the health of the patient (pain and knee
#' function) is stable and patients answers slightly differently at the
#' same question in a random way, e.g, randomly switch from the answer
#' "With little difficulty" to "With moderate difficulty" (leading to 1
#' point difference in the score) when replying to the question "During
#' the past 4 weeks... Could you kneel down and get up again
#' afterwards?". Maybe more importantly, we see than for most patients,
#' the score changes much more within the 12 months than from 12 to 24
#' months (i.e., 1-year after surgery, the score does no longer change
#' much). Finally, based on these 20 patients, the outcome of the surgery
#' does not look very different, in average, at 2 years after surgery. Of
#' course, we should not draw any strong conclusion based on 20 patients
#' and it can be useful to redo the plot for 20 other randomly selected
#' patients. See e.g., the plot below obtained using `d20 <-
#' d[1:20,]`. **Note**: I can say that these patients are randomly
#' selected here because the rows of the dataset are not ordered in any
#' specific way. This would be a questionable statement if , e.g., we had
#' sorted the dataset by date of inclusion. Arm 1 looks so much better
#' when we randomly chose these patients ! This might be a good incentive
#' to redo the plot for 20 other random patients (again, just to get a
#' feel of the data).
#' 
## ----echo=FALSE---------------------------------------------------------------
d20 <- d[1:20,]
thetimes <- c(0,1,6,12,24) # times of repeated measures
long20 <- reshape(d20, 
                varying = c( "Oxford.pre", "Oxford.01", "Oxford.06", 
                            "Oxford.12", "Oxford.24"), 
                v.names = "Oxford",
                timevar = "time",
                times=thetimes,
                direction = "long")
long20 <- long20[order(long20$id),] # reorder by subject id
rownames(long20) <- NULL # delete row names
long20$arm <- factor(long20$arm,levels=1:2,
                     labels=c("arm=1","arm=2"))
xyplot(Oxford~time | arm,        # show score per time, for each arm
       data=long20,              # data in long format
       group=id,                 # patient id
       type='b',                 # both points and lines are drawn
       xlab="Time since surgery (months)",
       ylab="Oxford knee score",
       scales=list(x=list(at=thetimes)) # set values to show on x-axis
       )

#' 
#' ## Question 4
#' 
#' The next descriptive plot of interest with this kind of repeated
#' measurements data is the plot of the missing data pattern. We could
#' already see that we have missing data from the spaghetti plots,
#' because some subjects did not have dots at all times (and/or no lines
#' between the dots).
#' 
## -----------------------------------------------------------------------------
library(LMMstar)
MissPat <- summarizeNA(d[,c( "Oxford.pre",
                            "Oxford.01",
                            "Oxford.06", 
                            "Oxford.12",
                            "Oxford.24")])
plot(MissPat)

#' 
#' ### Question 4.a
#' 
#' The first line (1 patient), third line (1 patient), seventh line (2
#' patients) and ninth line (28 patients) are compatible with patients
#' being "lost to follow-up". That is, as soon as we have a missing data
#' because a patient does not answer a questionnaire, then the patient
#' does not reply either at questionnaires sent later. In practice, we
#' usually can (and should!)  collect data about the reasons why we have
#' missing data. In this exercise, we have no such data.
#' 
#' ### Question 4.b
#' 
#' Some patients did not reply to the questionnaire at some follow-up
#' times but replied later. E.g., 7 patients did not reply at the
#' questionnaire sent after 1 month, but replied to all questionnaires
#' sent later.  In total, we have 7+1+1+9+11=29 with this kind of
#' intermittent missing data.
#' 
#' ### Question 4.c
#' 
#' We can see that 285 out of 346, i.e. 82%, replied to all
#' questionnaires.
#' 
#' ### Question 4.d
#' 
#' For the analysis of the change score at 24 months, which is our
#' primary outcome of interest here, we can read from the x-axis than 33
#' patients have missing data (i.e., did not reply to the
#' questionnaire). That is, 9.5% missing data.
#' 
#' ## Question 5
#' 
#' Informally, Missing Completely At Random (MCAR) means that the
#'     missingness mechanism is unrelated to the outcome and
#'     covariates. In this context, it means that the missingness
#'     mechanism is unrelated to age, sex, study site and pre-surgery
#'     Oxford score (i.e., the baseline covariates that we will use in
#'     the model for the analysis) and also unrelated to previously
#'     collected Oxford knee scores (i.e., to the answer to the previous
#'     questionnaires). This can be realistic if the main reason for not
#'     answering is that the patients simply forgot to answer or that we
#'     failed to reach out to them. This might be unrealistic if patients
#'     who are doing bad have a much stronger incentive to answer the
#'     questionnaire than those who are doing well. It could happen if,
#'     e.g., there is a free text question at the end of the
#'     questionnaire that patient can use to further communicate their
#'     worries to their doctors. In that case, missing data would be less
#'     common among patients having a "good" score than among patients
#'     having a "poor" score.
#' 
#' Informally, Missing At Random (MAR) means that the missingness may
#'     depend on covariates and previous measures of the outcome. In this
#'     context, it means that the missingness mechanism can be related to
#'     age, sex, study site and any Oxford score obtained via previous
#'     questionnaires. This is more realistic (less
#'     restrictive). Although not perfect, because e.g., the missingness
#'     cannot depend on the current score (unobserved, because missing),
#'     the more correlated the scores over time and the closer we are
#'     from the situation in which we could assume that it can depend on
#'     this current value.
#' 	
#' 	
#' 
#' ## Question 6
#' 
#' We now perform the main analysis, by fitting a Mixed Model for
#'   Repeated Measurements (MMRM). Before calling the `lmm()` function,
#'   we first need to create a data set in the long
#'   format. Additionally, we will *center* the covariates `age` and
#'   Oxford score pre-surgey and choose 24 months as the reference level
#'   for the factor variable. This is just to facilitate the
#'   interpretation of the default output of the software.
#' 
## -----------------------------------------------------------------------------
long <- reshape(d, 
                varying = c("Oxford.01", "Oxford.06","Oxford.12", "Oxford.24"), 
                v.names = "Oxford",
                timevar = "time",
                times=c(1,6,12,24),
                direction = "long")
long <- long[order(long$id),] # reorder by subject ID
rownames(long) <- NULL # delete row names
## head(long,n=5)      # quick check
#--- data management steps ---
long$time <- factor(long$time)
long$time <- relevel(long$time,ref="24")
long$arm <- factor(long$arm)
long$change <- long$Oxford-long$Oxford.pre
long$age67 <- long$age-67               
long$Oxford.pre22 <- long$Oxford.pre-22 
#--- fit MMRM --------------------
lmmfit <- lmm(change~Oxford.pre22*time + site*time
              + sex*time + arm*time + age67*time,
              repetition = ~time|id,
              structure = "UN", data = long)
summary(lmmfit)

#' 
#' ## Question 7
#' 
#' Yes! The results suggest that this trial brings sufficient evidence
#' that one type of surgery is better than the other, for the mean Oxford
#' score at 2 years. We estimate that, in average, the change in Oxford
#' score at 2 years is 2.267 (95-CI=[0.81; 3.724], p=0.002) points lower
#' for patients who receive the surgery of arm 2 than for those receiving
#' the surgery of arm 1. Hence, there is evidence that arm 1 is the
#' better. Because, of randomization, this mean difference of 2.267 has
#' two interpretations; either a difference "in average" (as in the
#' previous sentence, the so-called *marginal* interpretation) or when
#' comparing patients similar for age, sex, study side and baseline
#' Oxford score (the so-called *conditional* interpretation). The
#' *conditional* interpretation is valid under the assumption that the
#' model is correct (e.g., no interaction between arm and age, as no
#' interaction was assumed) whereas the marginal interpretation is valid
#' even if the model is not correct, to a large extent (e.g., if the
#' model is incorrect because an interaction term between arm and age
#' exists and was not included in the model).
#' 
#' ## Question 8
#' 
#' The 95% confidence interval is [0.81,3.724]. So, we cannot rule out
#' that the difference is less than one point. This is very small. Even
#' the estimated value 2.267 is not very large, when looking at the
#' patient to patient variability in the change score at 24 months, when
#' considering patients of similar age, study site, gender, surgery arm
#' and baseline score. The standard deviation that quantifies this
#' variability is estimated as 6.67 (see `sigma.24` in the output of the
#' software). This means that, in average, patients who receive the
#' surgery of arm 1 are doing better than those receiving the surgery of
#' arm 2, but that a large proportion of patients receiving the surgery
#' of arm 2 will anyway have a better score at 24 months than many
#' patients receiving the surgery of arm 1.
#' 
#' ## Question 9
#' 
#' ## Question 9.a
#' 
#' The interpretation of the default output seen above was easy because
#' the timepoint of interest was the reference level for the factor
#' variable time. This was important because of the interaction terms
#' between `time` and `arm`. If we want an equally easy read of the
#' results to estimate the between-arm difference in mean change score at
#' earlier timepoints, we can simply change the reference level and
#' re-fit the model.
#' 
## -----------------------------------------------------------------------------
long$time6 <- relevel(long$time,ref="6")
#--- fit MMRM --------------------
lmmfit6 <- lmm(change~Oxford.pre22*time6 + site*time6
               + sex*time6 + arm*time6 + age67*time6,
               repetition = ~time6|id,
               structure = "UN", data = long)
summary(lmmfit6, print=FALSE)$mean["arm2",] # print only the line of interest

#' 
#' So, we estimate that, in average, the change in Oxford score at 6
#' months is 4.37 (95-CI=[2.84; 5.9], p=0.002) points lower for patients
#' who receive the surgery of arm 2 than for those receiving the surgery
#' of arm 1. Hence we estimate that the between-arm difference is larger
#' at the earlier timepoint of 6 months. Although changing the reference
#' level is our *favorite trick*, we could have read this from the
#' previous output. Indeed, -2.267 --2.107 = 4.37 (see lines with `arm2`
#' and `time6:arm2` in the output above). However, we could not have read
#' the confidence interval and p-value from the previous output. But,
#' from the previous output, we could instead see that the treatment
#' effect, i.e., the between-arm difference in change score, is estimated
#' significantly larger at 6 months than at 24 months (difference is
#' 2.107, 95-CI=[0.746;3,468], p=0.0025). In other words, there is
#' evidence that one surgery is better than the other and that the
#' superiority of this surgery is larger at 6 months than at 24
#' months. We can proceed similarly with other time points (e.g., 1 or 12
#' months).
#' 
#' ## Question 9.b
#' 
#' The model did not show evidence that age was associated with the
#' change score at 24 months (p=0.074). We can even say that if an
#' association exists, we are confident that it is rather small. Indeed,
#' the confidence interval for the difference in mean change score at 24
#' months, when comparing two patients, one 10 years older than the
#' other, both patients being similar for sex, arm, etc..., is
#' [-1.74,0.08]. Because the results suggest that the association between
#' the outcome and age is not very large, the gain in power obtained by
#' adjusting on age was probably very modest. A similar conclusion
#' applies to sex.
#' 
#' ## Question 10
#' 
#' A simpler analysis would have been to use a complete case analysis
#' with a simple ANCOVA model.
#' 
#' ### Question 10.a
## -----------------------------------------------------------------------------
nrow(d)
dCCA <- d[!is.na(d$Oxford.24),]
dCCA$change <- dCCA$Oxford.24 - dCCA$Oxford.pre
nrow(d)-nrow(dCCA) 

#' This analysis would excludes 33 patients (as already seen in question 4)
#' 
#' ### Question 10.a
#' 
## -----------------------------------------------------------------------------
fitANCOVA <- lm(change~Oxford.pre + site + sex + arm + age,data=dCCA)
summary(fitANCOVA)$coef["arm",] # print only the line of interest
confint(fitANCOVA)["arm",]      # print only the line of interest

#' 
#' The result of this simple complete case ANCOVA analysis is very
#' similar! This is somewhat reassuring.  We estimate that, in average,
#' the change in Oxford score at 2 years is 2.295 (95-CI=[0.829; 3.760],
#' p=0.002) points lower for patients who receive the surgery of arm 2
#' than for those receiving the surgery of arm 1. **Reminder**: the
#' result of the main analysis seen at Question 7 was 2.267 (95-CI=[0.81;
#' 3.724], p=0.002).
#' 
#' This is not very surprising because:
#' 
#'  - not so many patients were excluded using the complete case analysis
#'    (only 9.5%)
#' 
#'  - the missing completely at random assumption seems reasonable in the context of this trial
#' 
#'  - the two methods of analysis are equivalent with complete data (i.e.,
#'    we would have had the exact same results, if we had run the
#'    analysis with a data set without any missing data)
#'    
#'    
#' ## Question 11
#' 
#' An even simpler analysis would have been to use a complete case
#'   analysis with a t-test.
#' 
## -----------------------------------------------------------------------------
 t.test(change~arm,data=dCCA)

#' Here we estimate that, in average, the change in Oxford score at 2 years is
#' of 1.68 points lower for patients who receive the surgery of arm 2
#' than for those receiving the surgery of arm 1 (95\%-CI=[-0.05, 3.41],
#' p=0.057). The results is not statistically significant.
#' 
#' However, this is not the recommended approach, as it does not leverage
#' the information contained in the baseline variables (hence it is less
#' powerful; note the wider confidence interval, width is 3.41-(-0.05)=3.46 vs 
#' 2.91=3.724-0.81 using the MMRM of the main analysis).
#' 
#' 
#' ## Question 12
#' 
#' Many researchers wonder whether they should define their primary
#'   outcome as the score at end of follow-up or as the change score at
#'   end of follow-up. The previous questions consider the later.  We now
#'   perform the MMRM analysis using the former.
#' 
## -----------------------------------------------------------------------------
lmmfitOx <- lmm(Oxford~Oxford.pre22*time + site*time
              + sex*time + arm*time + age67*time,
              repetition = ~time|id,
              structure = "UN", data = long)
summary(lmmfitOx, print=FALSE)$mean["arm2",]

#' 
#' We get the exact same results! This is not so surprising if we think
#' about it, because we adjust for the baseline score. Comparing the
#' score at 24 months or the change score at 24 months is equivalent,
#' when comparing patients who have the same baseline score. It does not
#' matter whether we fit the model using the score or the change score at
#' 24 months, as long as we adjust for the baseline score.
#' 
#' Using the score or the change score at 24 moths is however different,
#' when we do not adjust for the baseline score, as e.g., when using a
#' simple t-test with a complete case analysis. That is another reason to
#' not like an analysis unadjusted for the baseline score (on top of the
#' power gain argument).
#' 
## -----------------------------------------------------------------------------
t.test(Oxford.24~arm,data=dCCA)

#' 
#' Here the result is different from that of question 11. It is now
#' significant! 
