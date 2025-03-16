#--------- Follicle data case study: Rdemo starts ------------

rm(list=ls()) # clear R memory
load(url("http://paulblanche.com/files/HFollicles.rda"))
ls() # list objects
d <- HFollicles
head(d)
nrow(d) # 724 rows/follicles

# Data management steps: create outcome, log2-transform and make factor variables
d$growth <- d$Day8-d$Day0
d$loggrowth <- log(d$growth,base=2)
d$logDay0 <- log(d$Day0,base=2) - log(75,base=2)
d$PatientID <- factor(d$Patient)
d$Treat <- factor(d$Treatment)
head(d,n=10) # quick check


# Keep only data about follicles alive at day 8
d <- d[!is.na(d$loggrowth),]
nrow(d)  # 401 rows/follicles



library(lmerTest)
# fit random effect model (aka mixed model)
fitlmer <- lmer(loggrowth ~ Treat + logDay0 + (1|PatientID), data=d)
summary(fitlmer) # summary of the model fit
confint(fitlmer) # get confidence intervals of model parameters

# from the summary(fitlmer) we read the values
# of the Std corresponding to the between and within variances
omegaB <- 0.1709 
tauW <- 0.4066
# we compute the intra-class correlation
rho <- omegaB^2/(omegaB^2 + tauW^2)
rho

library(multcomp)
Multc <- glht(fitlmer,mcp(Treat="Tukey")) # make all-pairwise comparisons
summary(Multc,test=adjusted(type = "none")) #  p-values not adjusted for multiple testing
# summary(Multc) # now adjusted for multiple testing, using the min-P/max-t test approach (not needed in this case study)
confint(Multc,calpha = qnorm(0.975))$confint # 95%-CI not adjusted for multiple testing
round(2^confint(Multc,calpha = qnorm(0.975))$confint,2) # back-transformed 95%-CI (rounded, 2 digits), not adjusted for multiple testing (ratios of means/medians)

#--------- Follicle data case study: Rdemo stops ------------


#--------- GLP-2 data case study: Rdemo starts ------------
rm(list=ls()) # clear R memory
load(url("http://paulblanche.com/files/GLP2wide.rda"))
ls()
d <- GLP2wide

# time of blood samples (in mins)
thetimes <- c(-15,10,20,30,45,60,90,120,180,240)
# create data in the long format
long <- reshape(d, 
                varying = c("GLP2.minus.15",
                            "GLP2.10", "GLP2.20", "GLP2.30", "GLP2.45",
                            "GLP2.60", "GLP2.90",
                            "GLP2.120", "GLP2.180",
                            "GLP2.240"), 
                v.names = "GLP2",
                timevar = "time",
                times=thetimes,
                direction = "long")
long <- long[order(long$ID,long$meal),] # reorder by subject ID and time of blood sample
rownames(long) <- NULL # delete row names
head(long,n=20) # quick check (note: id is the identification number of a series of measurements; ID is that of the subject)


# Make Spaghetti plot
library(lattice)
xyplot(GLP2~time | meal, # show GLP2 per time, for each meal
       data=long, # data in long format
       group=ID, # note: "group=id" produces a similar plot, but without the same color for the same subjects 
       type='b', # both points and lines are drawn
       xlab="Time since food intake (mins)",
       ylab="GLP-2 (pmol/L)",
       scales=list(x=list(at=thetimes)) # optional: just to choose values to show on x-axis
       )

# data management: make new data of AUCs in wide format, with one column per meal, one row per subject
dauc <- data.frame("High carbohydrate"=d[d$meal=="High carbohydrate","AUC"],
                   "High fat"=d[d$meal=="High fat","AUC"],
                   "High protein"=d[d$meal=="High protein","AUC"],
                   check.names=FALSE # just to allow space in the column names
                   )
dauc # see/check resulting data

# paired t-test
t.test(dauc[,"High protein"],dauc[,"High fat"],paired=TRUE)


#--------- GLP-2 data case study: Rdemo stops ------------


#--------- Baseline follow-up case study: Rdemo starts ------------

rm(list=ls()) # clear R memory
load(url("http://paulblanche.com/files/Neuro.rda")) 
d <- Neuro
head(d)
summary(d)
mean(d$baseline) #  approximately 10

# center baseline value (makes interpretation easier)
d$baseline <- d$baseline-10
summary(d)

library(LMMstar)
# plot the missing data pattern
MissPat <- summarizeNA(d[,c("baseline","week6","week12")])
plot(MissPat)

# create data in long format
long <- reshape(d,
                idvar = "id",
                timevar="visit",
                varying = c("week6","week12"),
                v.names="score",
                direction = "long")
head(long) # check
long <- long[order(long$id),] # re-order by patient
head(long)

# relevel as convenient (see slides)
long$visit <- relevel(as.factor(long$visit),ref="2") 

# fit MMRM (Mixed-effect Model for Repeated Measures)
lmmfit <- lmm(score~baseline*visit + trt*visit,
              repetition = ~visit|id,
              structure = "UN", data = long)
summary(lmmfit)

#--------- Baseline follow-up case study: Rdemo stops ------------
