# Question 1: run the Rdemo

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


# Question 2: perform ANCOVA for complete case analysis.

# wide format data for complete case analysis
dCCA <- d[!is.na(d$week12),]
nrow(d)
nrow(dCCA)

ANCOVAfit <- lm(week12~baseline + trt, data = dCCA)
summary(ANCOVAfit) #est=0.9887, se=0.369, p-value=0.00825
summary(lmmfit)    #est=0.984 , se=0.364, p-value=0.00766

# Question 3: fit MMRM using comlpete case data.

# create data in long format for complete case analysis
longCCA <- reshape(dCCA,
                   idvar = "id",
                   timevar="visit",
                   varying = c("week6","week12"),
                   v.names="score",
                   direction = "long")
dim(longCCA)
dim(long)

# relevel as convenient (see slides)
longCCA$visit <- relevel(as.factor(longCCA$visit),ref="2") 

# fit MMRM (Mixed-effect Model for Repeated Measures)
lmmfitCCA <- lmm(score~baseline*visit + trt*visit,
                 repetition = ~visit|id,
                 structure = "UN", data = longCCA)
summary(lmmfitCCA)

# compare to ANOVA with same complete case data
summary(ANCOVAfit)$coef["trtExp",] # same est, se and p-value (as expected)
