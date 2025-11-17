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
