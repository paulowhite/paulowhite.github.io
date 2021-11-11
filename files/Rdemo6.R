rm(list=ls()) # clear all objects from R memory

library(Publish)

# Load the framingham data via internet
load(url("http://paulblanche.com/files/framingham.rda"))
head(framingham)

# Estimate a logistic model with (only) sex as predictor variable
fit1 <- glm(disease~sex, data=framingham, family=binomial)
summary(fit1) # default summary of the model fit
publish(fit1) # results nicely formatted
confint(fit1) # 95% CI of model parameters 
exp(coef(fit1)[-1])     # estimated ORs (note [-1] is because the first parameter, the intercept, does not correspond to an OR)
exp(confint(fit1)[-1,]) # 95% CI of ORs


# Compare with results from 2x2 table
# Note that below we explicitely specify the reference levels
# to make the order the rows and columns as desired. 
TabSex <- table(relevel(framingham$sex,ref="Female"), 
                factor(framingham$disease,levels=c(1,0)))
table2x2(TabSex,stat=c("table","or"))


# Change the reference level for sex and refit the model
framingham$sexF <- relevel(framingham$sex,ref="Female")
fit1a <- glm(disease~sexF, data=framingham, family=binomial)
summary(fit1a)
publish(fit1a)

# make the age group variable
framingham$AgeCut <- cut(x=framingham$AGE,
                         breaks=c(40,48,52,56,99),
                         labels=c("45-48","49-52","53-56","57-62")) # specify more suitable labels
TabAge <- table(as.numeric(framingham$disease),framingham$AgeCut)
TabAge # 2x2 table
addmargins(TabAge) # add margins and print the table

# Estimate a logistic model with (only) the age group as predictor variable
fit3 <- glm(disease~AgeCut, data=framingham, family=binomial)
summary(fit3) # default summary of the model fit
publish(fit3) # result nicely formatted
confint(fit3) # 95% CI of model parameters
exp(coef(fit3)[-1])     # estimated ORs (note [-1] is because the first parameter, the intercept, does not correspond to an OR)
exp(confint(fit3)[-1,]) # 95% CI of ORs

# Change the reference level for the age group variable and refit the model
framingham$AgeCutb <- relevel(framingham$AgeCut,"53-56")
fit3b <- glm(disease~AgeCutb, data=framingham, family=binomial)
publish(fit3b)

#---
# Make all pairwise comparison and adjust for multiple testing
# using the modern min-P method implemented in the multcomp package.
library(multcomp)  
Res3 <- glht(fit3, mcp(AgeCut="Tukey"))
# note that is the code line above:
## - we use the fitted model "fit3".
## - we explain that we want to compare different groups according to the specific variable AgeCut.
## - we explain that we want all pairwise comparisons with the keyword "Tukey" (change to  "Dunnett" for making many-to-one comparisons).
summary(Res3) # summary of the results which provides the adjusted p-values for each comparison
confint(Res3) # Estimated parameters and 95% CI (log of odds ratio)
exp(confint(Res3)$confint) # Estimates and 95% CI for odds ratios
#---

# Fit logistic model with age as a continuous variable (assuming linearity)
fit5 <- glm(disease~AGE,data=framingham,family=binomial)
summary(fit5)
publish(fit5)

# Refit the model such that 1 unit of the age variable corresponds to 10 years
framingham$age10 <- framingham$AGE/10 # new variable age with unit "10 years"
fit5b <- glm(disease~age10,data=framingham,family=binomial)
publish(fit5b)


# 2x2 table: smoke versus sex
framingham$Smoke <- factor(framingham$CIG>0,levels=c(FALSE,TRUE),labels=c("No","Yes"))
TabSmokeSex <- table(Smoke=framingham$Smoke,Sex=framingham$sex)
TabSmokeSex
prop.table(TabSmokeSex,margin=2) # to compare the proportions of smokers among women and among men

# Fit logistic model with predictor variables sex and Smoke (without interaction)
fit2 <- glm(disease~sex+Smoke,data=framingham,family=binomial)
summary(fit2)
publish(fit2)

# Fit logistic model with predictor variables AGE and sex (without interaction)
fit6 <- glm(disease ~ AGE + sex, family = binomial, data = framingham)
summary(fit6)
publish(fit6)

#----------- Plot Estimated/Predicted risks ----------------
# Estimated/Predicted risks by the model with predictor variables AGE and sex (without interaction)
# 1. Create new data "dnew" containing all subjects profile for which we want to make a
# prediction (here all men and all women of age between 45 and 62)
dnew <- expand.grid(AGE=seq(from=45,to=62,by=1),
                    sex=c("Female","Male"))
# 2. Compute predicted risks for each age and sex profile in new data "dnew"
dnew$risk <- predict(fit6,newdata=dnew,type="response")
# 3. visualize the results (first lines)
head(dnew)
# 4. Plot the estimated risk for Men
plot(dnew$AGE[dnew$sex=="Male"],dnew$risk[dnew$sex=="Male"],
     ylim=c(0,1),
     type="l",lwd=2,xlab="Age (years)",ylab="Risk of CHD (%)")
# 5. Add the predicted risk for women to the plot
lines(dnew$AGE[dnew$sex=="Female"],dnew$risk[dnew$sex=="Female"],type="l",lwd=2,col="red")
# 6. Add a legend
legend("top",legend=c("Male","Female"),col=c("black","red"),lwd=2,bty="n")

# Fit logistic model with predictor variables AGE and sex and their interaction
fit7 <- glm(disease ~ AGE +  sex + AGE:sex, family = binomial, data = framingham)
summary(fit7)
publish(fit7)

# Fit logistic model with predictor variables sex and Smoke and their interaction
fit8 <- glm(disease~sex*Smoke,data=framingham,family=binomial)
summary(fit8)
publish(fit8)

#-------------- Additional -----------------
# Below is an additional R code to compare the predicted risks of the model
# that uses age as a continuous variable (linear) to the predicted risks of
# the model that uses age as a categorical variable.

# 1. Fit the two models
fit3 <- glm(disease~AgeCut, data=framingham, family=binomial)
fit5 <- glm(disease~AGE,data=framingham,family=binomial)
# 2. Defines all ages from 45 to 62 for which we want to compute the predicted risks
AllAges <- seq(from=45,to=62,by=1)
# 3. Defines the age groups corresponding to all these ages.
AllAgeCut <- cut(AllAges,
                 c(40,48,52,56,99),
                 labels=c("45-48","49-52","53-56","57-62"))
# 4. Compute the predicted risks by the two models
AllRisksLinear <- predict(fit5,newdata=data.frame(AGE=AllAges),type="response")
AllRisksCat <- predict(fit3,newdata=data.frame(AgeCut=AllAgeCut),type="response")
# 5. Plot the estimated risks
plot(AllAges,AllRisksLinear,
     type="b",
     col="red",
     ylab="Risk of CHD (%)",
     xlab="Age")
points(AllAges,AllRisksCat,pch=17)
legend("bottomright",
       legend=c("no categorization (linear)","categorization"),
       col=c("red","black"),
       title="Estimated risk:",
       pch=c(1,17))
