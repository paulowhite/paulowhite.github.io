## * Data
data(armd.wide, package = "nlmeU")
## only keep relevant columns
dfW.prepost <- armd.wide[,c("subject","treat.f","lesion","visual0","visual4")]
## rename column
names(dfW.prepost)[names(dfW.prepost)=="treat.f"] <- "treatment"
## remove observations with missing values (for simplicity - but may not be a good idea in a real study)
dW.pp <- dfW.prepost[rowSums(is.na(dfW.prepost))==0,]

## * Introduction
## ** Basic concepts
## *** What are repeated measurements?
## *** Why performing repeated measurements? (1/3)
## *** Why performing repeated measurements? (2/3)
## *** Why performing repeated measurements? (3/3)
## ** Learning objectives
## *** Challenges when analyzing repeated measurements (1/2)
## *** Challenges when analyzing repeated measurements (2/2)
## *** Content
## * Pre-post study
## ** Dataset and data management
## *** Design: pre-post study
## *** Illustration: ARMD trial citep:interferon
## *** Illustration: =armd= dataset from the /nlmeU/ package
## *** Moving to the long format (1/3)
## *** Moving to the long format (2/3)

## chunk 4
head(dW.pp)

## chunk 5
## reshape to long format
dL.pp <- reshape2::melt(dW.pp, 
      id.var = c("subject","treatment","lesion"),
      measure.vars = c("visual0","visual4"),
      variable.name = "week", 
      value.name = "visual")

## chunk 6
## re-order dataset
dL.pp <- dL.pp[order(dL.pp$subject),]
## convert week as factor with appropriate values
dL.pp$week <- factor(dL.pp$week, 
                     level = c("visual0","visual4"),
                     labels = c(0,4))
## remove row names
rownames(dL.pp) <- NULL

## *** Moving to the long format (3/3)

## chunk 7
dW.pp[1:3,] # Wide format: 1 line = 1 subject

## chunk 8
head(dL.pp) # Long format: 1 line = 1 measurement

## *** Graphical display: histogram

## chunk 9
library(ggplot2)
gg <- ggplot(data = dL.pp,
             mapping = aes(x = visual, fill=treatment))
gg <- gg + facet_wrap(~week, labeller = label_both)
gg + geom_histogram(position  = "identity", 
                    alpha = 0.75, bins = 20)

## *** Graphical display: boxplot

## chunk 11
gg <- ggplot(data = dL.pp, 
   mapping = aes(x=week, y=visual, fill=treatment))
gg + geom_boxplot()

## ** Point estimation of the treatment effect
## *** Testing the treatment effect - exercise!

## *** Testing the treatment effect - solution!
## *** Testing the treatment effect - estimation!

## chunk 14
dW.ppA <- dW.pp[dW.pp$treatment=="Active",]
dW.ppP <- dW.pp[dW.pp$treatment=="Placebo",]
c(mu_A.0=mean(dW.ppA[,"visual0"]),
  mu_P.0=mean(dW.ppP[,"visual0"]))

## *** Estimating summary statistics on subgroups (1/2)
## *** Estimating summary statistics on subgroups (2/2)

## chunk 15
library(data.table)
dtL.pp <- as.data.table(dL.pp)
dtL.pp[,mean(visual),by = "week"]

## chunk 16
dtL.pp[,list(mu=mean(visual), sigma=sd(visual)), 
       by = c("treatment","week")]

## ** Statistical testing based on the difference
## *** Testing the treatment effect: t-test (1/2)

## chunk 17
dW.ppP$visualDiff04 <- dW.ppP$visual4-dW.ppP$visual0
dW.ppA$visualDiff04 <- dW.ppA$visual4-dW.ppA$visual0

## chunk 18
t.test(x=dW.ppP$visualDiff04, y=dW.ppA$visualDiff04)

## *** Testing the treatment effect: t-test (2/2)

## *** What about a standard linear regression?

## chunk 20
dW.pp$visualDiff04 <- dW.pp$visual4 - dW.pp$visual0
lm(visualDiff04 ~ treatment, data = dW.pp)

## *** Linear regression for heteroschedastic data

## chunk 21
library(nlme)
gls(visualDiff04 ~ treatment, data = dW.pp,
    weights = varIdent(form=~1|treatment))

## chunk 22
e.gls <- gls(visualDiff04 ~ treatment, data = dW.pp,
             weights = varIdent(form=~1|treatment))
cat("Coefficients:\n")
coef(e.gls)
cat("\n")
summary(e.gls$modelStruct)

## * Multivariate model
## *** 
## ** General concept
## *** Graphical display: are we missing something?

## chunk 23
gg <- ggplot(data = dL.pp, 
   mapping = aes(x=week, y=visual, fill=treatment))
gg + geom_boxplot()

## *** Graphical display: spaghetti plot (1/3)

## chunk 24
gg <- ggplot(data = dL.pp, 
   mapping = aes(x=week, y=visual, color=treatment))
gg + geom_point()

## *** Graphical display: spaghetti plot (2/3)

## chunk 26
gg <- ggplot(data = dL.pp, 
   mapping = aes(x=week, y=visual, color=treatment))
gg + geom_point() + geom_line(aes(group = subject))

## *** Graphical display: spaghetti plot (2/3)

## chunk 28
gg <- ggplot(data = dL.pp[dL.pp$subject < 20,], 
   mapping = aes(x=week, y=visual, color=treatment))
gg + geom_point() + geom_line(aes(group = subject))

## chunk 30
cor(dW.pp$visual0,
    dW.pp$visual4)

## *** Bivariate distribution
library(ggExtra)
library(gridExtra)
gg <- ggplot(dW.pp, aes(x=visual0, y=visual4, group = treatment, color = treatment)) + geom_point()
gg <- gg + xlab("visual at week 0") + ylab("visual at week 4")
gg <- gg + theme(text = element_text(size=25),
                 axis.line = element_line(size = 1.25),
                 axis.ticks = element_line(size = 2),
                 axis.ticks.length=unit(.25, "cm"),
                 legend.position="bottom")
empty <- ggplot() + geom_point(aes(1,1), colour="white") +
    theme(axis.ticks=element_blank(), 
          panel.background=element_blank(), 
          axis.text.x=element_blank(), axis.text.y=element_blank(),           
          axis.title.x=element_blank(), axis.title.y=element_blank())

gghist0 <- ggplot(dW.pp, aes(x=visual0, group = treatment, fill = treatment)) + geom_histogram(bins = 18, alpha=.75, position="identity") + guides(fill = FALSE)
gghist0 <- gghist0 + theme(plot.margin=unit(c(0,0,0,1.75),"cm"),
                           axis.ticks=element_blank(), 
                           panel.background=element_blank(), 
                           axis.text.x=element_blank(), axis.text.y=element_blank(),           
                           axis.title.x=element_blank(), axis.title.y=element_blank())

gghist4 <- ggplot(dW.pp, aes(x=visual4, group = treatment, fill = treatment)) + geom_histogram(bins = 18, alpha=.75, position="identity") + coord_flip() + guides(fill = FALSE)
gghist4 <- gghist4 + theme(plot.margin=unit(c(0,0,3.3,0),"cm"),
                           axis.ticks=element_blank(), 
                           panel.background=element_blank(), 
                           axis.text.x=element_blank(), axis.text.y=element_blank(),           
                           axis.title.x=element_blank(), axis.title.y=element_blank())

library(rayshader)
gg <- ggplot(dW.pp, mapping = aes(x=visual0, y=visual4)) 
gg <- gg + stat_density_2d(aes(fill = ..level..), geom = "polygon")
gg <- gg + facet_wrap(~treatment) 
gg <- gg + labs(fill="density") + xlab("visual at week 0") + ylab("visual at week 4")
gg <- gg + scale_fill_viridis_c(option = "D")
plot_gg(gg)

## *** Multivariate linear model - natural parametrisation
## *** Random intercept model - standard parametrisation
## ** Application in R
## *** Random intercept model in R

## chunk 32
e.lme <- lme(visual ~ week*treatment,
             random = ~1|subject,
             data = dL.pp)
logLik(e.lme)

## *** Mean structure - standard parametrisation

## chunk 33
data.frame(name = c("alpha","beta","gamma","delta"),
           value = fixef(e.lme),
           p.value = summary(e.lme)$tTable[,"p-value"])

## *** Fitted values

## chunk 34
UX <- unique(dL.pp[,c("week","treatment")])
UX

## chunk 35
UX$fit <- predict(e.lme, level = 0, newdata = UX)
UX

## *** Display fitted values

## chunk 36
gg <- ggplot(UX, aes(x = week, y = fit,
          group = treatment, color = treatment))
gg + geom_point() + geom_line()

## *** Variance-covariance structure (1/2)
## chunk 37

dL.pp$random <- fixef(e.lme)["(Intercept)"] + ranef(e.lme)[e.lme$groups[,1],1]

gg <- ggplot(data = dL.pp[dL.pp$subject < 20,], 
             mapping = aes(x=as.numeric(as.character(week)), y=visual, color=as.factor(subject)))
gg <- gg + geom_hline(aes(yintercept = fixef(e.lme)["(Intercept)"],linetype = "intercept"), size = 1.5)
gg <- gg + geom_point(aes(shape = "observation"), size = 3) + geom_line(aes(group = subject))
gg <- gg + guides(color = FALSE) + xlab("week") + scale_shape_manual("", values = c(17,16))
gg <- gg + scale_linetype_manual("", values = 2)
gg <- gg + theme(text = element_text(size=25),
                  axis.line = element_line(size = 1.25),
                  axis.ticks = element_line(size = 2),
                  axis.ticks.length=unit(.25, "cm"))

## *** Variance-covariance structure

## chunk 38
getVarCov(e.lme, individuals = 2:3, type = "marginal")

## * Conclusion
## ** Concluding remarks
## *** Conclusion
## *** Why using multivariate models?
## *** Want to know more?
## ** Backup
## *** Reference
## *** Natural parametrisation (1/3)

## chunk 39
dL.pp$treat <- factor(dL.pp$treatment, 
                      levels = c("Active","Placebo"),
                      labels = c("A","P"))

## chunk 40
e2.lme <- lme(visual ~ week:treat-1,
              random = ~1|subject,
              data = dL.pp)
logLik(e2.lme)

## chunk 41
fixef(e2.lme)

## *** Natural parametrisation (2/3)

## chunk 42
library(multcomp)
C <- "week4:treatA-week0:treatA-week4:treatP+week0:treatP=0"
e2.glht <- glht(e2.lme, linfct = C)
rownames(e2.glht$linfct) <- "delta"
summary(e2.glht)

## *** Natural parametrisation (3/3)
## *** Testing multiple null hypotheses
## chunk 43
library(multcomp)
e.glht <- glht(e.lme,
               linfct = c("week4:treatmentPlacebo=0",
                          "treatmentPlacebo+week4:treatmentPlacebo=0"))
rownames(e.glht$linfct) <- c("change","final value")
summary(e.glht)

## *** Multivariate linear model in R (no missing value)

## chunk 44
e.gls <- gls(visual ~ week*treatment, data = dL.pp,
         correlation = corSymm(form=~1|subject),
         weight = varIdent(form=~1|treatment*week))

## chunk 45
coef(e.gls$modelStruct$corStruct, unconstrained=FALSE)

## chunk 46
sigma(e.lme)^22
coef(e.gls$modelStruct$varStruct, unconstrained=FALSE)

## *** Multivariate linear model in R (no missing value)
## *** Design: longitudinal study
## *** Design: cross-over study
