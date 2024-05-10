## * Packages
library(LMMstar)
library(nlmeU)
library(ggplot2)

## * Shortcut
all.week <- c(0,4,12,24,52)
col.visual <- paste0("visual",all.week)
col.keep <- c("subject","treat.f",col.visual)

## * Load and process data
data(armd.wide, package = "nlmeU")
dfW <- armd.wide[,col.keep,drop=FALSE]
head(dfW)

dfL <- reshape(dfW, direction = "long", 
               varying = col.visual, times = all.week,
               timevar = "week.num", v.names = "visual")
head(dfL)

## re-order dataset by subject
dfL <- dfL[order(dfL$subject),] 
## categorical time variable
dfL$week <- factor(paste0("week ",dfL$week),
                   levels = paste0("week ",all.week))


## * Illustrative example


## ** descriptive statistics
## mean, sd, correlation
dfS <- summarize(visual ~ week + treat.f | subject, na.rm = TRUE,
                 data = dfL)
dfS

##  missing values patterns
dfNA <- summarizeNA(dfW)
dfNA

plot(dfNA)
plot(dfNA, order.pattern = "n.missing")
plot(dfNA, order.pattern = "frequency")

## ** spaghetti plot
gg.spa <- ggplot(dfL, aes(x = week, y = visual,
                          group = subject, color = treat.f))
gg.spa <- gg.spa + geom_point() + geom_line()
gg.spa <- gg.spa + labs(color = "Treatment group")
gg.spa

gg.spa2 <- ggplot(dfL, aes(x = week.num, y = visual,
                          group = subject, color = treat.f))
gg.spa2 <- gg.spa2 + geom_point() + geom_line()
gg.spa2 <- gg.spa2 + labs(color = "Treatment group")
gg.spa2

dfL.1_5 <- dfL[dfL$subject %in% 1:9,]
gg.spaId <- ggplot(dfL.1_5, aes(x = week, y = visual, group = subject))
gg.spaId <- gg.spaId + geom_point() + geom_line()
gg.spaId <- gg.spaId + facet_wrap(~subject, labeller = label_both)
gg.spaId

## ** boxplot
gg.box <- ggplot(dfL, aes(x = week, y = visual, fill = treat.f))
gg.box <- gg.box + geom_boxplot()
gg.box <- gg.box + labs(x = "Treatment", fill = "Treatment group")
gg.box

## ** mean plot
## 'manually'
gg.mean <- ggplot(dfS, aes(x = week, y = mean,
                           group = treat.f, color = treat.f))
gg.mean <- gg.mean + geom_point(size = 4) + geom_line(linewidth = 1.5)
gg.mean <- gg.mean + labs(y = "visual", x = "", color = "Treatment group")
gg.mean

## directly from the output of summarize
plot(dfS)
plot(dfS, type = "sd")

## ** spaghetti + mean plot
gg.spa2 <- ggplot(mapping = aes(x = week, color = treat.f))
gg.spa2 <- gg.spa2 + geom_line(data = dfL, alpha = 0.3,
                               aes(y = visual, group=subject))
gg.spa2 <- gg.spa2 + geom_point(data = dfS, aes(y = mean), size = 3)
gg.spa2 <- gg.spa2 + geom_line(data = dfS, aes(y = mean, group = treat.f), linewidth = 1.5)
gg.spa2 <- gg.spa2 + labs(x = "", color = "Treatment group")
gg.spa2

## ** percentage of missing values
## 'manually'
dfS$pc.missing <- dfS$missing/(dfS$missing+dfS$observed)

gg.pcNA <- ggplot(dfS, aes(x=week,y=pc.missing,group=treat.f,color=treat.f))
gg.pcNA <- gg.pcNA + geom_point(size = 4) + geom_line(linewidth = 1.5)
gg.pcNA <- gg.pcNA + labs(y = "Percentage of missing data", color = "Treatment group") + scale_y_continuous(labels = scales::percent)
gg.pcNA

## directly from the output of summarizeNA
plot(dfS, type = "pc.missing")

## * Univariate approach

## ** data management for complete case analysis
dfW.CC <- dfW[rowSums(is.na(dfW))==0,]
dfL.CC <- dfL[dfL$subject %in% dfW.CC$subject,]
head(dfW.CC)

## ** step 1: compute the change
dfW.CC$change52 <- dfW.CC$visual52 - dfW.CC$visual0

## ** step 2: visualize the change
gg.hist <- ggplot(dfW.CC, aes(change52, fill = treat.f))
gg.hist <- gg.hist + geom_histogram(alpha = 0.45, position = "identity")
gg.hist <- gg.hist + labs(x = "\u0394 Y (52)", fill="Treatment group", ylab = "number of patients")
gg.hist


## ** step 3: compare the two groups
t.test(change52~treat.f, data = dfW.CC)

## equivalent analysis
ttest.lmm <- lmm(change52~treat.f, structure = IND(~treat.f), data = dfW.CC)
summary(ttest.lmm)

## alternative analyses (ANCOVA)
ANCOVA.lm <- lm(visual52~visual0+treat.f, data = dfW.CC)
summary(ANCOVA.lm)
ANCOVA.lmm <- lmm(visual52~visual0+treat.f, structure = IND(~treat.f), data = dfW.CC)
summary(ANCOVA.lmm)
   
## equivalence with a mixed model
dfL52.CC <- dfL.CC[dfL.CC$week %in% c("week 0","week 52"),]
dfL52.CC$week <- droplevels(dfL52.CC$week)

e52.CC.lmm <- lmm(visual ~ week*treat.f,
                  repetition = ~week|subject,
                  structure = UN(~treat.f),                  
                  data = dfL52.CC)
summary(e52.CC.lmm)


## ** multiple t-tests
dfW.CC$change4 <- dfW.CC$visual4 - dfW.CC$visual0
dfW.CC$change12 <- dfW.CC$visual12 - dfW.CC$visual0
dfW.CC$change24 <- dfW.CC$visual24 - dfW.CC$visual0

ttest.mlmm_Adj <- mt.test(change4+change12+change24+change52 ~ treat.f, data = dfW.CC)
ttest.mlmm_Adj

ttest.mlmm_noAdj <- mt.test(change4+change12+change24+change52 ~ treat.f, data = dfW.CC,
                            method = "none")
ttest.mlmm_noAdj


## * Multivariate approach

## ** Unstructured model
## restrict to first and last timepoint
dfL52 <- dfL[dfL$week.num %in% c(0,52),]
dfL52$week <- factor(dfL52$week.num, levels = c(0,52))

## fit mixed model
e52.lmm <- lmm(visual ~ treat.f*week, ## mean structure
               repetition = ~ week | subject,
               structure = "UN", ## variance/correlation structure
               data = dfL52)

## display estimates
coef(e52.lmm)
model.tables(e52.lmm)
effects(e52.lmm, variable = "treat.f")
effects(e52.lmm, type = "difference", variable = "treat.f")

coef(e52.lmm, effects = c("variance","correlation"))

## display fitted values
plot(e52.lmm)
## display fitted values with observations
plot(e52.lmm, ci = FALSE, obs.alpha = 0.1)

## ** Treatment proportional to duration
dfL$week.f <- as.factor(dfL$week.num)

eLin.lmm <- lmm(visual ~ 0 + week + week.num:treat.f,
                repetition = ~ week | subject,
                structure = "UN",
                data = dfL)

model.tables(eLin.lmm)

plot(eLin.lmm)
plot(eLin.lmm, obs.alpha = 0.1, ci = FALSE)



