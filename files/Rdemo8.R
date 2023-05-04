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

dfL <- reshape(dfW, direction = "long", 
               varying = col.visual, times = all.week,
               timevar = "week", v.names = "visual")

## re-order dataset by subject
dfL <- dfL[order(dfL$subject),] 
## categorical time variable
dfL$week.f <- factor(paste0("week ",dfL$week),
                     levels = paste0("week ",all.week))


## * Illustrative example

## ** spaghetti plot
gg.spa <- ggplot(dfL, aes(x = week.f, y = visual,
                          group = subject, color = treat.f))
gg.spa <- gg.spa + geom_point() + geom_line()
gg.spa <- gg.spa + labs(color = "Treatment group")
gg.spa

gg.spa2 <- ggplot(dfL, aes(x = week, y = visual,
                          group = subject, color = treat.f))
gg.spa2 <- gg.spa2 + geom_point() + geom_line()
gg.spa2 <- gg.spa2 + labs(color = "Treatment group")
gg.spa2

## ** descriptive statistics
dfS <- summarize(visual ~ week.f + treat.f | subject, na.rm = TRUE,
                 data = dfL)
dfS

## * Univariate approach

## ** data management for complete case analysis
dfW.CC <- dfW[rowSums(is.na(dfW))==0,]
dfL.CC <- dfL[dfL$subject %in% dfW.CC$subject,]
head(dfW.CC)

## ** step 1: compute the change
dfW.CC$change <- dfW.CC$visual52 - dfW.CC$visual0

## ** step 2: visualize the change
gg.hist <- ggplot(dfW.CC, aes(change, fill = treat.f))
gg.hist <- gg.hist + geom_histogram(alpha = 0.45, position = "identity")
gg.hist <- gg.hist + labs(x = "\u0394 Y (52)", fill="Treatment group", ylab = "number of patients")
gg.hist


## ** step 3: compare the two groups
t.test(change~treat.f, data = dfW.CC)
   
## equivalence with a mixed model
dfL52.CC <- dfL.CC[dfL.CC$week.f %in% c("week 0","week 52"),]
dfL52.CC$week.f <- droplevels(dfL52.CC$week.f)

e52.CC.lmm <- lmm(visual ~ week.f*treat.f,
                  repetition = ~week.f|subject,
                  structure = UN(~treat.f),                  
                  data = dfL52.CC)
summary(e52.CC.lmm)


## * Multivariate approach

## ** Unstructured model
## restrict to first and last timepoint
dfL52 <- dfL[dfL$week %in% c(0,52),]
dfL52$week.f <- factor(dfL52$week, levels = c(0,52))

## fit mixed model
e52.lmm <- lmm(visual ~ treat.f*week.f, ## mean structure
               repetition = ~ week.f | subject,
               structure = "UN", ## variance/correlation structure
               data = dfL52)

## display estimates
coef(e52.lmm)
model.tables(e52.lmm)
dummy.coef(e52.lmm)

coef(e52.lmm, effects = c("variance","correlation"))

## display fitted values
plot(e52.lmm)
## display fitted values with observations
plot(e52.lmm, ci = FALSE, obs.alpha = 0.1)

## ** Treatment proportional to duration
dfL2 <- dfL ## shorten name
dfL2$week.f <- factor(gsub("week ","",dfL$week.f), all.week)

eLin.lmm <- lmm(visual ~ 0 + week.f + week:treat.f,
                repetition = ~ week | subject,
                structure = "UN",
                data = dfL2)

model.tables(eLin.lmm)

plot(eLin.lmm)
plot(eLin.lmm, obs.alpha = 0.1, ci = FALSE)

## * Extra material

## ** missing values patterns
dfNA <- summarizeNA(dfW)
dfNA

plot(dfNA)
plot(dfNA, order.pattern = "n.missing")
plot(dfNA, order.pattern = "frequency")


dfS$pc.missing <- dfS$missing/(dfS$missing+dfS$observed)

gg.pcNA <- ggplot(dfS, aes(x=week.f,y=pc.missing,group=treat.f,color=treat.f))
gg.pcNA <- gg.pcNA + geom_point(size = 4) + geom_line(linewidth = 1.5)
gg.pcNA <- gg.pcNA + labs(y = "Percentage of missing data", color = "Treatment group") + scale_y_continuous(labels = scales::percent)
gg.pcNA

## ** boxplot
gg.box <- ggplot(dfL, aes(x = week.f, y = visual, fill = treat.f))
gg.box <- gg.box + geom_boxplot()
gg.box <- gg.box + labs(x = "Treatment", fill = "Treatment group")
gg.box

## ** mean plot
gg.mean <- ggplot(dfS, aes(x = week.f, y = mean,
                           group = treat.f, color = treat.f))
gg.mean <- gg.mean + geom_point(size = 4) + geom_line(linewidth = 1.5)
gg.mean <- gg.mean + labs(y = "visual", x = "", color = "Treatment group")
gg.mean

## ** spaghetti + mean plot
gg.spa2 <- ggplot(mapping = aes(x = week.f, color = treat.f))
gg.spa2 <- gg.spa2 + geom_line(data = dfL, alpha = 0.3,
                               aes(y = visual, group=subject))
gg.spa2 <- gg.spa2 + geom_point(data = dfS, aes(y = mean), size = 3)
gg.spa2 <- gg.spa2 + geom_line(data = dfS, aes(y = mean, group = treat.f), linewidth = 1.5)
gg.spa2 <- gg.spa2 + labs(x = "", color = "Treatment group")
gg.spa2

