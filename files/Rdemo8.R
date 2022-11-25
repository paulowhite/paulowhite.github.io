## * Packages
library(LMMstar)
library(nlmeU)
library(ggplot2)
library(mice)

## * Data
data(armd.wide, package = "nlmeU")

## only keep the necessary columns
keep.col <- c("subject","treat.f",
              "visual0","visual4","visual12","visual24","visual52")
dfW <- armd.wide[,keep.col,drop=FALSE]

## moving from long to wide format
dfL <- reshape(dfW, direction = "long", idvar = "subject",
               varying = paste0("visual",c(0,4,12,24,52)),
               timevar = "week", v.names = "visual")
dfL <- dfL[order(dfL$subject),] ## re-order dataset by subject
rownames(dfL) <- NULL ## remove row names
dfL$time <- factor(dfL$week, levels = 1:5, ## character version of the time variable
                   labels = paste0("week ",c(0,4,12,24,52)))
dfL$week <- as.numeric(gsub("week ", "",as.character(dfL$time)))

## complete case
dfW.CC <- dfW[rowSums(is.na(dfW))==0,]
dfL.CC <- dfL[dfL$subject %in% dfW.CC$subject,]


## * Illustrative example

## ** descriptive statistics
dfS <- summarize(visual ~ time + treat.f | subject, na.rm = TRUE,
                 data = dfL)
dfS

md.pattern(dfW, plot = FALSE)

## ** graphical display

## missing values
md.pattern(dfW, plot = TRUE)

## boxplot
gg.box <- ggplot(dfL, aes(x = time, y = visual, fill = treat.f))
gg.box <- gg.box + geom_boxplot()
gg.box <- gg.box + labs(x = "Treatment", fill = "Treatment group")
gg.box

## spaghetti plot
gg.spa <- ggplot(dfL, aes(x = time, y = visual,
                          group = subject, color = treat.f))
gg.spa <- gg.spa + geom_point() + geom_line()
gg.spa <- gg.spa + labs(x = "", color = "Treatment group")
gg.spa

## mean plot
gg.mean <- ggplot(dfS, aes(x = time, y = mean,
                           group = treat.f, color = treat.f))
gg.mean <- gg.mean + geom_point() + geom_line()
gg.mean <- gg.mean + labs(y = "visual", x = "", color = "Treatment group")
gg.mean

## spaghetti + mean plot
gg.spa2 <- ggplot(mapping = aes(x = time, color = treat.f))
gg.spa2 <- gg.spa2 + geom_line(data = dfL, alpha = 0.3,
                               aes(y = visual, group=subject))
gg.spa2 <- gg.spa2 + geom_point(data = dfS, aes(y = mean), size = 3)
gg.spa2 <- gg.spa2 + geom_line(data = dfS, aes(y = mean, group = treat.f), size = 1.5)
gg.spa2 <- gg.spa2 + labs(x = "", color = "Treatment group")
gg.spa2

## missing values
gg.NA <- ggplot(dfS, aes(x = time, y = missing/(observed+missing), color = treat.f, group = treat.f))
gg.NA <- gg.NA + geom_point(size = 6) + geom_line(size = 2)
gg.NA <- gg.NA + labs(x = NULL, y = "Percentage of missing data", color = "Treatment group") + scale_y_continuous(labels = scales::percent)
gg.NA <- gg.NA  + theme(text = element_text(size=25),
                        axis.line = element_line(size = 1.25),
                        axis.ticks = element_line(size = 2),
                        axis.ticks.length=unit(.25, "cm"))
gg.NA

## * Univariate approach

## step 1: compute the change
dfW.CC$change <- dfW.CC$visual52 - dfW.CC$visual0

## step 2: visualize the change
gg.hist <- ggplot(dfW.CC, aes(change, fill = treat.f))
gg.hist <- gg.hist + geom_histogram(alpha = 0.45, position = "identity")
gg.hist <- gg.hist + labs(x = "\u0394 Y (52)", fill="Treatment group", ylab = "number of patients")
gg.hist

gg.dens <- ggplot(dfW.CC, aes(change, fill = treat.f))
gg.dens <- gg.dens + geom_histogram(alpha = 0.45, aes(y=..density..), position = "identity")
gg.dens <- gg.dens + labs(x = "\u0394 Y (52)", fill="Treatment group")
gg.dens

## step 3: compare the two groups
t.test(change~treat.f, data = dfW.CC)
   
## equivalence with a mixed model
dfL52.CC <- dfL.CC[dfL.CC$time %in% c("week 0","week 52"),]
dfL52.CC$time <- droplevels(dfL52.CC$time)

e52.CC.lmm <- lmm(visual ~ time*treat.f,
                  repetition = ~time|subject,
                  structure = UN(~treat.f),                  
                  data = dfL52.CC,
                  control = list(optimizer = "FS"))
summary(e52.CC.lmm)


## * Multivariate approach

## ** Unstructured model
## restrict to first and last timepoint
dfL52 <- dfL[dfL$time %in% c("week 0","week 52"),]
dfL52$time <- gsub("week ","",droplevels(dfL52$time)) ## shorten names

## fit mixed model
e52.lmm <- lmm(visual ~ treat.f*time, ## mean structure
               repetition = ~ time | subject,
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
eLin.lmm <- lmm(visual ~ 0 + time + week:treat.f, ## mean structure
                repetition = ~ time | subject,
                structure = "UN", ## variance/correlation structure
                data = dfL)

## unique combination of covariates
grid <- unique(dfL[,c("time","week","treat.f")])
grid

## predicted values
gridA <- predict(eLin.lmm, newdata = grid, keep.newdata = TRUE)

## graphical display
ggLin <- ggplot(gridA, aes(x = time, y = estimate, group = treat.f, color = treat.f))
ggLin <- ggLin + geom_point() + geom_line()
ggLin

ggLin2 <- ggplot(gridA, aes(x = week, y = estimate, group = treat.f, color = treat.f))
ggLin2 <- ggLin2 + geom_point() + geom_line()
ggLin2
