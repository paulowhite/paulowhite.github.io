### exercise-repMes.R --- 
##----------------------------------------------------------------------
## Author: Brice Ozenne
## Created: nov 27 2022 (23:10) 
## Version: 
## Last-Updated: maj 10 2024 (12:54) 
##           By: Brice Ozenne
##     Update #: 13
##----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
##----------------------------------------------------------------------
## 
### Code:

library(LMMstar)
library(ggplot2)
library(nlmeU)

## * load data --------------------------
data(armd.wide, package = "nlmeU")

## reshape
armd.long <- reshape(armd.wide, direction = "long",
                     varying = paste0("visual",c(0,4,12,24,52)),
                     times = c(0,4,12,24,52),
                     timevar = "week.num",
                     v.names = "visual")
armd.long$week <- as.factor(armd.long$week)

## * Part 1: descriptive statistics --------------------------

## ** question 1
str(armd.wide)
table(armd.wide$miss.pat)
table(armd.wide$treat.f)

table(armd.long$week, armd.long$treat.f)

## ** question 2
armd.s <- summarize(visual ~ week + treat.f, na.rm = TRUE,
                    data = armd.long)
armd.s


summarize(visual ~ week, na.rm = TRUE,
          data = armd.long)

armd.s2 <- summarize(visual ~ week + treat.f|subject, na.rm = TRUE,
                     data = armd.long)
armd.s2
cor(armd.wide[armd.wide$treat.f=="Active","visual0"],
    armd.wide[armd.wide$treat.f=="Active","visual4"],
    use = "pairwise")

armd.s3 <- summarize(visual ~ week|subject, na.rm = TRUE,
                     data = armd.long)
cor(armd.wide$visual0,armd.wide$visual4, use = "pairwise")

## ** question 3
gg.box <- ggplot(armd.long, aes(x = week, y = visual, fill = treat.f))
gg.box <- gg.box + geom_boxplot()
gg.box <- gg.box + labs(x = "week", fill = "Treatment group")
gg.box

gg.spa <- ggplot(armd.long, aes(x = week, y = visual,
                          group = subject, color = treat.f))
gg.spa <- gg.spa + geom_point() + geom_line()
gg.spa <- gg.spa + labs(x = "week", color = "Treatment group")
gg.spa

gg.spa2 <- ggplot(armd.long, aes(x = as.numeric(as.character(week)), y = visual,
                          group = subject, color = treat.f))
gg.spa2 <- gg.spa2 + geom_point() + geom_line()
gg.spa2 <- gg.spa2 + labs(x = "week", color = "Treatment group")
gg.spa2

gg.mean <- ggplot(armd.s, aes(x = week, y = mean,
                             group = treat.f, color = treat.f))
gg.mean <- gg.mean + geom_point() + geom_line()
gg.mean <- gg.mean + labs(y = "visual", x = "week", color = "Treatment group")
gg.mean

gg.spa2 <- ggplot(mapping = aes(x = week, color = treat.f))
gg.spa2 <- gg.spa2 + geom_line(data = armd.long, alpha = 0.3,
                               aes(y = visual, group=subject))
gg.spa2 <- gg.spa2 + geom_point(data = armd.s, aes(y = mean), size = 3)
gg.spa2 <- gg.spa2 + geom_line(data = armd.s, aes(y = mean, group = treat.f), size = 1.5)
gg.spa2 <- gg.spa2 + labs(x = "", color = "Treatment group")
gg.spa2

plot(armd.s2, type = "mean")
plot(armd.s2, type = "sd")
plot(armd.s2, type = "cor")

## question 4
## left panel
gg.NA <- ggplot(armd.s , aes(x = week, y = missing/(observed+missing),
                             color = treat.f, group = treat.f))
gg.NA <- gg.NA + geom_point(size = 6) + geom_line(size = 2)
gg.NA <- gg.NA + scale_y_continuous(labels = scales::percent)
gg.NA

plot(armd.s2, type = "pc.missing")

## right panel
armd.visual <- armd.wide[,paste0("visual",c(0,4,12,24,52))]
plot(summarizeNA(armd.visual))

## * Part 2: univariate approach --------------------------

## question 5
test <- is.na(armd.wide$visual0)+is.na(armd.wide$visual52)
armd.wideCC <- armd.wide[test==0,]
armd.wideCC$change52 <- armd.wideCC$visual52 - armd.wideCC$visual0

hist(armd.wideCC$change52, breaks = seq(-60,30,by=5))
hist(armd.wideCC$change52)

## question 6
e.tt <- t.test(change52 ~ treat.f, data = armd.wideCC)
e.tt
# mean in group Placebo  mean in group Active 
# -11.18095             -15.47778 

e.tt$estimate[2] - e.tt$estimate[1]
diff(e.tt$estimate)
diff(armd.s[armd.s$week==52,"mean"] - armd.s[armd.s$week==0,"mean"])

armd.wide[1,] ## excluded individual

mean52CC <- summarize(visual52 ~ treat.f, data = armd.wideCC)$mean
mean0CC <- summarize(visual0 ~ treat.f, data = armd.wideCC)$mean
diff(mean52CC - mean0CC)

## question 7
boxplot(change52 ~ treat.f, data = armd.wideCC)

e.lm <- lm(change52 ~ treat.f, data = armd.wideCC)
summary(e.lm)$coef

e.lmm <- lmm(change52 ~ treat.f, data = armd.wideCC, structure = IND(~treat.f))
summary(e.lmm)
summary(anova(e.lmm, effects = "variance"))

## question 8
armd.wideCC$change4 <- armd.wideCC$visual4 - armd.wideCC$visual0
armd.wideCC$change12 <- armd.wideCC$visual12 - armd.wideCC$visual0
armd.wideCC$change24 <- armd.wideCC$visual24 - armd.wideCC$visual0

ttest.mlmm_noAdj <- mt.test(change4+change12+change24+change52 ~ treat.f,
                            data = armd.wideCC,
                            method = "none")

ttest.mlmm <- mt.test(change4+change12+change24+change52 ~ treat.f,
                            data = armd.wideCC)

## * Part 3: multivariate approach --------------------------
armd.long52 <- armd.long[armd.long$week %in% c("0","52"),]
armd.long52$week <- droplevels(armd.long52$week)

## Question 9
armd.long52CC <- armd.long52[armd.long52$subject %in% armd.wideCC$subject,]

e052.lmm <- lmm(visual ~ treat.f*week,
                repetition = ~week|subject,
                data = armd.long52CC)
summary(e052.lmm)
plot(e052.lmm)
model.tables(e052.lmm)

c(placebo.0 = as.double(coef(e052.lmm)["(Intercept)"]),
  placebo.52 = sum(coef(e052.lmm)[c("(Intercept)","week52")]),
  active.0 = sum(coef(e052.lmm)[c("(Intercept)","treat.fActive")]),
  active.52 = sum(coef(e052.lmm)))

effects(e052.lmm, variable = "treat.f")

## Question 10
e52.lmm <- lmm(visual ~ treat.f*week,
               repetition = ~week|subject,
               data = armd.long52)
summary(e52.lmm)

e.lmm <- lmm(visual ~ treat.f*week,
             repetition = ~week|subject,
             data = armd.long)
summary(e.lmm)
plot(e.lmm)
anova(e.lmm)

## Question 11
armd.long$week.num <- as.numeric(as.character(armd.long$week))
eLin.lmm <- lmm(visual ~ week + week.num:treat.f,
                repetition = ~ week | subject, structure = "UN",
                data = armd.long)
model.tables(eLin.lmm)

52*coef(eLin.lmm)["week.num:treat.fActive"]
effects(eLin.lmm, variable = "treat.f", type = "difference")

grid <- unique(armd.long[,c("week","week.num","treat.f")],)
gridA <- predict(eLin.lmm, newdata = grid, keep.data = TRUE)
gridB <- predict(e.lmm, newdata = grid, keep.data = TRUE)

gg.fit <- ggplot(mapping = aes(x = week.num, y = estimate,
                               color = treat.f, group = treat.f))
gg.fit <- gg.fit + geom_point(data = gridA, aes(shape = "linear"),
                              size = 2)
gg.fit <- gg.fit + geom_line(data = gridA, aes(linetype = "linear"),
                             size = 1)
gg.fit <- gg.fit + geom_point(data = gridB, aes(shape = "non-linear"),
                              size = 2)
gg.fit <- gg.fit + geom_line(data = gridB, aes(linetype = "non-linear"),
                             size = 1)
gg.fit <- gg.fit + labs(x = "Time (in weeks)", y = "Vision",
                        shape = "Treatment effect model",
                        linetype = "Treatment effect model",
                        color = "Treatment group")
gg.fit

##----------------------------------------------------------------------
### exercise-repMes.R ends here
