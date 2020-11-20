## * Warming up

## chunk 1
dW.pp <- read.table("prepost.txt")
dL.pp <- reshape2::melt(dW.pp, 
                        id.var = c("subject","treatment","lesion"),
                        measure.vars = c("visual0","visual4"),
                        variable.name = "week", 
                        value.name = "visual")
dL.pp <- dL.pp[order(dL.pp$subject),]
## convert week as factor with appropriate values
dL.pp$week <- factor(dL.pp$week, 
                     level = c("visual0","visual4"), 
                     labels = c(0,4))
## remove row names
rownames(dL.pp) <- NULL

## chunk 2
vec.mean <- c(mean(dW.pp[dW.pp$treatment=="Active","visual0"]),
              mean(dW.pp[dW.pp$treatment=="Active","visual4"]),
              mean(dW.pp[dW.pp$treatment=="Placebo","visual0"]),
              mean(dW.pp[dW.pp$treatment=="Placebo","visual4"]))
vec.mean

## chunk 3
vec.treatment <- c("Active","Active","Placebo","Placebo")
vec.week <- c("0","4","0","4")
dftable <- data.frame(treatment = vec.treatment,
                      week = vec.week,
                      mu = vec.mean)
dftable

## chunk 4
aggregate(visual ~ week + treatment,
          data = dL.pp, FUN = "mean")

## chunk 5
library(data.table)
dtL.pp <- as.data.table(dL.pp)
dtL.pp[,list(mu=mean(visual), sigma=sd(visual)), 
                 by = c("treatment","week")]

## chunk 6
dW.pp$visualDiff04 <- dW.pp$visual4 - dW.pp$visual0
c(mean(dW.pp[dW.pp$treatment=="Active","visualDiff04"]),
  mean(dW.pp[dW.pp$treatment=="Placebo","visualDiff04"]))

## chunk 7
mu.active <- dftable[dftable$treatment=="Active","mu"]
mu.placebo <- dftable[dftable$treatment=="Placebo","mu"]

## chunk 8
change <- c(Placebo = diff(mu.active),
            Active = diff(mu.placebo))
change

## chunk 9
as.double(change["Active"]-change["Placebo"])

## chunk 10
dW.ppA <- dW.pp[dW.pp$treatment=="Active",]
dW.ppP <- dW.pp[dW.pp$treatment=="Placebo",]

## chunk 11
dW.pp$treatment <- relevel(factor(dW.pp$treatment),"Placebo")

## chunk 12
library(nlme)
e.t.test <- t.test(x=dW.ppP$visualDiff04, y=dW.ppA$visualDiff04)
e.lm <- lm(visualDiff04 ~ treatment, data = dW.pp)
e.gls <- gls(visualDiff04 ~ treatment, data = dW.pp,
             weights = varIdent(form=~1|treatment))

## chunk 13
c("t-test" = as.double(diff(e.t.test$estimate)),
  "lm" = as.double(coef(e.lm)["treatmentActive"]),
  "gls" = as.double(coef(e.gls)["treatmentActive"]))

## chunk 14
c("t-test" = as.double(e.t.test$stderr),
  "lm" = sqrt(vcov(e.lm)["treatmentActive","treatmentActive"]),
  "gls" = sqrt(vcov(e.gls)["treatmentActive","treatmentActive"]))

## * Exercise A: Longitudinal study

## chunk 15
head(armdW)

## chunk 16
head(is.na(armdW))

## chunk 17
colSums(is.na(armdW))

## chunk 18
rowSums(is.na(armdW))[1:5]

## chunk 19
vec.visual <- c("visual0", "visual4", "visual12",
                "visual24", "visual52")
plot(armdW[,vec.visual], col = armdW$treat.f)

## chunk 20
plot(armdW[,vec.visual], col = armdW$treat.f)

## chunk 21
library(ggplot2)
gg <- ggplot(armdL, aes(x = week, y = visual, 
                        group = subject, color = treat.f))
gg <- gg + geom_point() + geom_line()
gg

## chunk 22
print(gg + theme(text = element_text(size=20)))

## chunk 23
gg2 <- gg + facet_grid(treat.f~lesion, labeller = label_both)
gg2

## chunk 24
print(gg2 + theme(text = element_text(size=20)))

## chunk 25
gg <- ggplot(armdL, aes(x = week, y = visual, 
                        fill = treat.f))
gg <- gg + geom_boxplot()
gg

## chunk 26
print(gg + theme(text = element_text(size=20)))

## chunk 27
armdW.p <- armdW[armdW$treat.f=="Placebo",]
armdW.a <- armdW[armdW$treat.f=="Active",]

## chunk 28
mean(armdW.a$visual0)

## chunk 29
library(data.table)
dtL <- as.data.table(armdL)
dtL[,.(n = length(visual), 
       missing = sum(is.na(visual)), 
       observed = sum(!is.na(visual)), 
       mean = mean(visual, na.rm=TRUE),
       std = sd(visual, na.rm=TRUE),
       min = min(visual, na.rm=TRUE),
       max = max(visual, na.rm=TRUE)), 
    by = c("week","treat.f")]

## chunk 30
rho <- cor(armdW[,c("visual0","visual4","visual12",
                    "visual24","visual52")], use = "complete")
rho

## chunk 31
position <- which(lower.tri(rho), arr.ind =TRUE)
position[] <- c(0,4,12,24,52)[as.double(position)]
df.rho <- data.frame(interval = position[,1] - position[,2],
                     correlation = rho[lower.tri(rho)])
gg <- ggplot(df.rho, aes(x = interval, y = correlation))
gg <- gg + geom_point() + geom_smooth(method = "lm")
gg <- gg + xlab("time interval")
gg

## chunk 32
print(gg + theme(text = element_text(size=20)))

## chunk 33
armdW.p$change4 <- armdW.p$visual4 - armdW.p$visual0 
armdW.p$change12 <- armdW.p$visual12 - armdW.p$visual0 
armdW.p$change24 <- armdW.p$visual24 - armdW.p$visual0 
armdW.p$change52 <- armdW.p$visual52 - armdW.p$visual0 

armdW.a$change4 <- armdW.a$visual4 - armdW.a$visual0 
armdW.a$change12 <- armdW.a$visual12 - armdW.a$visual0 
armdW.a$change24 <- armdW.a$visual24 - armdW.a$visual0 
armdW.a$change52 <- armdW.a$visual52 - armdW.a$visual0 

## chunk 34
ttest4 <- t.test(armdW.a$change4,armdW.p$change4)
ttest12 <- t.test(armdW.a$change12,armdW.p$change12)
ttest24 <- t.test(armdW.a$change24,armdW.p$change24)
ttest52 <- t.test(armdW.a$change52,armdW.p$change52)

## chunk 35
ttest52

## chunk 36
c(week4 = as.double(diff(ttest4$estimate)), 
  week12 = as.double(diff(ttest12$estimate)), 
  week24 = as.double(diff(ttest24$estimate)), 
  week52 = as.double(diff(ttest52$estimate)))

## chunk 37
p.adjust(c(ttest4$p.value, ttest12$p.value, 
           ttest24$p.value, ttest52$p.value),
         method = "bonferroni")

## chunk 38
X$fit.linear <- predict(e.linear, newdata = X)
X$fit.np <- predict(e.np, newdata = X)

## chunk 39
gg <- ggplot(X, aes(x = week.num, group = treat.f, color = treat.f))
gg <- gg + geom_line(aes(y = fit.linear, linetype = "linear"))
gg <- gg + geom_line(aes(y = fit.np, linetype = "non-parametric"))
gg <- gg + geom_point(aes(y = fit.linear, shape = "linear"))
gg <- gg + geom_point(aes(y = fit.np, shape = "non-parametric"))
gg <- gg + xlab("time from inclusion (in weeks)")
gg <- gg + ylab("expected vision")
gg <- gg + labs(shape = "model", linetype = "model")
gg

## chunk 40
print(gg + theme(text = element_text(size=20)))

## chunk 41
anova(e.linear,e.np)

## chunk 42
summary(e.linear)$coef

## chunk 43
gg <- ggplot(armdL.cc, aes(x = week, y = residuals, fill = treat.f))
gg <- gg + geom_boxplot()
gg

## chunk 44
print(gg + theme(text = element_text(size=20)))

## chunk 45
cor(armaW2.cc[,c("0","4","12","24","52")], use = "complete")

## chunk 46
X$fit.lme <- predict(e.lme, newdata = X, level = 0)

gg <- ggplot(X, aes(x = week.num, group = treat.f, color = treat.f))
gg <- gg + geom_line(aes(y = fit.lme))
gg <- gg + geom_point(aes(y = fit.lme))
gg <- gg + xlab("time from inclusion (in weeks)") + ylab("expected vision")
gg

## chunk 47
summary(e.lme)$tTable

## chunk 48
X$fit.gls <- predict(e.gls, newdata = X)

gg <- ggplot(X, aes(x = week.num, group = treat.f, color = treat.f))
gg <- gg + geom_line(aes(y = fit.lme, linetype = "random intercept"))
gg <- gg + geom_line(aes(y = fit.gls, linetype = "unstructured"))
gg <- gg + geom_point(aes(y = fit.lme, shape = "random intercept"))
gg <- gg + geom_point(aes(y = fit.gls, shape = "unstructured"))
gg <- gg + xlab("time from inclusion (in weeks)") + ylab("expected vision")
gg <- gg + labs(shape = "model", linetype = "model")
gg

## chunk 49
summary(e.gls)$tTable

## * Exercise B: Data visualisation

## chunk 50
c(mean(dW[,"X1"]),mean(dW[,"X2"]),mean(dW[,"X3"]))

## chunk 51
cov(dW[,c("X1","X2","X3")])

## chunk 52
mu

## chunk 53
Sigma

## chunk 54
n2 <- 20
set.seed(10)
dW2 <- data.frame(id = 1:n2, rmvnorm(n2, mean = 0:2, sigma = Sigma))
cov(dW2[,c("X1","X2","X3")])

## chunk 55
set.seed(10)
df.res <- do.call(rbind,lapply(1:100, function(i){
  dW2 <- data.frame(id = 1:n2, rmvnorm(n2, mean = 0:2, sigma = Sigma))
  Sigma.hat <- cov(dW2[,c("X1","X2","X3")])
  return(setNames(Sigma.hat[,1], c("sigma2_11","cov_12","cov_23")))
}))
boxplot(df.res, main = "sample size: 20") 
abline(h = c(sd1^2,cov,cov2), col = "red")

## chunk 56
boxplot(df.res, main = "sample size: 20") 
abline(h = c(sd1^2,cov,cov2), col = "red")

## chunk 57
n3 <- 2000
set.seed(10)
df.res <- do.call(rbind,lapply(1:100, function(i){
  dW2 <- data.frame(id = 1:n3, rmvnorm(n3, mean = 0:2, sigma = Sigma))
  Sigma.hat <- cov(dW2[,c("X1","X2","X3")])
  return(setNames(Sigma.hat[,1], c("sigma2_11","cov_12","cov_23")))
}))
boxplot(df.res, main = "sample size: 2000") 
abline(h = c(sd1^2,cov,cov2), col = "red")

## chunk 58
boxplot(df.res, main = "sample size: 2000") 
abline(h = c(sd1^2,cov,cov2), col = "red")

## chunk 59
plot(dW[,c("X1","X2","X3")])

## chunk 60
plot(dW[,c("X1","X2","X3")])

## chunk 61
boxplot(X ~ time, data = dL)
ggplot(dL, aes(x = time, y = X)) + geom_boxplot()

## chunk 62
ggplot(dL, aes(x = time, y = X)) + geom_boxplot() + theme(text = element_text(size=20))

## chunk 63
matplot(t(dW[,c("X1","X2","X3")]), type = "l")
ggplot(dL, aes(x = time, y = X, group = id, color = id)) + geom_line() + geom_point()

## chunk 64
ggplot(dL, aes(x = time, y = X, group = id, color = id)) + geom_line() + geom_point() + theme(text = element_text(size=20))

