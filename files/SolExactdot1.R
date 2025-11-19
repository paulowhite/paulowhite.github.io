rm(list=ls())

library(exact2x2)

# Question 1:
# primary outcome, main analysis
x1obs <- 0
n1obs <- 84
x2obs <- 0
n2obs <- 98


uncondExact2x2(x1=x1obs, # x1obs is the number of sequelae observed in IVT group
               n1=n1obs, # n1obs is the number of patients in IVT group
               x2=x2obs, # x2obs is the number of sequelae observed in OT group
               n2=n2obs, # n2obs is the number of patients in OT group
               parmtype="difference", # estimand is risk difference
               alternative = ("less"),# one-dided test (non-inferiority)
               method="score", # score statistic ordering is used
               conf.level = 0.975, # 97.5% confidence level
               nullparm=0.05, # non-inferiority margin is 5%
               conf.int=TRUE) # computation of confidence interval wanted

# primary outcome, per protocol analysis
x1obs <- 0
n1obs <- 76
x2obs <- 0
n2obs <- 81


uncondExact2x2(x1=x1obs, # x1obs is the number of sequelae observed in IVT group
               n1=n1obs, # n1obs is the number of patients in IVT group
               x2=x2obs, # x2obs is the number of sequelae observed in OT group
               n2=n2obs, # n2obs is the number of patients in OT group
               parmtype="difference", # estimand is risk difference
               alternative = ("less"),# one-dided test (non-inferiority)
               method="score", # score statistic ordering is used
               conf.level = 0.975, # 97.5% confidence level
               nullparm=0.05, # non-inferiority margin is 5%
               conf.int=TRUE) # computation of confidence interval wanted

# secondary outcome: switch
x1obs <- 3
n1obs <- 91
x2obs <- 5
n2obs <- 101

uncondExact2x2(x1=x1obs, # x1obs is the number of events in IVT group
               n1=n1obs, # n1obs is the number of patients in IVT group
               x2=x2obs, # x2obs is the number of events in OT group
               n2=n2obs, # n2obs is the number of patients in OT group
               parmtype="difference", # estimand is risk difference
               alternative = "two.sided", # two-sided CI wanted
               method="score", # score statistic ordering is used
               conf.level = 0.95, # 95% confidence level
               conf.int=TRUE) # computation of CI wanted

# secondary outcome: recurrence
x1obs <- 1
n1obs <- 91
x2obs <- 0
n2obs <- 101

uncondExact2x2(x1=x1obs, # x1obs is the number of events in IVT group
               n1=n1obs, # n1obs is the number of patients in IVT group
               x2=x2obs, # x2obs is the number of events in OT group
               n2=n2obs, # n2obs is the number of patients in OT group
               parmtype="difference", # estimand is risk difference
               alternative = "two.sided", # two-sided CI wanted
               method="score", # score statistic ordering is used
               conf.level = 0.95, # 95% confidence level
               conf.int=TRUE) # computation of CI wanted



# Question 2:
# Compare with standard results
library(Publish)
tabRes <- rbind(c(0,98-0),c(0,84-0))
rest2x2 <- table2x2(tabRes,stats="rd")
rest2x2

tabRes <- rbind(c(0,81-0),c(0,76-0))
rest2x2 <- table2x2(tabRes,stats="rd")
rest2x2

tabRes <- rbind(c(5,101-5),c(3,91-3))
rest2x2 <- table2x2(tabRes,stats="rd")
rest2x2

tabRes <- rbind(c(0,101-0),c(1,91-1))
rest2x2 <- table2x2(tabRes,stats="rd")
rest2x2

