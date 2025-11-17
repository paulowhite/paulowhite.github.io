rm(list=ls())

p1 <- 0.17
p2 <- 0.11
power <- 0.8
alpha <- 0.05

# Question 1

# sample size per arm (round up)
the.n <- ceiling(power.prop.test(p1=p1,
                                 p2=p2,
                                 sig.level=alpha,
                                 power=power)$n)
the.n
# total sample size
2*the.n

# Question 2

# power for n=650/2
round(power.prop.test(p1=p1,
                p2=p2,
                sig.level=alpha,
                n=round(650/2))$power,2)
# Question 3

# power for n=850/2
round(power.prop.test(p1=p1,
                p2=p2,
                sig.level=alpha,
                n=round(850/2))$power,2)
# Question 4

# power for n=1048/2 with 17% vs 13% instead of 17% vs 11%
round(power.prop.test(p1=p1,
                p2=0.13,
                sig.level=alpha,
                n=1048/2)$power,2)

# Question 5

# smaller risk reduction to have at least 75% power
# First, let's run this wrong code...
power.prop.test(p1=p1,
                sig.level=alpha,
                n=1048/2,
                power=0.75)$p2
# It does not work !! Because helppage says "If one of ‘p1’ and ‘p2’ is
## computed, then p1 < p2 is assumed and will hold, but if you
## specify both, p2 <= p1 is allowed."
round(power.prop.test(p2=p1,
                      sig.level=alpha,
                      n=1048/2,
                      power=0.75)$p1,3)
# note: for communication purpose, it might be OK to round 11.3% as 11.5%
# (discussing whole percentages or whole percentages plus a half percent might be sufficient)

# Question 6
0.4*0.3 + 0.6*0.075
0.4*0.2 + 0.6*0.05

# Question 7

# advantage: sample size could be smaller
the.n.SS <- ceiling(power.prop.test(p1=0.3,
                                    p2=0.2,
                                    sig.level=alpha,
                                    power=power)$n)
2*the.n.SS

# disadvantage: trial duration
# duration of the trial if we include all (both patients in septic shock and not in septic shock)
the.n*2/130

# duration of the trial if we inluce only patients in spetic shock
the.n.SS*2/(0.4*130)

the.n.SS/the.n # 0.56% of the total sample size, not 40% ... patients in not in septic shock
# are not as "informative" as those in septic shock to show treatment efficacy,
# but they help. Including them is better than not including them.
