rm(list=ls())

p1 <- 0.17   # expected risk in control arm
p2 <- 0.11    # expected risk in active arm
power <- 0.8  # power (1 - Type-II error) 
alpha <- 0.05 # Type-I error

# Question 1

# sample size per arm (rounded up)
the.n <- ceiling(power.prop.test(p1=p1,
                                 p2=p2,
                                 sig.level=alpha,
                                 power=power)$n)
the.n   # sample size per arm
2*the.n # total sample size

# Question 2

# power for n=650/2
round(power.prop.test(p1=p1,
                      p2=p2,
                      sig.level=alpha,
                      n=650/2)$power,2)
# Question 3

# power for n=850/2
round(power.prop.test(p1=p1,
                      p2=p2,
                      sig.level=alpha,
                      n=850/2)$power,2)
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

