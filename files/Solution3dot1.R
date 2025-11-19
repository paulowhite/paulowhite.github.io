rm(list=ls())

p1 <- 0.17
p2 <- 0.11
power <- 0.8
alpha <- 0.05
the.n <- 1048/2


# reminder
0.4*0.3 + 0.6*0.075 # 16.5, approx 17%
0.4*0.2 + 0.6*0.05  # 11%


# Question 1
p2.ITT <- 0.4*(0.2*0.9 + 0.3*0.1) + 0.6*0.05 
p2.ITT # 11.4% instead of 11%

# Question 2
# Reminder: compute power under perfect adherence
pbar <- (p1 + p2)/2
delta <- p1-p2
SE <- sqrt(2*pbar*(1-pbar)/the.n) # Compute standard error
pnorm( delta/SE - 1.96)  # 80%           

# Power with nonadherence and ITT
pbar.ITT <- (p1 + p2.ITT)/2
delta.ITT <- p1-p2.ITT
SE.ITT <- sqrt(2*pbar.ITT*(1-pbar.ITT)/the.n) # Compute standard error
pnorm( delta.ITT/SE.ITT - 1.96)   # 74%

# check 
power.prop.test(p1=p1,
                p2=p2.ITT,
                sig.level=alpha,
                n=the.n)

# Question 3
# E(Z) principle formula
the.n.ITT <- (1.96 + 0.84)^2*(2*pbar.ITT*(1-pbar.ITT))/(p1-p2.ITT)^2
the.n.ITT  # 609 per arm (524 before)

# check using power.prop.test()
ceiling(power.prop.test(p1=p1,
                        p2=p2.ITT,
                        sig.level=alpha,
                        power=power)$n)

# Question 4
(2*(the.n.ITT-the.n)/130)*12 # approx 16 months
