rm(list=ls())

# expected risks, Type-I error and power
p1 <- 0.17    # expected risk in control arm
p2 <- 0.11    # expected risk in active arm
alpha <- 0.05 # Type-I error

# we consider two sample sizes per arm
# to compute the power: n=650/2 and n=850/2
the.n <- c(650,850)/2   

# compute pbar, delta, se and EZ
pbar <- (p1 + p2)/2               # average of the two risks
delta <- p1-p2                    # risk difference
SE <- sqrt(2*pbar*(1-pbar)/the.n) # Compute standard error
EZ <- delta/SE 

# check that 'pnorm(z)' computes P(Z<z) where Z is a standard normal random variable via two examples
pnorm(1.96)
pnorm(0)

# now compute the powers for the two sample sizes 
round(pnorm( EZ - 1.96),2)  # results: 60% and 71%

