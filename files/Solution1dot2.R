rm(list=ls())

# expected risks, Type-I error and power
p1 <- 0.17
p2 <- 0.11
power <- 0.8
alpha <- 0.05

# compute pbar and delta
pbar <- (p1 + p2)/2
delta <- p1-p2

# checks
pnorm(1.96)
pnorm(0)

# power if n=650/2 and n=850/2
the.n <- c(650,850)/2             # sample size, per arm
SE <- sqrt(2*pbar*(1-pbar)/the.n) # Compute standard error
pnorm( delta/SE - 1.96)           # results: 60% and 71%

