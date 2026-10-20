rm(list=ls())

# Question 1 (TRACTION sample size calculation)
p1 <- 0.15   # expected risk in control arm
p2 <- 0.15    # expected risk in active arm
NI <- 0.05    # non-inferiority margin
n <- 1070

# we use the EZ principle
#  - we add the non-inferiority margin to the risk difference to define delta (see slides)
#  - we use the standard SE formula (see slides)
delta <- p1-p2 + NI              
SE <- sqrt( (p1*(1-p1) + p2*(1-p2))/n)
EZ <- delta/SE
round(pnorm( EZ - 1.96),2) # we see 90% power

# Alternatively, using the formula E(Z)=1.96 + 1.28 (see EZ principle slide) and solving in n gives:
n.traction <- 2*(1.96 + 1.28)^2*(2*0.15*(1-0.15))/(0.15-0.15-(-0.05))^2
n.traction/2 # per arm


# Question 2
p <- c(0.20,0.17,0.15,0.13,0.10) # risk, assumed to be equal in the two arms
delta <- p-p-(-0.05)
SE <- sqrt( 2*p*(1-p)/n)
EZ <- delta/SE
power.traction <- round(pnorm( EZ - 1.96),2)
data.frame(p=p,power=round(power.traction,2)) # the larger the risk the lower the power


# Question 3
p1 <- 0.15    # expected risk in control arm
p2 <- 0.17    # expected risk in active arm

# we use the EZ principle again
delta <- p1-p2 + NI              
SE <- sqrt( (p1*(1-p1) + p2*(1-p2))/n)
EZ <- delta/SE
round(pnorm( EZ - 1.96),2) # we see 47% power

