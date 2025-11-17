rm(list=ls())

# Question 1 (TRACTION sample size calculation)
n.traction <- 2*(1.96 + 1.28)^2*(2*0.15*(1-0.15))/(0.15-0.15-(-0.05))^2
n.traction/2 # per arm


# Question 2
p <- c(0.20,0.17,0.15,0.13,0.10) # risk, assumed to be equal in the two arms
power.traction <- pnorm( (p-p-(-0.05))/sqrt( (2*p*(1-p))/(1070)) - 1.96) #
data.frame(p=p,power=round(power.traction,2)) # the larger the risk the lower the power


# Question 3
power.17.vs.15 <- pnorm( (0.15-0.17-(-0.05))/sqrt( (0.15*(1-0.15) + 0.17*(1-0.17))/(1070)) - 1.96) 
round(power.17.vs.15,2)
