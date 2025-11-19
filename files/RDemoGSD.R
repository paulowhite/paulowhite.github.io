rm(list=ls())

library(gsDesign)

Res.GSD <- gsDesign(k=3,           # K=3 analyses, i.e., 2 interim
                    test.type = 4, # non-binding futility boundaries
                    alpha=0.025,   # type-I error
                    beta=0.2,      # type-II error = 1-power
                    sfu=sfPower,   # spending function for upper boundaries
                    sfl=sfPower,   # spending function for lower boundaries
                    sfupar=3,      # rho=2 (upper)
                    sflpar=3,      # rho=2 (lower)
                    timing=c(0.5,0.75,1)) # timing of analyses
# print main results (default)
# Note: what is called Theta is E(Z) the equivalent fixed sample size.
# i.e., E(Z)=0 under the null hypotheses and E(Z)=qnorm(1-alpha) + qnorm(1-beta)
# under the treatment effect assumption that we used to power the trial.
Res.GSD


# Compute crossing probabilities under additional effect assumptions
# (none, as used to power the trial or 50% larger)
Prob.GSD <- gsProbability(d=Res.GSD, theta=Res.GSD$delta*c(0,1,1.5))
Prob.GSD
