rm(list=ls())


# Question 1

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


# Question 1.1
# See output code above.

# Question 1.2
# The output of the code above provides:

## Upper boundary (power or Type I Error)
          ## Analysis
   ## Theta      1      2      3  Total   E{N}
  ## 0.0000 0.0031 0.0074 0.0135 0.0240 0.6827
  ## 2.8016 0.2414 0.3205 0.2381 0.8000 0.8125
  ## 4.2024 0.6234 0.2951 0.0692 0.9877 0.6453

## We read that the chances of stopping early for efficacy (Assuming that the treatment effect indeed corresponds to the
## value assumed to power the trial) are 0.2414 (1st interim), 0.3205 (2nd interim) and 0.2381 (final).
## We can check that 0.2414 +  0.3205 + 0.2381= 0.80, as expected as we planned for 80% power.


# Question 1.3

## From the same outpu, we can further read that the chances of stopping early for efficacy, now assuming no treatment effect,
## are 0.0031 (1st interim), 0.0074 (2nd interim) and 0.0135 (final).
## We can check that 0.0031 + 0.0074 + 0.0135 = 0.0240 = 2.4%, which is sligthly less than the intented 2.5% type-I
## error that we wished. This is because the possibility for stopping early for futility also slightly reduces the risk
## of false positive finding. Here, the 2.4% was computed assuming that crossing a futility boundary would stop the trial.
## However, because the futility boundaries are not binding, we have to plan for the worse case
## scenario, that is, assume that the trial would not stop early for futility, when computing the efficacy boundaries,
## to ensure the control the type-I error at 2.5% in any case. 


# Question 1.4

# The output of the code above provides:

## Lower boundary (futility or Type II Error)
          ## Analysis
   ## Theta      1      2      3  Total
  ## 0.0000 0.5289 0.3345 0.1125 0.9760
  ## 2.8016 0.0250 0.0594 0.1156 0.2000
  ## 4.2024 0.0015 0.0032 0.0076 0.0123

## We read that the risks of stopping early for futility (Assuming that the treatment effect indeed corresponds to the
## value assumed to power the trial)  are 0.0250 (1st interim), 0.0594 (2nd interim) and 0.1156 (final).
## We can check that 0.0250 + 0.0594 + 0.1156= 0.20, as expected as we planned for 80% power, hence the type-II error is 20%..

# Question 1.5

## Similarly, we further read that the chance of stopping early for futility (Assuming no treatment effect) are
## 0.5289 (1st interim), 0.3345 (2nd interim) and 0.1125 (final).
## We can check that 0.5289 + 0.3345 + 0.1125 = 0.9760, as expected as we have seen above that the type-I error is 2.4%,
## and 100% - 2.4%= 97.6%.

# Question 1.6

## In the output of the above code, we can read "Sample Size Analysis Ratio*" with the
## footnote "* Sample size ratio compared to fixed design with no interim". At Analysis 3 (final)
## this provides the maximal Sample Size Ratio as compared to a fixed design trial without interim analyses,
## in other words, the inflation factor. It is 1.053, i.e., we need 5.3% higher (maximal) sample size to keep the
## same power using a group sequential design.


# Question 2

# We can change 3 by 1 or 2 in "sfupar=3" and "sflpar=3" to recognize the same results for another rho parameter.
# We can similarly recognize the results presented on the slides.

# Question 3

1285/130     # nearly 10 years if doe not stop early,
# so 5 or 7.5 years if stop early at first or second interim analysis


# Question 4:
Prob.GSD <- gsProbability(d=Res.GSD, theta=Res.GSD$delta*c(0,0.5,1,1.5)) # we just added 0.5 
Prob.GSD
# Efficacy probbailities: 24% -> 4.3% and 32% -> 9.7% (as compared to treatment effect as assumed to power the trial);
# It makes sense, it is more difficult to conclude that a treatment works if it indeed works but less well than expected.
# Futility probbailities: 53% -> 17% and 33% -> 27% (as compared to no treatment effect)
# It makes sense, it is more difficult to conclude that the treatment does work if it indeed work.


# Question 5:
Res.GSD.4 <- gsDesign(k=4,           # K=4 analyses, i.e., 3 interim
                      test.type = 4, # non-binding futility boundaries
                      alpha=0.025,   # type-I error
                      beta=0.2,      # type-II error = 1-power
                      sfu=sfPower,   # spending function for upper boundaries
                      sfl=sfPower,   # spending function for lower boundaries
                      sfupar=3,      # rho=2 (upper)
                      sflpar=3,      # rho=2 (lower)
                      timing=c(0.25,0.5,0.75,1)) # timing of analyses
Res.GSD.4
# If keeping the same rho, power etc.. and adding an interim analysis at 25% of
# the maximal sample size:
# 1) inflation factor: 5.3% -> 5.4%,
#    i.e., n.max=1220*1.053=1285 -> n.max=1220*1.054=1286.
#    Just one more subject needed to have the same power.
# 2) still about 53% chance of stopping early for futility before
#    enrolling more than 50% of the patients if we should, but now
#    we could even stop sometimes as early as after 25%, with a non-neglible
#    chance of almost 10%. This might be an advantage, from an ethical point of view.
# 3) Similarly, chance of stopping early for efficacy still about 24% before
#    enrolling more than 50% of the patients if treatment is as good as assumed for
#    the power calculation, but now there is also a very small chance to stop even before (3%).
#
# Hence, from the perspective of doing the right thing (stop or continue) at the right time,
# and from an ethical perspective, it could be better to add this interim analysis.
# However, it means more work (i.e., one more analysis). So the logistic is heavier. Also, sometimes
# it might be good to not stop before we have acccrued enough data, as this can be important also
# for e.g., secondary endpoints. Is the additional work/logistic worth the "improved" chances to
# do the right thing at the right time? Not easy to say...

# Question 6:
# Using a smaller value for rho (e.g. rho=2)  makes the early interim analysis more
# interesting (higher chances to stop) and keep having the other analysis as interesting.
# But the price to pay is the increase in maximal sample size.... See results of code below.
Res.GSD.4.rho2 <- gsDesign(k=4,         # K=4 analyses, i.e., 3 interim
                      test.type = 4, # non-binding futility boundaries
                      alpha=0.025,   # type-I error
                      beta=0.2,      # type-II error = 1-power
                      sfu=sfPower,   # spending function for upper boundaries
                      sfl=sfPower,   # spending function for lower boundaries
                      sfupar=2,      # rho=2 (upper)
                      sflpar=2,      # rho=2 (lower)
                      timing=c(0.25,0.5,0.75,1)) # timing of analyses
Res.GSD.4.rho2


# Question 7:
Res.GSD.2 <- gsDesign(k=3,           # K=3 analyses, i.e., 2 interim
                      test.type = 4, # non-binding futility boundaries
                      alpha=0.025,   # type-I error
                      beta=0.2,      # type-II error = 1-power
                      sfu=sfPower,   # spending function for upper boundaries
                      sfl=sfPower,   # spending function for lower boundaries
                      sfupar=3,      # rho=2 (upper)
                      sflpar=3,      # rho=2 (lower)
                      timing=c(0.33,0.66,1)) # timing of analyses
Res.GSD.2
# Well, it makes the first interim analysis definitely more interesting. But maybe the second comes a
# bit late (66% instead of 50%).... Not easy to say. Careful thinking in the context of the trial
# (with the investigators) would be needed. As a side remark, for a long trial of up to 10 years,
# interim analyses at 50% and 75% are not very close to each other (2.5 years apart). For a shorter trial
# more time between the interim analysis (e.g., 33% of full duration) might be more appropriate.


# Question 8:

u1 <- Res.GSD$upper$bound[1] # upper (Efficacy) boundary
u1 # 2.73
l1 <- Res.GSD$lower$bound[1] # lower (Futility) boundary
l1 # 0.07


R <- max(Res.GSD$n.I) # inflation factor
R  # 1.053
EZfixed <- qnorm(1-0.025)  + qnorm(1-0.2)
EZfixed # 2.8

# chance of early stop if the treatment effect indeed
# corresponds to the value assumed to power the trial (mean is not zero).
1-pnorm(u1,mean=(EZfixed*sqrt(R/2))) # 24%

# chance of stopping early for futility, if the treatment
#  does not work at all (mean is not zero).
pnorm(l1,mean=0) # 53%

# Alternatively, using a similar code as in Solution of Exercise 4.1:
p1 <- 0.17
p2 <- 0.4*(0.2*0.9 + 0.3*0.1) + 0.6*0.05 
power <- 0.8
alpha <- 0.05
the.n <- (1285/2)/2

pbar <- (p1 + p2)/2
delta <- p1-p2
SE <- sqrt(2*pbar*(1-pbar)/the.n) # Compute standard error
pnorm( delta/SE - u1) # 24%=P(Z>u_1) ; standard power calculation
1 - pnorm(0  - l1)    # 53%=P(Z<l_1)=1-P(Z>l_1)

