rm(list=ls())

# Question 1

#  E(Z_delta)=1.96+0.84=2.8, for the Z statistic for the main treatment effect delta.
# As E(Z_delta)=delta/se(delta), we have E(Z_interaction)=interaction/se(interaction)=
# (delta/2)/(2*se(delta))= (delta/se(delta))/4=2.8/4=0.7

# check, power for average treatment effect 
pnorm(2.8 - 1.96) # 80%

# power for interaction term
pnorm(0.7 - 1.96) # 10%

# Question 2

# Well, E(Z) is 4 times too small (0.7 instead of 2.8). Also, E(Z) is proportional to sqrt{n},
# as it is proportion to 1/SE and SE is proportional to 1/sqrt{n}. Hence, we need sqrt{n} to be
# 4 times larger. That is, use n'=4^2n=16n. We need a sample size 16 times larger!


# Question 3
# simulate many Z statistics normally distributed with mean=0.7 and SD=1
z <- rnorm(1e6, 0.7, 1)
# compute the mean among those who are significant (>1.96)
mean(z[z>1.96]) #2.4=E(Z|Z>1.96)= (approximately) E(Estimated interaction|Z>1.96)/se(interaction)
# Conclusion: Estimated interaction is, in average, 2.4/0.7 = 3.4 times to large !

# Lessons: we have very little power to show that an interaction term exists (i.e., that there is
# heterogenous treatment effect) and when we show it exists, we usually overestimate it. This is
# a classical drawback of all under-powered studies.

