rm(list=ls())

# Question 1

mean1 <- 10 # mean reduction under treatment A=1 with perfect adherence
mean0 <- 22 # mean reduction under treatment A=0 with perfect adherence
pNA <- 0.3  # probability of nonadherence

mean1*(1-pNA) # 7
mean0*(1-pNA) # 15.4 (we could round to 15)

# Question 2
diff.perfect.adherence <- mean1 - mean0 
diff.perfect.adherence # difference under perfect adherence

diff.ITT.nonadherence <- mean1*(1-pNA) - mean0*(1-pNA)
diff.ITT.nonadherence # difference under nonadherence, using ITT
# Rk: we assumed no reduction if no treatment taken, or alternatively,
# same reduction in the two arms (e.g., regression to the mean)

# Question 3
NImargin <- 10
diff.perfect.adherence + NImargin # -2: negative
diff.ITT.nonadherence + NImargin  # 1.6: positive

# E(Z)=(delta-delta_0)/SE. If E(Z)>0, then type-I error is larger
# than 2*pnorm( - 1.96), hence larger than 5%, e.g. if E(Z)=1,
2*pnorm( 1 - 1.96) # 33%
# If E(Z)<0, then type-I error is smallter than
#  2*pnorm( - 1.96), hence larger than 5%, e.g. if E(Z)=-1,
2*pnorm( -1 - 1.96) # 0.3%
# ITT with nonadherence here leads to E(Z)>0, hence type-I error > 5%.

# Question 4
# E(Z)=2.8 if 80% power, and E(Z)=(delta + delta0)/SE, where SE depends (only) on the standard
# deviation of the outcome and sample size (SE=sigma*\sqrt{2/n}, see lecture 1); so
# we can reasonably expect that SE is about the same whether the treatments
# are equally efficacious or not. If they are equally efficacious, the mean difference
# is delta=0. Hence, delta0/SE=2.8, hence SE=delta0/2.8=10/2.8=3.57. If the difference
# is not 0, but instead -8.4 (diff.ITT.nonadherence), then  EZ=(-8.4 + 10)/3.57=1.6/3.57
# and
2*pnorm(0.448-1.96) # 13% Type-I error, RK: we multiply by 2 because 2-sided p-values
# Another look at it (equivalent, more details):
EZ.adherence <- (diff.perfect.adherence + NImargin)/(10/2.8)
EZ.nonadherence <- (diff.ITT.nonadherence + NImargin)/(10/2.8)
EZ.nodiff <- 0
2*pnorm(EZ.nodiff-1.96)    # 5% Type-I error if difference = NI-margin (e.g., 20 vs 10, perfect adherence)
2*pnorm(EZ.adherence-1.96) # 1% < 5% Type-I error if difference > NI-margin (e.g., 22 vs 10, perfect adherence)
2*pnorm(EZ.nonadherence-1.96) #13% > 5%   Type-I error if difference > NI-margin (e.g., 22 vs 10, by with noadherence)
