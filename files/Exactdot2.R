rm(list=ls())

# {{{ parameters
p1 <- 0.99     # expected prop no event in group 1
p2 <- p1       # expected prop no event in group 2
delta <- 0.05  # non-inf margin
n1 <- n2 <- 81 # sample size, per arm
alpha <- 0.025 # type-I error (one sided test)
# }}}

# {{{  All "most likely" cases
AllCases <- expand.grid(x1=0:3,x2=0:3)
head(AllCases)
tail(AllCases)
# }}}

# {{{ Compute the likelihood
# of observing x1 and x2 events at the
# end of the trial, assuming the risks in each
# arm are indeed p1 and p2.
ProbCase <- rep(NA,nrow(AllCases))
for(i in 1:nrow(AllCases)){
    print(paste0("Step ",i," out of ",nrow(AllCases)))
    x1 <- AllCases$x1[i]
    x2 <- AllCases$x2[i]
    # Prob(X_1=x1 & X_2=x2)=Prob(X_1=x1)Prob(X_2=x2) (2 indepent arms)
    # and computation using binomila distribution
    ProbCase[i] <- dbinom(x=x1,size=n1,prob=1-p1)*dbinom(x=x2,size=n2,prob=1-p2)
}
# Probability of possible cases covered by our computation (we ignore many
# some rare cases, e.g., x1=x2=50...).
sum(ProbCase)
# }}}


# {{{ Data management
AllCases <- cbind(AllCases,ProbCase)
AllCases <- AllCases[order(-AllCases[,"ProbCase"]),]
AllCases$pvalue <- NULL
head(AllCases)
# }}}


# {{{ Compute p-value in each of the most likely cases
library(exact2x2)
StartLoop <- Sys.time()
for(i in 1:nrow(AllCases)){    
    print(paste0("Step ",i," out of ",nrow(AllCases)))
    #--
    x1 <- AllCases$x1[i]
    x2 <- AllCases$x2[i]
    #--
    ResFunc <- uncondExact2x2(x1=x1,n1=n1, # IVT group
                              x2=x2,n2=n2, # OT  group
                              parmtype="difference",
                              alternative = "less",
                              method="score",
                              conf.level = 0.975,
                              nullparm=delta,
                              conf.int=FALSE) # FALSE to speed up computation
    AllCases$pvalue[i] <- ResFunc$p.value
    #--
}
StopLoop <- Sys.time()
difftime(StopLoop,StartLoop)
# }}}

# Print the computed power 
cat(paste0("\nPower is ",round(sum(AllCases[which(AllCases$pvalue<=alpha),"ProbCase"])*100,1),
       "% with n=n1=n2=",n1," per arm and risk=p1=p2=",p1,"\nNote: we covered ",
       round(100*(sum(ProbCase)),1),"% of all cases;\nhence the power could be understimated by at most ",
       round(100*(1-sum(ProbCase)),1),"%.\n"))
