## R code based on Appendix of Steingrimsson et al,
## Contemporary Clinical Trials 54 (2017) 18–24 

# Compute estimates of risks and risk difference, via
# regression standadization, using a multiple logistic regression model
#
# Input are:
# - Y: vector of 0 or 1, binary outcome
# - A: vector of 0 or 1, treatment arm
# - W: dataset with columns containing the relevant baseline covariates
#      to adjust for (not more, not less).
#
stand.est = function(Y, A, W){
    # Creating a data frame from the input variables
    data.used = data.frame(W, A, Y)
    # Fitting the multiple logistic regression model
    log.reg = glm(Y ~., data = data.used, family = "binomial")
    # Creating dataset for calculating the predictions corresponding to A = 1 and A = 0 
    data.a.1 = data.used
    data.a.1$A = 1
    data.a.0 = data.used
    data.a.0$A = 0
    # Calculating the predictions
    pred.1 = predict.glm(log.reg, newdata = data.a.1[, colnames(data.used) != "Y"] , type = "response")
    pred.0 = predict.glm(log.reg, newdata = data.a.0[, colnames(data.used) != "Y"] , type = "response")
    # estimated average/marginal risk for each arm
    res.1 = mean(pred.1) 
    res.0 = mean(pred.0)
    # estimated average/marginal/unconditional risk difference
    res.diff = res.1 - res.0   
    # output
    return(c(Diff=res.diff,risk.1=res.1,risk.0=res.0))
}

# Compute the estimated variance of the estimators, via bootstrapping.
#
# Note: the number of Boostrap samples n.boot=400 is the default,
# although in practice we typically want more for the main analysis!
# (e.g., n.boot=2000). A smaller number of n.boot=400 is just convenient
# to save time during practicals or for preliminary analyses.
#
var.stand.est = function(Y, A, W, n.boot=400,Trace=TRUE){
    boot.res.1 <- boot.res.0 <-  rep(NA, n.boot)
    # Loop over bootstrapped samples
    for(i in 1:n.boot){
        if(Trace) cat("\nStep ",i,"out of ",n.boot)
        # Finding the bootstrap sample
        bs = sample(1:length(A), size = length(A), replace = TRUE)
        # Compute estimates from the bootstrap sample
        resb <- stand.est(Y[bs], A[bs], W[bs,])
        # Save the estimated risks
        boot.res.1[i] = resb["risk.1"]
        boot.res.0[i] = resb["risk.0"]
    }
    if(Trace) cat("\n")
    # Compute all corresponding risk differnce estimates
    boot.diff <- boot.res.1-boot.res.0
    # Print the estimated variances (i.e., square of standard errors)
    # for the estimated risks in each arm and their difference.
    cat("\nVar.Diff=",var(boot.diff),", Var.risk.1=",var(boot.res.1),
        " and Var.risk.0=",var(boot.res.0),"\n")
    # Output: return all estimated variances as well as the main results of each
    # boostrapped sample in "details", as it can be useful for further computations.
    invisible(list(Var.Diff=var(boot.diff),
                   Var.risk.1=var(boot.res.1),
                   Var.risk.0=var(boot.res.0),
                   details=list(boot.res.1=boot.res.1,
                                boot.res.0=boot.res.0))
              )
}
