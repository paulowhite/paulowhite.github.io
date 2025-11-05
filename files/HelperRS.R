## R code based on Appendix of Steingrimsson et al,
## Contemporary Clinical Trials 54 (2017) 18–24 Contents 

# compute estimators of risks and risk difference
stand.est = function(Y, A, W){
    # Creating a data frame from the input variables
    data.used = data.frame(W, A, Y)
    # Fitting the multiple logistic regression model
    log.reg = glm(Y ~., data = data.used, family = "binomial")
    # Creating dataset for calculating the predictions corresponding to # A = 1 and A = 0 
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

# Calculating the variance of the estimators, via bootstrapping.
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
    # compute all corresponding risk differnce estimates
    boot.diff <- boot.res.1-boot.res.0
    # Print the estimated variances (i.e., square of standard errors)
    # for the estimated risk in each arm and the difference.
    cat("\nVar.Diff=",var(boot.diff),", Var.risk.1=",var(boot.res.1),
        " and Var.risk.0=",var(boot.res.0),"\n")
    # Outpiy: return all estimated variances as well as the main results of each
    # boostrapped sample in appendix (can be useful for further computation)
    invisible(list(Var.Diff=var(boot.diff),
                   Var.risk.1=var(boot.res.1),
                   Var.risk.0=var(boot.res.0),
                   details=list(boot.res.1=boot.res.1,
                                boot.res.0=boot.res.0))
              )
}
