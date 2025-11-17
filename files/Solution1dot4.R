rm(list=ls())


# Question 1
# First computation by hand, using the EZ principle
n.nadja <- 2*6.5^2*(1.96+1.28)^2/(15-12)^2
n.nadja # 98.5608, rounded to n=99 per arm

# Then check using power.t.test() function 
power.t.test(delta=3,sd=6.5,power=0.9) # n = 99.62325, rounded to 100.
# Minor difference (100 vs 99) because an asymptotically equivalent, but
# different, approximation is used.


# Question 2
# First computation by hand, using the EZ principle
All.n <- c(80,100,120,150)
power.nadja <- pnorm( 3*sqrt((All.n/2)/2)/6.5 - 1.96) 
data.frame(n=All.n,power=round(power.nadja,2))

# Then check using power.t.test() function.
# One by one
power.t.test(delta=3,sd=6.5,n=100/2)$power
power.t.test(delta=3,sd=6.5,n=150/2)$power
# or all at once.
power.t.test(delta=3,sd=6.5,n=All.n/2)$power


# Question 3
# First computation by hand, using the EZ principle. We compute the power
# for many difference in mean delta, to see which one leads to 90% power.
all.delta <- seq(from=3,to=6,by=0.05)
power.delta.nadja.100 <- pnorm( all.delta*sqrt((100/2)/2)/6.5 - 1.96) #
# Gather results in a table and round
Res.delta.power.nadja <- data.frame(delta=all.delta,
                                    power=round(power.delta.nadja.100,2))
Res.delta.power.nadja # see all
Res.delta.power.nadja[min(which(power.delta.nadja.100>=0.90)),]


power.delta.nadja.100 # all results
Res.delta.power.nadja[min(which(power.delta.nadja.100>=0.90)),] # answer to the question: 4.25 days

# Plot to summarize the results
plot(x=all.delta,y=power.delta.nadja.100,
     xlab="Difference in mean number of days (delta)",
     ylab="Power (with n=100/2 per arm)",type="l",ylim=c(0,1),
     lwd=2,col="blue"
     )
abline(h=0.90,col="red")
abline(v=4.25,col="red")

# check with power.t.test() function
power.t.test(sd=6.5,n=100/2,power=0.9)
