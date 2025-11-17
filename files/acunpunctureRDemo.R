rm(list=ls())

# load data
load(url("http://paulblanche.com/files/acupuncture.rda"))
head(acupuncture)
d <- acupuncture # just for convenience

# data of each arm
d1 <- d[d$arm=="placebo",]
d2 <- d[d$arm=="acupuncture",]

# plot data
cols <- c("blue","red") 
plot(x=d1$pre,y=d1$post,pch=1,xlim=c(20,100),ylim=c(20,100),
     xlab="pre-treatment score",ylab="post-treatment score",cex=2,col=cols[1])
points(x=d2$pre,y=d2$post,pch=4,cex=2,col=cols[2])
abline(h=c(20,40,60,80,100),col="grey")
abline(v=c(20,40,60,80,100),col="grey")
legend("bottomleft",
       pch=rev(c(1,4)),
       col=rev(cols),
       legend=rev(c("Placebo","Acupuncture")),bg="white")

# ancova analysis
ancova <- lm(post~pre+arm,data=d)
summary(ancova)
confint(ancova)
coef(ancova)[1]

# add regression lines of ANCOVA to the previous plot
abline(a=coef(ancova)[1],b=coef(ancova)[2],col=cols[1],lty=2,lwd=2)
abline(a=coef(ancova)[1]+coef(ancova)[3],b=coef(ancova)[2],col=cols[2],lty=2,lwd=2)


# t-test on post-treatment score
t.test(post~arm,data=d)

# t-test on post-treatment change
d$change <- d$post-d$pre # add change variable to dataset
t.test(change~arm,data=d)
