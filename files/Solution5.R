#' ---
#' title: "Exercise 5 - solution"
#' author: "Paul Blanche"
#' output: pdf_document
#' fontsize: 12pt
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
rm(list=ls())

#' 
#' # Exercise A
#' 
#' ## Question 1
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
power.prop.test(p1=0.40,p2=0.60,power=0.85)

#' 
#' We could include 111 women in each treatment group, hence we could
#' include 222 women in total. This is enough to have 85% power if the
#' chances of pregnancy are indeed 40% and 60% in the two
#' groups. However, the power might be considerably less if the treatment
#' turns out to be less effective than we thought. It would be a pity to
#' make a type-II error, that is, to not conclude that the data show
#' enough evidence to conclude that the treatment works, if the treatment
#' works reasonably well, although maybe less well than we
#' anticipated. Hence the result to the calculation made at the next
#' questions are important to make a good, well-informed, decision about
#' the sample size.
#' 
#' ## Question 2
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
power.prop.test(n=111,p1=0.40,p2=0.5)

#' 
#' The power of the study drops to only 32%, if we include 222 women (111
#' per group) and if the treatment results in only 50% chance of
#' pregnancy. This is important to know. If we think that a treatment
#' that leads to 50% chance of pregnancy is still a "successful"
#' treatment worth prescribing, we might worry about the sample size of
#' 222 women. With this sample size, it is unlikely that we can show that
#' the treatment "works", if it works but leads to only 50% chance of
#' pregnancy.
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
power.prop.test(n=111,p1=0.40,p2=0.55)

#' 
#' The power of the study drops to 61%, if we include 222 women (111 per
#' group) and if the treatment results only in 55% chance of pregnancy.
#' This is important to know. After all, 55% is not very different from
#' our expectation of 60%, so it might not be completely unlikely. Is it
#' wise, and ethical, to choose a sample size of 222 women knowing this
#' result?  Only the investigators can know about that, after balancing
#' the pros and cons, and thinking about the financial, logistical and
#' ethical constraints they have. The investigators are expected to take
#' an informed decision about the sample size accordingly.
#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
power.prop.test(n=111,p1=0.40,power=0.75)

#' 
#' The smallest improvement in chance of pregnancy that leads to a decent
#' power of 75% or more, with this sample size of 222 women, is 18%,
#' i.e. 58% chance of pregnancy with treatment versus 40% without
#' treatment (if the chance of pregnancy without treatment is indeed
#' 40%). Again, we believe it is important to know to take an informed
#' decision about the sample size of the study. If we think that a
#' treatment leading to less than 58% chances of pregnancy can still be
#' thought as a "successful" treatment, then we could consider increasing
#' the sample size. Indeed, otherwise we have a somewhat low power (less
#' than 75%) to have a significant result with such a treatment.
#' 
#' 
#' To sum-up, several sample size and power calculations are often
#' interesting to perform, to best understand the consequences of the
#' choice of a specific sample size. The main aim of this exercise was to
#' illustrate that.
#' 
#' Note that it is often interesting to choose the sample size which is
#' the result of using not what we expect in the treatment group as input
#' in the sample size calculation (60% in this exercise), but instead the
#' "least good result" that we would still be happy to have (e.g.,
#' 50%). But, ethical, financial and logistical constraints are also
#' important to take into account, such that we do not always proceed
#' like this. Planning a study is not all about statistics, although
#' statistics have an important role to play!
#' 
#' 
#' 
#' # Exercise B
#' 
#' # Part I
#' 
#' ## Question 1
#' 
#' We first load the data and look at the "summary", as always.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
load(url("http://paulblanche.com/files/smoking.rda")) 
d <- smoking
summary(d)

#' 
#' We can see that 945 women among the 1314 were still alive 20 years after the initial survey. There were 582 smokers among the 1314.
#' 
#' ## Question 2 (2.a & 2.b)
#' 
#' We first produce a 2 by 2 table. 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(smoker=d$smoker,death=d$death)

#' 
#' We observe:
#' 
#'  - 502 non-smokers alive at 20 years after the initial survey
#'  - 230 non-smokers dead within 20 years after the initial survey
#'  - 443 smokers alive at 20 years after the initial survey
#'  - 139 smokers dead within 20 years after the initial survey
#' 
#' This gives the following 20-year risk estimates for smokers and non-smokers:
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
230/(502+230) # 20-year risk of death among non smokers
139/(443+139) # 20-year risk of death among smokers

#' 
#' That is, 31.4% for non smokers and 23.9% for smokers. These results seem surprising: we know that smoking is unhealthy, hence we expected a higher risk of death for smokers.  
#' 
#' ## Question 2 (2.c & 2.d)
#' We now compute the corresponding "exact" binomial confidence intervals.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
binom.test(x=230,n=502+230) # non smokers
binom.test(x=139,n=443+139) # smokers

#' 
#' We therefore have the following 20-year risk estimate and 95% CI:
#' 
#'  - Non-smokers: 31.4% (28.1 ; 34.9)
#'  - Smokers: 23.9% (20.5 ; 27.6)
#'  
#' The 95% confidence intervals do not overlap, hence the condidence intervals "suggest" that the direction of the result, that is, a higher risk of death for non-smokers, is not due to small sample random variation. 
#' 
#' ## Question 3
#' We now perform a statistical hypothesis test to compare the risk among smokers and non-smokers. We choose to use the Fisher's exact test because we prefer to obtain "exact" results to approximate results when we can. Howerver, the large sample size can justify to use a large sample method such as the Pearson Chi-square test.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fisher.test(table(smoker=d$smoker,death=d$death))

#' 
#' We obtain a p-value=0.003, smaller than 5%, hence we conclude to a significant association between smoking and 20-year risk of death.
#' 
#' Just out of curiosity, we also compute the p-value of the Pearson Chi-square test.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
chisq.test(table(smoker=d$smoker,death=d$death))

#' 
#' We see that the p-value is almost the same, which is due to the large sample size.
#' 
#' ## Questions 4 & 5
#' We now use the fonction **table2x2()** from the **Publish** package to estimate several association measures, with confidence intervals. We start with the survival probability difference:
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Tab1 <- table(smoker=d$smoker,death=d$death)
library(Publish)
table2x2(Tab1,stats = c("table","rd"))

#' 
#' We can first check that the survival probability estimates match the previous results. We have:
#' 
#'  - Non-smokers: 68.6%, which is indeed equal to 100 - 31.4
#'  - Non-smokers: 76.1%, which is indeed equal to 100 - 23.9
#' 
#' We finally conclude that the estimated survival chance difference is 7.5%  (95% CI= [12.4;2.7]).
#' 
#' We now compute the ratio of the survival probabilities.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table2x2(Tab1,stats = c("rr"))

#' 
#' The estimated survival ratio is 0.901 (95% CI=[0.843;0.963]). Equivalently, we can also say that the chance of survival is 1-0.901= 9.9% lower for non smokers than for smokers (95% CI=[3.7;15.7]). 
#' 
#' Finally, we now compute the survival odds ratio.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table2x2(Tab1,stats = c("or"))

#' 
#' The estimated survival odds ratio is 0.685 (95% CI=[0.535;0.876]).
#' 
#' 
#' ## Question 6
#' We now produce a barplot to compare the number of included women in each age group, between smokers and non smokers.
#' 
#' We first compute the frequency table.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Tab2 <- table(d$smoker,d$age) 
Tab2

#' 
#' And we now plot these counts.
#' 
## ---- fig.height=5,fig.width=6------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
barplot(Tab2,beside=TRUE,
        xlab="age",
        ylab="number of women",
        legend=TRUE,
        args.legend=list(title="Smoker",ncol=2))

#' 
#' 
#' Interestingly, we observe that very few women above 65 are smokers. Although the number of smokers and non-smokers is not the same also in the other age groups, the differences are not as large. 
#' 
#' ## Question 7
#' 
#' We now produce a barplot to compare the 20-year survival probability in each age group. 
#' 
#' We first compute the frequency table, and then compute the survival probabilities.
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Tab3 <- table(d$death,d$age) # counts
Tab3 
Tab4 <- prop.table(Tab3,margin=2) # proportions per age
Tab4 

#' 
#' We are now ready to plot the 20-year survival probabilities in each age group.
## ---- fig.height=5,fig.width=6------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
barplot(100*Tab4[1,],
        xlab="age",
        ylab="20-year survival probability (%)",
        col="black")

#' 
#' We can observe that the older the women, the less likely they survive 20 years. This seems to make perfect sense. 
#' 
#' ## Question 8
#' We now produce a barplot to compare the 20-year survival probability in each age group, between smokers and non smokers. First, we compute the survival probabilities in each age group.
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Tab3smokers <- table(d$death[d$smoker=="yes"],
                     d$age[d$smoker=="yes"]) # counts for smokers
Tab4smokers <- prop.table(Tab3smokers,
                          margin=2) # proportions per age for smokers
Tab3NonSmokers <- table(d$death[d$smoker=="no"],
                        d$age[d$smoker=="no"]) # counts for non smokers
Tab4NonSmokers <- prop.table(Tab3NonSmokers,
                             margin=2) # proportions per age for non smokers
# merge the results into one unique matrix
Tab5 <- rbind(Tab4smokers[1,],Tab4NonSmokers[1,])
rownames(Tab5) <- c("Smoker","Non smoker")
Tab5

#' 
#' 
#' We are now ready to produce the plot.
#' 
## ---- fig.height=5,fig.width=6------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
barplot(Tab5,
        beside=TRUE,
        xlab="age",
        ylab="20-year survival probability",
        legend=TRUE)

#' 
#' We see observe that in all age groups, the 20-year survival
#' probability is lower for smokers than for non-smokers. This makes
#' sense, we know that smoking is not healthy. We also observe that there
#' is a substantial difference in 20-year survival probability between
#' age groups. This makes sense, the older a women, the more likely she
#' cannot survive 20 years. Finally, we observe that the differences in
#' 20-year survival probability between age groups are substantially
#' larger than the differences in 20-year survival between smoker
#' and non smokers, when comparing women of the same age group. This
#' makes sense: smoking is bad, but not as bad as becoming old (for the
#' 20-year risk of death).
#' 
#' ## Question 9
#' 
#' We have seen that most smokers are younger than non-smokers in this
#' data set. Especially, most of the women older than 65 are
#' non-smokers. Because we have also seen that the 20-year risk of death
#' is much higher for women older than 65 than the others (as expected),
#' we might suspect that the survival difference between smokers and
#' non-smokers is mostly driven by the fact that those who do not smoke
#' are older.
#' 
#' Smoking is known to be unhealthy, but it is probably not as
#' life-threatening as becoming "old", which is what suggests the plot
#' obtained at question 8. Hence the results of question 2: when
#' comparing young smokers to old non-smokers, the data suggest that old
#' non-smokers die more often.
#' 
#' # Part II
#' 
#' ## Question 10 
#' Below we examplify how to do it for the age group 45-54.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Tab1a <- table(smoker=d$smoker[d$age=="45-54"],death=d$death[d$age=="45-54"])
table2x2(Tab1a,stats = c("table","rr"))

#' 
#' We can proceed similarly for the other age groups and obtain:
#' 
#'  - Age group below 45: estimated risk ratio 1.030 (95% CI= [0.992;1.069]).
#'  - Age group 45-54: estimated risk ratio 1.068 (95% CI= [0.938;1.215]).
#'  - Age group 55-64: estimated risk ratio 1.203 (95% CI= [0.979;1.478]).
#'  - Age group above 65: estimated risk ratio 1.016 (95% CI= [0.472;2.186]).
#' 
#' There is no significant difference in any age group, although there
#' seems to be a systematic trend towards a higher 20-year risk of death
#' for smokers. The fact that the results are not significant is probably
#' due to a lack of power. The sample size of each age group is not very
#' large.
#' 
#' ## Question 11
#' 
#' The results of Part II can be thought as more interesting because the
#' two groups that we compare (smokers versus non smokers) are expected
#' to be more similar with respect to everything but smoking, as the
#' women of the two groups have a similar age. Hence we can expect that
#' the association between smoking and survival that we estimated are
#' closer to causal associations than those of Part I, although we cannot
#' rigouroulsy claim that they are indeed causal.
#' 
#' 
#' # Exercise C
#' 
#' ## Question 1
#' 
#' We first load the data and look at the first rows.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
rm(list=ls())   # clear memory (to start fresh for the new exercise)
load(url("http://paulblanche.com/files/sedative.rda"))
head(sedative) 
d <- sedative # data in an object with a shorter name, for convenience

#' 
#' 
#' ## Question 2
#' 
#' First, we tabulate the data from the night after receiving zolpidem.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(d$zolpidem)

#' 
#' We can read that 33 out of 33+19=52 subjects did not wake before receiving the loudest
#' tone (110 dB). 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
prop.table(table(d$zolpidem))

#' 
#' This corresponds to 33/(19+33)=63.5% of the subjects. We proceed similarly with the data from the night after receiving doxepin.
#' 
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(d$doxepin)
prop.table(table(d$doxepin))

#' 
#' We read that 9 out of 52 subjects did not wake before receiving the loudest
#' tone (110 dB), that is 17.3%. 
#' 
#' ## Question 3
#' 
#' We compute 95% exact binomial confidence intervals for the risk of not waking  before receiving the loudest tone (110 dB), after receiving each treatment.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
binom.test(x=33,n=52)
binom.test(x=9,n=52)

#' 
#' We read the 95% confidence intervals: [49.0%; 76.4%] for zolpidem and [8.2% ; 30.3%] for doxepin.
#' 
#' Note that to round the values of the upper and lower limits, we can use this code:
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
round(100*binom.test(x=9,n=52)$conf.int,1)

#' 
#' ## Question 4
#' 
#' Here we want to compare two proportions obtained from the **same**
#' subjects. In other words, we want to compare two sets of binary data
#' which are **paired**. Although we might first think about using a
#' Fisher test or a Chi-square test when we aim to compare two
#' proportions, here this would not be appropriate. This is because
#' Fisher test and Chi-sqaqre tests assume that the two samples that we
#' compare are independent. This is not the case here as the data come
#' from two different nights for the **same** subjects. The data are
#' paired per subject and not independent. Some subjects will tend to be
#' fast asleep no matter the drug they take (or not) and some others tend
#' to be light sleepers, also no matter the drug they take. To analyze
#' such **paired data** the appropriate analogue to the chi-square test
#' is the **McNemar test**.
#' 
#' ## Question 5
#' 
#' We now compute the p-value from a **McNemar test** test. We prefer the
#' "exact" version of the test to a (maybe more commonly used) version
#' based on a large sample approximation. After all, why approximate when
#' you can get the exact?
#' 
#' The aim when using this test is to understand whether the data of the
#' two nights are sufficiently different to confidently conclude that one
#' treatment leads to a higher risk of not waking than the other.
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Tabcompare <- table(zolpidem=d$zolpidem,
                    doxepin=d$doxepin)
Tabcompare

#' 
#' 
#' We can now proceed with the `mcnemar.exact` function of the package `exact2x2`.
## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(exact2x2)
mcnemar.exact(Tabcompare)

#' 
#' We read that the p-value is very small: p < 0.001. Hence the data
#' contains enough evidence to confidently conclude that the risk of
#' not waking are different after receiving each treatment. Usually it is
#' unnecessary to provide the exact value of a p-value when it is smaller
#' that 0.001.  Reporting the exact value of the p-value, i.e., 1.93e-05,
#' will often look a bit "foolish" in a scientific paper.
#' 
#' A possible conclusion sentence is: _"The risk of not waking after
#' receiving zolpidem (63.5%, 95%-CI=[49.0%; 76.4%]) was found
#' significantly larger than that after recieving doxepin (17.3%,
#' 95%-CI=[8.2% ; 30.3%], p-value<0.001)."_
#' 
#' 
#' ## Question 6
#' 
#' In the abstract of Drake et al (2017), we can read: _"A significantly
#' greater proportion of participants in the zolpidem condition (63.5%)
#' did not wake until receiving the loudest tone (110 dB) as compared to
#' the doxepin (17.6%) and placebo conditions (17.3%, 5.8%)."_ We can
#' notice that 63.5% and the significant result are similar to those we
#' found with our own analyses above. We can be happy to have
#' successfully reproduced their results. There is, however, a tiny
#' difference of 17.6% instead of 17.3% in the result for doxepin. We
#' suspect that this is a minor typo because 17.3% is actually reported
#' in the result section ‘Primary Outcomes’, subsection ’Auditory
#' Awakening Threshold’, on page 4 of the paper.
#' 
