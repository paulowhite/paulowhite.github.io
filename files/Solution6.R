#' ---
#' title: "Exercise 6 - solution"
#' author: "Paul Blanche"
#' output: pdf_document
#' fontsize: 12pt
#' ---
#' 
## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
rm(list=ls())

#' 
#' # Exercise A
#' 
#' # Part I
#' 
#' ## Question 1
#' 
#' We first load the data and look at the "summary", as always.
## -----------------------------------------------------------------------------
load(url("http://paulblanche.com/files/MI.rda")) 
summary(MI)

#' 
#' We can see that 3 variables have missing values: weight (n=12), bmi (n=12) and history (n=7). It does not matter for this exercise because we will not use these variables.
#' 
#' ## Question 2
#' We first create and add to the data a factor variable named **Smoke**, which explicitly indicates the smoking status of each woman. We then use summary again to check that the variable has been added to the dataset and see how many observations we have in each group.
#' 
## -----------------------------------------------------------------------------
MI$Smoke <- factor(MI$tobacco,
                   levels=c(1,2,3),
                   labels=c("never smoked",
                            "current smoker",
                            "former smoker"))
summary(MI)

#' 
#' We can see that 215 have never smoked, 135 are current smokers and 99 former smokers. We now estimate a "simple" (univariate) logistic model to investigate the association between myocardial infarction (**mi**) and smoking status (**Smoke**).
#' 
## -----------------------------------------------------------------------------
fit1 <- glm(mi~Smoke, data=MI, family=binomial)
summary(fit1)

#' 
#' The above summary provides us with the parameter estimates. They have an interpretation as either a "log odds" (Intercept) or as the log of an odds ratio (the others parameters). To facilitate the interpretation, we asked for the "formatted" results using the **publish()** function of the **Publish** package.
#' 
## -----------------------------------------------------------------------------
library(Publish)
publish(fit1)

#' 
#' We can now interpret the resuls easily:
#' 
#'  - The risk of myocardial infarction (MI) is (significantly) higher for current smokers than for women who have never smoked (OR=4.26, 95% CI=[2.58;7.02], p-value<0.0001).
#'  - The risk of myocardial infarction (MI) is (significantly) higher for former smokers than for women who have never smoked (OR=6.65, 95% CI=[3.88;11.42], p-value<0.0001).
#'  
#'  Note however, that (surprisingly) the risk is estimated higher for former smokers than for current smokers, as the odds ratio is higher when comparing former smokers to women who have never smoked than when comparing current smokers to women who have never smoked. The odds ratio to compare former smoker to current smoker can actually be computed as 6.65/4.26=1.56.
#' 
#' Note that in the output of the summary function, we could see the estimated log odds ratios 1.4490 and 1.8953, from which we can deduce the odds ratio exp(1.4490)=4.26 and exp(1.8953)=6.65.
#' 
#' The above results are actually just another way of looking at the following frequency table.
#' 
## -----------------------------------------------------------------------------
table(Smoke=MI$Smoke,MI=MI$mi)

#' 
#' For instance the odds ratio (and log odds ratio) to compare the risk of MI between women who are current smokers and women who have never smoked can be found as follows:
#' 
## -----------------------------------------------------------------------------
60*181/(75*34)
log(60*181/(75*34))

#' 
#' And we regognize the results already seen above (up to the "default" rounding when printing the results).
#' 
#' We now make all-pairwise comparisons between the smoking groups and adjust for multiple testing. Here we make three comparisons and we adjust for multiple testing because we want to make sure that if we conclude to any association, then the risk of this conclusion to be incorrect is not "too large", i.e. not larger than 5%. 
#' 
#' 
## -----------------------------------------------------------------------------
library(multcomp)
Res1 <- glht(fit1, mcp(Smoke="Tukey"))
summary(Res1) 

#' 
#' 
#' The results indicates that there is an association between the smoking status and the risk of MI. The p-value for this association is defined as the minimun of the p-values of all pairwise comparisons, hence here p-value <0.0001. We can further conclude that there are two significant differences; between the risk of MI of:
#' 
#'  - women who are former smokers and those who have never smoked (p-value <0.0001)
#'  - women who are current smokers and those who have never smoked (p-value <0.0001)
#' 
#' The results do not show a significant difference between the risk of MI of women who are former smokers and those who are current smokers (p-value=0.214).
#' 
#' To obtain the estimated odds ratios with adjusted 95% confidence intervals, we use the following code. The idea is that by default the results are presented for the parameters, which are the logarithm of odds ratios. Hence, to get the results for the odds ratios we take the exponential.
#' 
## -----------------------------------------------------------------------------
exp(confint(Res1)$confint)

#' 
#' Hence we can update our conclusions as follows. The results indicate that there is a significance difference in the risk of MI between:
#' 
#'  - women who are former smokers and those who have never smoked (OR=4.26, 95% CI=[2.34;7.74], p-value <0.0001)
#'  - women who are current smokers and those who have never smoked (OR=6.65, 95% CI=[3.49;12.7],p-value <0.0001)
#' 
#' 
#' ## Question 3
#' 
#' We now fit a "simple" (univariate) logistic model to investigate the association between myocardial infarction (**mi**) and use of oral contraceptives (**oc**).
## -----------------------------------------------------------------------------
fit2 <- glm(mi~oc, data=MI, family=binomial)
summary(fit2)
publish(fit2)

#' 
#' The results indicate that the risk of MI is significantly higher for women who use oral contraceptives than for those who do not (OR=7.29, 95% CI=[4.66;11.40], p-value<0.0001).
#' 
#' ## Question 4
#' 
#' We now compute a frequency (3 by 2) table to compare the proportions of women who are current smokers, former smokers or who have never smoked, among those who use oral contraceptives and those who do not use them.
#' 
#' First we look at the counts.
## -----------------------------------------------------------------------------
Tab1 <- table(OC=MI$oc,Smoke=MI$Smoke)
Tab1

#' 
#' We now look at the proportions. We use the **margin=1** options to indicate that we want the proportions by use of oral contaceptives, i.e. by line (we would use **margin=2** to obtained the proportions by column). 
## -----------------------------------------------------------------------------
prop.table(Tab1,margin=1)

#' 
#' We can see that among users of oral contraceptives there are less women who have never smoked (37.5%) than among those who do not use them (56.2%). This is interesting because it means that when we compared women who use contraceptives to those wo do not at the previous question we were **implicitely** comparing:
#' 
#'  - women who use oral contraceptives **and** have "often" a current or past history of smoking (62%) 
#'  - to women who do not use oral contraceptives **and** have "less often" a current or past history of smoking (44%). 
#' 
#' As the two groups are different with respect to both use of oral contraceptives and smoking status, we conclude that from the previous results it seems difficult to know what is the "reason" for the difference in risks of MI that we observe between the two groups. It could be due to either use of oral contraceptives or smoking status/history or both (or in fact another difference between the two groups). As it is known that smoking influences the risk of MI on its own, this makes the results to the previous question not very informative to answer our research question.
#' 
#' 
#' We now estimate a (multiple) logistic model to model the risk of myocardial infarction (**mi**) using the two variables corresponding to smoking status (**Smoke**) and use oral contraceptives (**oc**). Here we are asked to not model an interaction.
#' 
## -----------------------------------------------------------------------------
fit3 <- glm(mi ~ oc + Smoke, data=MI, family=binomial)
publish(fit3)

#' 
#' We can have the following interpretations. From this model, we estimate that, when comparing two women:
#'  
#'  - one uses oral contraceptives, the other does not, both have the same smoking status/history (whatever it is), the risk of MI of the user of oral contraceptives is significantly higher (OR=6.71, 95% CI=[4.18;11.77], p-value<0.0001).
#'   - one is a current smoker, the other has never smoked, both have the same use of oral contaceptives (whatever it is, either they both use them or both do not use them), the risk of MI of the current smoker is significantly higher (OR=4.34, 95% CI=[2.51;7.51], p-value<0.0001).
#'  - one is a former smoker, the other has never smoked, both have the same use of oral contaceptives (whatever it is, either they both use them or both do not use them), the risk of MI of the former smoker is significantly higher (OR=5.32, 95% CI=[2.96;9.56], p-value<0.0001).
#' 
#' 
#' According to the first item above, the odds ratio which compares the risk of users of oral contraceptives to that of non users is **the same whatever the smoking status/history**. This seems to be inconsistent with what previous studies suggested, i.e. "*oral contraceptives could increase the risk of MI differently for smokers, non-smokers and former smokers*". What is important to notice is that this inconsistency between previous results and ours is not driven by the data but, instead, by the modeling assumption of no interaction. In other words, our modeling assumption does not allow us to estimate a different association between MI and use of oral contraceptive for women who smoke, do not smoke or have never smoked, although we should, according to previous studies.
#' 
#' ## Question 5
#' 
#' We now estimate a (multiple) logistic model to model the risk of myocardial infarction using the two variables corresponding to smoking status (**Smoke**) and oral contraceptives (**oc**) and we model an **interaction** between the two variables.
#' 
## -----------------------------------------------------------------------------
fit4 <- glm(mi ~ oc * Smoke, data=MI, family=binomial)
summary(fit4)

#' 
#' We further ask for the "formatted" results using the **publish()** function. This facilitates the interpretation of the results, especially with such a model with an interaction.
#' 
## -----------------------------------------------------------------------------
publish(fit4)

#' 
#' We can have the following interpretations. From this model, we estimate that when comparing the risk of MI of a user of oral contraceptives to that of a woman who does not use oral contraceptives:
#'  
#'  - when both have **never smoked**, the risk of MI of the user of oral contraceptives is significantly higher (OR=3.27, 95% CI=[1.54;6.95], p-value=0.002).
#'  - when both are **current smokers**, the risk of MI of the user of oral contraceptives is significantly higher (OR=5.20, 95% CI=[2.49;10.86], p-value<0.0001).
#'  - when both are **former smokers**, the risk of MI of the user of oral contraceptives is significantly higher (OR=38.25, 95% CI=[11.23;130.24], p-value<0.0001).
#' 
#' Note that these three odds ratio can be computed "by hand" from the estimated parameters shown by the **summary** function as follows:
## -----------------------------------------------------------------------------
exp(1.18562)
exp(1.18562 + 0.46276)
exp(1.18562 + 2.45852)

#' 
#' Hence, the exponential of the parameter at the lines **oc** in the output of the summary function (i.e. 1.18562) corresponds to the log of the odds ratio to for use of oral contraceptive for the **reference** smoking group, i.e. those who have never smoked. Hence the confidence interval for the OR could also be obtained as follows:
#' 
## -----------------------------------------------------------------------------
exp(confint.default(fit4)["oc",])

#' 
#' The other confidence intervals could be obtained similarly after changing the reference level for the variable "Smoke". For instance:
## -----------------------------------------------------------------------------
MI$Smokeb <- relevel(MI$Smoke,ref="current smoker")
fit4b <- glm(mi ~ oc * Smokeb, data=MI, family=binomial)
exp(coef(fit4b)["oc"])             # odds ratio
exp(confint.default(fit4b)["oc",]) # confidence interval for the odds ratio

#' 
#' We can see that the results suggest that use of oral contraceptives is differently associated
#' with MI depending on the smoking status. Indeed, the confidence intervals are quite different for the women who are current smokers, former smokers and those who have never smoked. Furthermore, in the output of the summary function we can read:
#'  
#'  - \texttt{oc:Smokeformer smoker   2.45852    0.73371   3.351 0.000806 ***}
#'  
#' This suggests that the association between MI and oral contraceptives is significantly different among women who have never smoked and among woman who are former smokers (p-value=0.000806). The parameter estimate 2.45852 represent the log of the ratio of the two odds ratios, i.e.:
## -----------------------------------------------------------------------------
log(38.25/3.27)

#' 
#' Note: the above results does not match "exactly" the parameter estimate 2.45852 here only because we have used the estimates of the two odds ratios 38.25 and 3.27 rounded at the second digits (not their exact estimated value).
#' 
#' 
#' ## Question 6
#' 
#' We now make some boxplots to compare the distribution of ages in the six groups of women defined by all possible combinations of smoking status and use of oral contraceptives.
#' 
#' We first make a new variables to indicate in which of the six groups each woman belongs (and look at how many women there are in each group).
## -----------------------------------------------------------------------------
MI$group <- interaction(MI$oc,MI$Smoke)
table(MI$group)

#' 
#' We can now easily make the boxplots. We use the red color for users of oral contraceptives and green for the others. 
## ---- fig.height=5,fig.width=11-----------------------------------------------
boxplot(MI$age~interaction(MI$oc,MI$Smoke),
        xlab="Group",
        ylab="Age",
        col=rep(c("forestgreen","red"),3))

#' 
#' From the boxplot we can see that when we compare women who use oral contraceptives to those who do not, both having the same smoking status, the two groups are not necessarily similar with respect to age. For instance, current smokers who use oral contraceptives seem to be often younger than current smokers who do not use oral contraceptives. Hence, although we found at the previous question that:
#'  
#'  - the risk of MI of the user of oral contraceptives is significantly higher to that of women who do not use contraceptives, among current smokers (OR=5.20, 95% CI=[2.49;10.86], p-value<0.0001);
#'  
#' from a purely statistical point of view, it is difficult to say whether the difference in risk of MI that we see between these two groups comes from the use of oral contraceptives or from the difference in age (or maybe another difference between the two groups). However, we can reasonably think that if the two compared groups had been similar with respect to age, we would have seen an even larger difference in the risk of MI. Indeed, those who use contraceptives are younger and it is known that aging (in its own) increases the risk of MI.
#' 
#' # Part II
#' 
#' We now proceed to the main analysis of the data, which aims to shed light on the research
#' question and lead to the main conclusions. We will estimate a model similar to that of question 5 but we will additionally adjust on age. Hence, we will be able to compare women who use contraceptives to those who do not, among women who are similar with respect to smoking and age.
#' 
#' 
#' ## Question 7
#' 
#' We first create add and to the data a categorical variable **AgeGroup**, which is a categorical variable for age, with three groups 15-39, 40-55 and 55 or above. We further use the **table()** function to read the number of observations in each group.
## -----------------------------------------------------------------------------
MI$AgeGroup <- cut(MI$age,
                   breaks=c(15,40,55,100),
                   include.lowest=TRUE)
table(MI$AgeGroup)

#' 
#' ## Question 8
#' 
#' We now estimate a (multiple) logistic model to model the risk of myocardial infarction (**mi**) using the three variables corresponding to smoking status (**Smoke**), use of oral contraceptives (**oc**) and age group (**AgeGroup**). We model an interaction between smoking status (**Smoke**) and use of oral contraceptives (**oc**)
## -----------------------------------------------------------------------------
fit5 <- glm(mi ~ oc * Smoke + AgeGroup, data=MI, family=binomial)
summary(fit5)
publish(fit5)

#' 
#' 
#' This model seems reasonable for the main analysis for the following reasons:
#'  
#'  - it enables us to study the association between use of oral contraceptives and MI.
#'  - we adjust on age group and smoking status to compare the risk of MI between women who use and those who do not use contraceptives, who are otherwise similar with respect to smoking status and age (group). This seems sensible because 1) previous studies suggest that both smoking status and age influence the risk of MI and 2) the study design (observational study) does not ensure that the group of women who use contraceptive is "similar" to that of those who do not, in terms of age and smoking status.
#'  - as we use an interaction between smoking and use of contraceptives, we do not assume that the association between use of oral contraceptives and MI is the same in all smoking groups (i.e. we do not force the model to estimate the same association between contraceptives and MI in all smoking groups).
#'  
#'  
#' From this model, we estimate that when comparing the risk of MI of a user of oral contraceptives to that of a woman who does not use oral contraceptives:
#'  
#'  - when both have **never smoked** and belong to the **same age group** (whatever it is), the risk of MI of the user of oral contraceptives is significantly higher (OR=4.39, 95% CI=[1.99;9.69], p-value=0.002).
#'  - when both are **current smokers** and belong to the **same age group** (whatever it is), the risk of MI of the user of oral contraceptives is significantly higher (OR=8.05, 95% CI=[3.59;18.04], p-value<0.0001).
#'  - when both are **former smokers** and belong to the **same age group** (whatever it is), the risk of MI of the user of oral contraceptives is significantly higher (OR=43.64, 95% CI=[12.37;153.91], p-value<0.0001).
#'  
#' There is a significant association in each smoking group, but it seems that its strength differs from one smoking group to another. For instance, the output of the summary function shows that the odds ratios modeling the association between MI and use of contraceptives seem different for former smokers and those who have never smoked (p-value=0.002449). The difference in odds ratio is not significant between current smokers and those who have never smoked (p-value=0.278952). Note that the third comparison of odds ratios, between current and former smokers is not shown in the output. To get it, you can simply refit the model after changing the reference level as shown below and see that the difference is again significant (p-value=0.02419).
#' 
## -----------------------------------------------------------------------------
MI$Smokeb <- relevel(MI$Smoke,ref="current smoker")
fit5b <- glm(mi ~ oc * Smokeb + AgeGroup, data=MI, family=binomial)
summary(fit5b)

#' 
#' # Part III
#' 
#' ## Question 9
#' 
#' We now perform a "sensitivity analysis" by changing the way the variable age enters the multiple logistic regression model. We now use the variable age as a continuous variable (assuming a linear effect of age). To make the interpretation somehow "easier", we create the variable **age20**, which is the variable **age** divided by 20.
#' 
## -----------------------------------------------------------------------------
MI$age20 <- MI$age/20
fit6 <- glm(mi ~ oc * Smoke + age20, data=MI, family=binomial)
publish(fit6)

#' 
#' We can see that the main results, that is, the results for the association between use of contraceptives and risk of MI, are relatively unchanged. Of course the estimated values are a bit different, but the confidence intervals that we obtain are very similar (in the sense that they overlap a lot) with this model and the main model. Hence the conclusions that we draw from this model or the main model are similar. The small differences between the results of the two models are irrelevant for the clinical interpretation of the results. This is somehow reassuring: the results do not heavily depend on the (somehow) arbitrary way we have modelled the effect of age.
#' 
