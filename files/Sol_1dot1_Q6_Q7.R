# Question 6
0.4*0.3 + 0.6*0.075
0.4*0.2 + 0.6*0.05

# Question 7

# advantage: sample size could be smaller
the.n.SS <- ceiling(power.prop.test(p1=0.3,
                                    p2=0.2,
                                    sig.level=alpha,
                                    power=power)$n)
2*the.n.SS

# disadvantage: trial duration
# duration of the trial if we include all (both patients in septic shock and not in septic shock)
the.n*2/130

# duration of the trial if we inluce only patients in spetic shock
the.n.SS*2/(0.4*130)

the.n.SS/the.n # 0.56% of the total sample size, not 40% ... so patients  not in septic shock
# are not as "informative" as those in septic shock to show treatment efficacy,
# but still they help. Including them is better than not including them.
