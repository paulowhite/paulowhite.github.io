rm(list=ls())

# Scenarion A
# The first p-value is above 5%. We reject none !

# Scenarion B
# We reject all those of steps 1-5. Indeed, for step 1-3, they are all below 5%.
# For Step 4 and 5, they are below 5/3=1.667. For step 6, the significance level
# is 5/4=1.25%. Hence reject only two of the four (Koos pain score and EQ5D pain score)

# Scenarion C
# We reject at steps 1-3, as they are all below 5%. At step 4, we fail to reject
# for Range of Movement  at t=24 months, as p=3.2% > 5%/3 = 1.667%. At step 5, the
# significance level is therefore 5%*(2/3)=3.33%. We are allowed to keep testing
# because at least one was significant at step 4, but no longer with full alpha at 5%.
# Because of Bonferroni, we compare the p-values to 5%*(2/3)/3=1.11%. So we can reject
# all at step 5, although it was borderline for SF36 bodily pain score. At step 6, the  
# significance level is therefore still 3.33%, as 5%*(2/3)*(3/3)=3.33%. As all were
# significant at step 5, the significance level stays unchanged. Because of Bonferroni,
# we compare the p-values to 5%*(2/3)/4=0.83%. Hence, 2 are still significant
# (KOOS pain score, p=0.1% and EQ5D pain score, p=0.6%) but 2 are not (SF36 vitality
# score, p=1.5% and UCLA activity scale, p=40%).
