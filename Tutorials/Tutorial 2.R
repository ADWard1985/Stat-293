# Tutorial 2



# Question 2
#------------------------------------------------------------------------------
rm(list = ls())

# 3a - Calculate SST
SST <- 45*4.59^2 + 102*4.88^2 + 18 * 6.24^2  - (45*4.59 + 102*4.88 + 18*6.24)^2 / (45 + 102 + 18)
SST

# 3a - Calculate MST
MST = SST / (3-1)
MST

# 3a - Calculate SSE
SSE <- 44*0.70^2 + 101*0.64^2 + 17 * 0.90^2
SSE

# 3a - Calculate MSE
MSE = SSE / (45+102+18-3)
MSE

# 3a - Calculate F-statistic
Fval <- MST / MSE
Fval

# 3a - Calculate p-value. Note df for MST = 2, df for MSE = 162
pval <- 1 - pf(Fval, 2, 162)
pval


# 3b - Conclusions
    # H0 : \mu_1 = \mu_2 = \mu_3        (all means are equal)
    # H1 : \mu_i != \mu_j for some i,j  (not all means are equal)
    # F-statistic = 0.4734543 with 2 and 162 degrees of freedom.
    # P-value = 2.364775e-14
    # Very strong evidence against the null hypothesis





# Question 4
#------------------------------------------------------------------------------
rm(list = ls())

# 4a.1 - Read in the crdhoneycough data set
honey <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\Tutorials\\Tutorial Data\\crdhoneycough.csv")
honey

# 4a.2 - Construct boxplot
boxplot(score ~ treatment, data=honey)
  
    # The boxplot indicates that mean changes in symptom scores MAY differ among 
    # groups, with honey leading to greater improvement compared to the other 
    # treatments

# 4a.3 - Calculate means for the different groups
tapply(honey$score, honey$treatment, mean)

# 4a.4 - Calculate standard deviations
tapply(honey$score, honey$treatment, sd)





# 4b - Fit a linear model to the data
fit1 <- lm(score~treatment, data=honey)
summary(fit1)

    # \hat{µ}_H - \hat{µ}_C = 4.2008 indicating that mean score change for honey 
    # is higher (based on positive value) than that for the control group by 
    # 4.2008.




# 4c - Estimates of group means from previous summary table

    # \hat{µ}_C  = 6.5135
    # \hat{µ}_DM - \hat{µ}_C = 1.8198   =>  \hat{µ}_DM = 8.3333
    # \hat{µ}_H  - \hat{µ}_C = 4.2008   =>  \hat{µ}_H  =  10.7143

# Note that these are precisely the values obtained in part 4a.3




# 4d - Perform ANOVA test
anova(fit1)
    # H_0 : \mu_C = \mu_DM = \mu_H            (all means are equal)
    # H_1 : \mu_i != \mu_j for      some i,j  (not all means are equal)
    # F = 17.51, df = (2,102), p = 2.902e^{-7}
    # Very strong evidence against null hypothesis




# 4e.1 - Conduct pairwise Tukey HSD tests
TukeyHSD(aov(score~treatment,data=honey))
    #     diff      lwr      upr      p adj
    #DM-C 1.819820 0.1023625 3.537277 0.0351562
    #H-C  4.200772 2.5094509 5.892094 0.0000001
    #H-DM 2.380952 0.6405157 4.121389 0.0043728

    # There is sufficient evidence that all treatment pairs differ in mean 
    # scores. The greatest difference is between honey and no treatment.

# 4e.2 - Plotting the results
plot(TukeyHSD(aov(score~treatment,data=honey)))





# 4f - Overall conclusion:
# Based on these results, buying the over-the-counter cough remedy would seem to 
# be a waste of money! Parents would be better off using the honey they have at 
# home as it resulted in higher symptom improvement scores compared to the 
# over-the-counter cough remedy.







