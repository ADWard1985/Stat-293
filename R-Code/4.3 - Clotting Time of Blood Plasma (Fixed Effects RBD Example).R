# CHAPTER 4 - Randomised Block Design

# PROGRAM   4.3 - Clotting Time of Blood Plasma (Fixed Effects RBD Example)

# SUMMARY   The overall effects of four different treatments on the clotting 
#           time of blood plasma are to be investigated. To control for
#           underlying differences in clotting rates among individuals, a 
#           fixed effects RBD was used with Subjects as blocks. Four 
#           samples of plasma were collected from each of eight subjects (the 
#           'blocks').The four treatments were then assigned in random order to 
#           the four samples of each subject, ie each subject received all 
#           four treatments. The csv data set entitled "plasma" gives the time 
#           to clotting in minutes resulting from these experiments.
#
#           This program is structured as follows:
#
#           Section 1 - Read in the data
#           Section 2 - Conducting Fixed Effects RBD ANOVA on the data
#           Section 3 - Conducting One-Way ANOVA on the data to illustrate the
#                       effect of ignoring subject-to-subject variation in the
#                       data.


# Clear all variables before starting
rm(list = ls())




# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data 

# 1.1a - Read in the 'plasma.csv' data set
plasma <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 4 - Randomised Block Design\\plasma.csv")
plasma



# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting Fixed Effects RBD ANOVA on the data

# 2.1a - Fitting a linear model to the data
# The as.factor() command ensures that Subject and Treatment are treated as
# categorical variables
fit1 <- lm(Clottime ~ as.factor(Subject) + as.factor(Treatment), data=plasma)



# 2.1b - Obtain Residual vs fitted value plot and Q-Q Plot
  
  # Create two graphs side by side
  par(mfrow=c(1,2))
  
  # Create Residual vs Fitted value plot
  plot(fit1$res ~ fit1$fitted, xlab="Fitted values", ylab="Residuals")
  abline(h=0, lty=2, col="grey")
  
  # Create QQ-Plot
  qqnorm(fit1$res, main=" ", ylab="Residuals",
       xlab="Quantiles of Standard Normal"); qqline(fit1$res)


    ## Plot of residuals vs fitted values suggests non-constant variance
    ## Q-Q plot suggests non-normality at the top end of the distribution
    ## It is likely that the RBD conditions (independence, normality and
    ## homogenous variance within each cell) are violated. Consequently, the
    ## p-values produced in the RBD ANOVA process are likely to be invalid.
    ## Although a permutation test may be more appropriate, our aim here is
    ## to illustrate RBD ANOVA.




# 2.1c - Obtain the ANOVA table
anova(fit1)

    ## Analysis of Variance Table
    ##                        Df  Sum Sq  Mean Sq   F value   Pr(>F)    
    ## as.factor(Subject)     7   78.989  11.2841   17.204    2.197e-07 ***
    ## as.factor(Treatment)   3   13.016  4.3388    6.615     0.00255 ** 
    ## Residuals              21  13.774  0.6559   
      
    ## Strong evidence against the null hypothesis.
    ## There is strong evidence that treatment effects differ after controlling
    ## for subject-to-subject variation in clotting time.
  
    # Check p-value
    1-pf(6.615,3,21)

# 2.1d - Obtain model coefficients from the summary table
summary(fit1)
  
    ## Coefficients:
    ##                        Estimate    Std. Error   t value  Pr(>|t|)    
    ## (Intercept)             9.2562     0.4748       19.494   6.26e-15 ***
    ## as.factor(Subject)2     3.8750     0.5727       6.767    1.08e-06 ***
    ## as.factor(Subject)3    -0.0250     0.5727      -0.044    0.965591    
    ## as.factor(Subject)4     0.1750     0.5727       0.306    0.762927    
    ## as.factor(Subject)5    -1.5500     0.5727      -2.707    0.013214 *  
    ## as.factor(Subject)6    -0.1500     0.5727      -0.262    0.795925    
    ## as.factor(Subject)7    -0.5750     0.5727      -1.004    0.326772    
    ## as.factor(Subject)8    -1.4000     0.5727      -2.445    0.023404 *  
    ## as.factor(Treatment)2   0.4125     0.4049       1.019    0.319948    
    ## as.factor(Treatment)3   0.6375     0.4049       1.574    0.130359    
    ## as.factor(Treatment)4   1.7250     0.4049       4.260    0.000349 ***
  
  


# SECTION 3 #
#------------------------------------------------------------------------------#
# Conducting One-Way ANOVA on the data, i.e. ignoring subject-to-subject variation

# 3.1a - Fitting the linear model with clottime as a function of treatment only
fit2 <- lm(Clottime~ as.factor(Treatment), data=plasma)

# 3.1b - Obtain the ANOVA table
anova(fit2)

  # Analysis of Variance Table
  #                       Df  Sum Sq  Mean Sq   F value   Pr(>F)
  # as.factor(Treatment)  3   13.016  4.3388    1.3096    0.2909
  # Residuals             28  92.762  3.3129  

  # The 'Treatment' row of the one-way ANOVA table is the same as before, but 
  # the MSE (unexplained error variance) is higher (3.3129 instead of 0.6559). 
  # This is because with the simpler one-way model the variation due to the 
  # nuisance variable (Subject) has been included in the unexplained error, 
  # inflating it considerably. The net result of not allowing for people 
  # (blocks) as a source of variation is to make it impossible to detect 
  # the treatment effect we are interested in.





