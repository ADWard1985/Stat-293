# CHAPTER 4 - Randomised Block Design

# PROGRAM   4.4 - Clotting Time of Blood Plasma (Mixed Effects RBD Example)

# SUMMARY   The overall effects of four different treatments on the clotting 
#           time of blood plasma are to be investigated. To control for
#           underlying differences in clotting rates among individuals, a 
#           mixed effects RBD was used with Subjects as blocks. Four 
#           samples of plasma were collected from each of eight subjects (the 
#           'blocks').The four treatments were then assigned in random order to 
#           the four samples of each subject, ie each subject received all 
#           four treatments. The csv data set entitled "plasma" gives the time 
#           to clotting in minutes resulting from these experiments.
#
#           This program is structured as follows:
#
#           Section 1 - Read in the data
#           Section 2 - Conducting mixed effects RBD ANOVA on the data




# Clear all variables before starting
rm(list = ls())



# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data 

# 1.1a - Read in the 'plasma.csv' data set
# stored as dataframe
plasma <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 4 - Randomised Block Design\\plasma.csv")
plasma
str(plasma)

# 1.1b - Change the variables 'Subject' and 'Treatment' to categorical variables
plasma$Subject <- factor(plasma$Subject)
plasma$Treatment<-factor(plasma$Treatment)
str(plasma)





# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting Mixed Effects RBD ANOVA on the data

# 2.1a - Fitting a linear model to the data. Make sure you have installed the
# nlme package before running. Note the 'random=~1|Subject' option specifies 
# that Subject should be treated as a random effect rather than a fixed effect.
library(nlme)
fit2 <- lme(Clottime ~ Treatment, random=~1|Subject, data=plasma)


# 2.1b - Obtain Residual vs fitted value plot and Q-Q Plot
  
    # Create two graphs side by side
    par(mfrow=c(1,2))
    
    # Create Residual vs Fitted value plot
    plot(fit2$res ~ fit2$fitted, xlab="Fitted values", ylab="Residuals")
    abline(h=0, lty=2, col="grey")
    
    # Create QQ-Plot
    qqnorm(fit2$res, main=" ", ylab="Residuals",
       xlab="Quantiles of Standard Normal"); qqline(fit2$res)


    ## Plot of residuals vs fitted values suggests non-constant variance
    ## Q-Q plot suggests non-normality at the top end of the distribution
    ## It is likely that the RBD conditions (independence, normality and
    ## homogenous variance within each cell) are violated. Consequently, the
    ## p-values produced in the RBD ANOVA process are likely to be invalid.
    ## Although a permutation test may be more appropriate, our aim here is
    ## to illustrate mixed effects RBD ANOVA.




# 2.1c - Obtain the ANOVA table
anova(fit2)

    ## Analysis of Variance Table
    ##              numDF denDF   F-value     p-value
    ## (Intercept)  1     21      283.23032   <.0001
    ## Treatment    3     21      6.61503     0.0025 
      
    ## Strong evidence against the null hypothesis.
    ## There is strong evidence that treatment effects differ.
    ## Note that the F-statistic, dfs and p-value for treatment are exactly the
    ## same as we had in the fixed effects model.
  
  

# 2.1d - Obtain model coefficients and estimates of standard deviations 
summary(fit2)
  
    ## Coefficients:
    ## Fixed effects:  Clottime ~ Treatment 
    ##              Value   Std.Error   DF  t-value   p-value
    ## (Intercept)  9.3000  0.6435202   21  14.45176  0.0000
    ## Treatment2   0.4125  0.4049361   21  1.018679  0.3199
    ## Treatment3   0.6375  0.4049361   21  1.574323  0.1304
    ## Treatment4   1.7250  0.4049361   21  4.259932  0.0003
  
    ## Standard Deviations
    ##            Block         Residual      Total
    ## StdDev:    1.630047      0.8098721

    ## Block Proportion    = 1.630047^2  / (1.630047^2 + 0.8098721^2) = 0.802
    ## Residual Proportion = 0.8098721^2 / (1.630047^2 + 0.8098721^2) = 0.198





