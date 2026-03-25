# CHAPTER 4 - Randomised Block Design

# PROGRAM   4.5 - Clotting Time of Blood Plasma (Permutation Test RBD Example)

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
#           Section 2 - Showing that the ANOVA conditions are violated
#           Section 3 - Conducting a Permutation Test using the lmPerm package




# Clear all variables before starting
rm(list = ls())



# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data 

# 1.1a - Read in the 'plasma.csv'
plasma <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 4 - Randomised Block Design\\plasma.csv")
plasma
str(plasma)

# 1.1b - Change the variables 'Subject' and 'Treatment' to categorical variables
plasma$Subject <- factor(plasma$Subject)
plasma$Treatment<-factor(plasma$Treatment)
str(plasma)





# SECTION 2 #
#------------------------------------------------------------------------------#
# Showing that the ANOVA conditions are violated

# 2.1a - Fitting a linear model to the data
# The as.factor() command ensures that Subject and Treatment are treated as
# categorical variables
fit1 <- lm(Clottime ~ Subject + Treatment, data=plasma)



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
    ## As such, in the next section we conduct a permutation test on the data.






# SECTION 3 #
#------------------------------------------------------------------------------#
# Conducting a Permutation Test using the lmPerm package

# Make sure you have installed the lmPerm package before running. 
library(lmPerm)

# 3.1a - Conducting the permutation test. For RBDs permutations must
# be done within each block. We use the aovp() and the Error() commands to 
# achieve this.
plasma
set.seed(3141579)
fit3 <- aovp(Clottime ~ Treatment + Error(Subject/Treatment), data=plasma)


# 3.1b - obtaining the results of the permutation test
summary(fit3)

    ##            Df  Sum Sq    Mean Sq   Iter  Pr(Prob)  
    ## Treatment  3   13.016    4.3388    5000   0.0704 .
    ## Residuals  21  13.774    0.6559   

    ## The permutation test indicates only weak evidence of differences among 
    ## treatments since p-value < 0:1. This differs from the conclusions reached 
    ## using fixed or mixed effect models. 

    #  However, since normal theory assumptions were not met in either case,
    #  those results can be considered unreliable.


