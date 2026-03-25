# CHAPTER 3 - Permutation Testing

# PROGRAM   3.2 - Student IQ Tests (ANOVA Example with Checks)

# SUMMARY   A group of researchers are interested in whether the mean IQs of 
##          students studying different degrees differ. They take a sample of 1,000
##          Art, Biology, Chemistry, Philosophy and Statistics students and make
##          them sit an IQ test. The data pertaining to these tests is contained 
##          in the csv file entitled 'IQ Tests (All Subjects)'.
##
##          This program is arranged as follows:
##
##          Section 1 - Reading in the data
##          Section 2 - Graphically assessing whether ANOVA conditions hold
##          Section 3 - Numerically assessing whether ANOVA conditions hold
##          Section 4 - Fitting the ANOVA Model
##          Section 5 - Assessing whether ANOVA conditions hold via residual
##                      analysis
##          Section 6 - Assessing whether ANOVA conditions hold with 
##                      statistical tests
##          Section 7 - Conducting ANOVA to test between the hypotheses:
##
##                      H0: There is no difference in the mean IQ of students 
##                          studying different degrees. 
##
##                      H1: There is a difference in the mean IQ of students 
##                          studying different degrees.
##
##                      We then conduct post-hoc Tukey Honestly Significant 
##                      Difference tests to determine which groups of students 
##                      have significantly different mean IQs.




# Clear all variables before starting
rm(list = ls())




# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data

# 1.1a - Read in the 'IQ Tests (All Subjects).csv' data set 
IQ1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 3 - Permutation Testing\\IQ Tests (All Subjects).csv")
IQ1a


# 1.1b - Refining to required variables, i.e. dropping the 'Student' column
IQ1b = subset(IQ1a, select = c(Art, Biology, Chemistry, Philosophy, Statistics))
IQ1b





# SECTION 2 #
#------------------------------------------------------------------------------#
# Graphically assessing whether the ANOVA conditions hold

# 2.1a - Creating a box plots of the data in each group
boxplot(IQ1b, col = rainbow(ncol(IQ1b)) ,  ylab='IQ Score')

# 2.1b - Creating histograms of the data in each group. Make sure you have 
# installed the Hmisc package
library(Hmisc)
hist.data.frame(IQ1b)


    ## Box-plot and histograms do not suggest that the ANOVA conditions are
    ## violated. All data looks approximately normally distributed with constant
    ## variance.




# SECTION 3 #
#------------------------------------------------------------------------------#
# Numerically assessing whether the ANOVA conditions hold

# 3.1a - Obtain Summary statistics of the data students in each group
IQ3a <- summary(IQ1b)
IQ3a

# 3.1b - Obtain the standard deviation of the IQs of students in each group
IQ3b <- sapply(IQ1b, sd)
IQ3b


    ## Variance in each group appears to be constant \approx 10




# SECTION 4 #
#------------------------------------------------------------------------------#
# Fitting the ANOVA model

# 4.1a - Stacking the variables on top of each other
IQ4a <- stack(IQ1b)
IQ4a


# 4.1b - Re-ordering the columns and renaming them
IQ4b <- IQ4a[, c("ind", "values")]          # Reorder columns
names(IQ4b)[names(IQ4b)=='ind'] <- 'Group'  # Rename ind = Group
names(IQ4b)[names(IQ4b)=='values'] <- 'IQ'  # Rename values = IQ
IQ4b


# 4.1c - Creating the linear model
IQ4c <- lm(IQ ~ Group, data=IQ4b)


  # Command below provides a list of the data sets stored within IQ4c
  names(IQ4c)






# SECTION 5 #
#------------------------------------------------------------------------------#
# Assessing whether the ANOVA conditions hold via residual analysis

# 5.1a - Obtaining a plot of residuals vs fitted values and QQ plot of residuals 
  
  # Create two plots side by side
  par(mfrow=c(1,2))
  
  # Create Residual vs Fitted Value Plot
  plot(IQ4c$res~IQ4c$fit,xlab="Fitted value",ylab="Residuals")
  abline(h=0,col="gray")
  
  # Create QQ-Plot
  qqnorm(IQ4c$res,main="")
  qqline(IQ4c$res)

  ## Plot of residuals vs fitted values suggests constant variance across groups
  ## QQ plot suggests residuals are normally distributed.
   

  
  
  
  
  
# SECTION 6 #
#------------------------------------------------------------------------------#
# Assessing whether the ANOVA conditions hold using statistical tests

#6.1a - Conducting Levene's test for homogeneous variance. Make sure you 
# have the "car" package installed.
library(car)
leveneTest(IQ4b$IQ, as.factor(IQ4b$Group))

    ##    Df        F value   Pr(>F)
    ##    4,4995    1.1803    0.3173
    ## => No evidence against the null hypothesis that the variance in each
    ## groups is constant.






# SECTION 7 #
#------------------------------------------------------------------------------#
# In the previous sections we have shown that the ANOVA conditions hold. In this
# section we conduct ANOVA and post-hoc tests to determine:
#   i.  If there is a difference in mean between some groups
#   ii. Which groups exhibit significantly different means.
# Note that the ANOVA model has already been constructed in Section 4

# 7.1a - Obtaining the parameter estimates of the ANOVA model
summary(IQ4c)

    ##  Coefficients:
    ##                    Estimate    Std. Error  t value   Pr(>|t|)    
    ##  (Intercept)       98.4000     0.3151      312.233   < 2e-16
    ##  GroupBiology      0.7480      0.4457      1.678     0.0934 
    ##  GroupChemistry    1.4550      0.4457      3.265     0.0011
    ##  GroupPhilosophy   2.9130      0.4457      6.536     6.95e-11
    ##  GroupStatistics   3.6340      0.4457      8.154     4.42e-16

    ## F-statistic: 22.79 on 4 and 4995 DF,  p-value: < 2.2e-16


# 7.1b - Obtaining the summary ANOVA table
anova(IQ4c)

    ##            Df    Sum Sq    Mean Sq   F value     Pr(>F)    
    ## Group      4     9056      2263.89   22.794      < 2.2e-16 ***
    ## Residuals  4995  496098    99.32 

    ## ANOVA indicates that there is strong evidence against the null hypothesis
    ## that the means of all groups are equal. We therefore proceed to post-hoc
    ## tests to determine which groups have different means.
    


# 7.1c - Conducting post-hoc Tukey HSD tests
IQ7c <- aov(IQ ~ Group, data=IQ4b)
TukeyHSD(IQ7c)

    ##                        diff    lwr       upr       p adj
    ## Biology-Art            0.748 -0.4681808  1.964181  0.4475197
    ## Chemistry-Art          1.455  0.2388192  2.671181  0.0097299
    ## Philosophy-Art         2.913  1.6968192  4.129181  0.0000000
    ## Statistics-Art         3.634  2.4178192  4.850181  0.0000000
    ## Chemistry-Biology      0.707 -0.5091808  1.923181  0.5062442
    ## Philosophy-Biology     2.165  0.9488192  3.381181  0.0000121
    ## Statistics-Biology     2.886  1.6698192  4.102181  0.0000000
    ## Philosophy-Chemistry   1.458  0.2418192  2.674181  0.0095121
    ## Statistics-Chemistry   2.179  0.9628192  3.395181  0.0000103
    ## Statistics-Philosophy  0.721 -0.4951808  1.937181  0.4859925





