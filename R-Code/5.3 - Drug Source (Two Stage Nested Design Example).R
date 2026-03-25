# CHAPTER 5 - Nested Designs

# PROGRAM   5.3 - Drug Source (Two Stage Nested Design Example)

# SUMMARY   A researcher obtains three different cholesterol lowering drugs. 
#           Each drug is sourced from two different 
#           suppliers selected at random from all suppliers who make the 
#           drug. The researcher wants to know if the drugs differ in their 
#           effectiveness. It is possible that different suppliers 
#           will be better or worse at making the same drug. So the researcher 
#           also uses the factor 'supplier' to account for this possibility.  
#           Each drug from each supplier is given to five 
#           people (replicates) randomly selected from volunteers.
#
#           The data pertaining to these trials is shown below but is also
#           available in the csv entitled 'drugs'.
#
#           Drug X                  Drug Y                  Drug Z
#           SA        SB            SC        SD            SE        SF
#           21        15            14        6             17        7  
#           33        18            9         13            25        1 
#           18        34            11        13            20        4
#           14        23            22        21            18        16
#           27        26            5         18            31        10
#
#           Note that since only certain levels of Factor B (supplier) appear in
#           each level of Factor A (Drug) this is a nested design.
#
#           We wish to test whether the choice of drug has any effect on the
#           level of cholesterol
#             H0: Choice of drug has no effect on cholesterol levels
#             H1: Choice of drug has an effect on cholesterol levels
#
#           We also wish to determine the proportion of variation (after 
#           controlling for Drug) that arise from different random factors.
#
#           This program is structured as follows.
#
#           Section 1 - Read in the data
#           Section 2 - Checking that the ANOVA conditions are satisfied 
#           Section 3 - Obtaining ANOVA results using the lme() command
#           Section 4 - Obtaining ANOVA results using the aov() command






# Clear all variables before starting
rm(list = ls())




# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data 

# 1.1a - Read in the 'drugs.csv' data set 
drugs <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 5 - Nested Designs\\drugs.csv")
drugs




# SECTION 2 #
#------------------------------------------------------------------------------#
# Checking that the ANOVA conditions are satisfied 

# 2.1a - Fitting the model. Make sure you have installed the nlme package before 
# running. Note the 'random=~1|supplier' option specifies 
# that supplier should be treated as a random effect rather than a fixed effect.
library(nlme)
fit1 <- lme(chol ~ drug, random=~1|supplier, data=drugs)


# 2.1b - Obtaining a fitted vs residual plot and a QQ-plot

  # Create two graphs side by side
  par(mfrow=c(1,2))

  # Create residual vs fitted value plot
  plot(fit1$res ~ fit1$fitted, xlab="Fitted values", ylab="Residuals"); 
    abline(h=0, lty=2, col="red")
  
  # Create QQ plot
  qqnorm(fit1$res, main=" ", ylab="Residuals", xlab="Quantiles of Std. Normal"); 
    qqline(fit1$res)

    ## Fitted vs Residual plot indicates constant variance
    ## QQ Plot suggests that their may be a departure from normality in the tails
    ## of the distribution.



# 2.1c - Conducting a Shapiro-Wilks test for normality
shapiro.test(fit1$res)

    ## W = 0.97732, p-value = 0.3261
    ## No evidence against the null hypothesis that the data is normally 
    ## distributed









# SECTION 3 #
#------------------------------------------------------------------------------#
# Obtaining ANOVA results using the lme() command

# 3.1a - Obtaining the ANOVA table
anova(fit1)

    ##                 numDF  denDF  F-value    p-value
    ## (Intercept)     1      24     47.83008   <.0001
    ## drug            2      3      1.48014    0.3571

    ## F-Statistic = 1.48014, P-value = 0.3571
    ## No evidence against the null hypothesis, i.e. no evidence to show that
    ## the drugs differ in reducing cholesterol level.

    # Check F-statistic
    1-pf(47.583008,2,3)

# 3.1b - Obtaining the summary table
summary(fit1)

    ##             (Intercept)  Residual    Total
    ## StdDev:     5.27952      6.47302
    ## Variance:   27.87333     41.90000    69.77332

    ## Factor B Proportion = 27.87333 / 69.77332 = 39.9%
    ## Residual Proportion = 41.90000 / 69.77332 = 60.1%

    ## 39.9% of the variation after allowing for Drug difference is due to
    ## Source differences.
    
    ## 60.1% of the variation after allowing for drug is due to unexplained 
    ## residual variation.







# SECTION 4 #
#------------------------------------------------------------------------------#
# Obtaining ANOVA results using the aov() command

# 4.1a - Creating the model. Note this will produce a WARNING message.
drugs
fit2 <- aov(chol ~ drug + Error(drug:supplier) , data=drugs)


# 4.1b - Obtaining required statistics
summary(fit2)

    ## Error: drug:source
    ##            Df Sum Sq   Mean Sq   F value   Pr(>F)
    ## drug       2  536.6   268.3      1.48      0.357
    ## Residuals  3  543.8   181.3

    ## Error: Within
    ##            Df  Sum Sq  Mean Sq   F value   Pr(>F)
    ## Residuals  24  1006    41.9  

    ## F-Statistic = 1.48, P-value = 0.357 (same as above)
    ## No evidence against the null hypothesis, i.e. no evidence to show that
    ## the drugs differ in reducing cholesterol level.

    ## MS_A = 268.3 = \hat{\sigma}^2 + n \hat{\sigma}^2_B + bnv(\hat{\alpha})
    ## MS_B = 181.3 = \hat{\sigma}^2 + n \hat{\sigma}^2_B
    ## MSE  = 41.9  = \hat{\sigma}^2

    ## Re-arranging gives \hat{\sigma}^2_B = 27.874

    ## Factor B Proportion = 27.874 / (27.874 + 41.9) = 39.9%
    ## Residual Proportion = 41.9   / (27.874 + 41.9) = 60.1%
    
    ## 39.9% of the variation after allowing for Drug difference is due to
    ## Source differences.

