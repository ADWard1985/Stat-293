# CHAPTER 5 - Nested Designs

# PROGRAM   5.5 - Burger Fat (Three Stage Nested Design Example)

# SUMMARY   An experiment is conducted to study the fat content of popular 
#           burgers from a large fast food restaurant chain. Particular interest 
#           centers on whether fat content varies significantly with geographic 
#           region, town or restaurant. Three towns are randomly selected in the 
#           North and three more are selected in the South. In each town, three 
#           restaurants are randomly chosen and at each restaurant, five burgers 
#           are randomly chosen and prepared in a typical fashion. Each burger 
#           is then ground up, and the total amount of fat is determined. The 
#           response is expressed in a coded scale for computational convenience.
#
#           The data pertaining to these trials is available in the csv data set
#           entitled 'burger'.
#
#           This is a three way nested design since Town (Factor B) is nested
#           within Region (Factor A), and Restaurant (Factor C) is nested within
#           Town.
#
#           We wish to test whether Region has any effect on the quantity of
#           fat within the burgers
#             H0: Region has no effect on fat quantity
#             H1: Region has an effect on fat quantity
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

# 1.1a - Read in the 'burger.csv' data set 
burger <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 5 - Nested Designs\\burger.csv")
burger




# SECTION 2 #
#------------------------------------------------------------------------------#
# Checking that the ANOVA conditions are satisfied 

# 2.1a - Fitting the model. Make sure you have installed the nlme package before 
# running. Note the 'random=~1|town/rest' option specifies 
# that town & rest should be treated as a random (rather than fixed) effects.
library(nlme)
fit1 <- lme(fat ~ region, random=~1|town/rest, data=burger)


# 2.1b - Obtaining a fitted vs residual plot and a QQ-plot
par(mfrow=c(1,2))
plot(fit1$res ~ fit1$fitted, xlab="Fitted values", ylab="Residuals"); abline(h=0, lty=2, col="red")
qqnorm(fit1$res, main=" ", ylab="Residuals", xlab="Quantiles of Std. Normal"); qqline(fit1$res)

    ## Fitted vs Residual plot indicates assumption of constant variance may be
    ## in doubt. However, departure is not severe enough to cause concern.
    ## QQ Plot suggests assumption of normality has been met.



# 2.1c - Conducting a Shapiro-Wilks test for normality
shapiro.test(fit1$res)

    ## W = 0.99438, p-value = 0.4203
    ## No evidence against the null hypothesis that the data is normally 
    ## distributed









# SECTION 3 #
#------------------------------------------------------------------------------#
# Obtaining ANOVA results using the lme() command

# 3.1a - Obtaining the ANOVA table
anova(fit1)

    ##                 numDF  denDF  F-value    p-value
    ## (Intercept)     1      72     60.79804   <.0001
    ## region          1      4      0.16176    0.7081

    ## F-Statistic = 0.16176, P-value = 0.7081
    ## No evidence against the null hypothesis, i.e. no evidence to show that
    ## the mean fat content of burgers differs between regions



# 3.1b - Obtaining the summary table
summary(fit1)

    ## \sigma_B = 1.944218      \sigma_B^2 = 3.779984
    ## \sigma_C = 1.198628      \sigma_C^2 = 1.436709
    ## \sigma   = 1.458688      \sigma^2   = 2.127771

    #   \sigma_B^2 / (\sigma_B^2 + \sigma_C^2 + \sigma^2)
    #   = 3.779984 / 7.344463
    #   = 51.5%

    #   \sigma_C^2 / (\sigma_B^2 + \sigma_C^2 + \sigma^2)
    #   = 1.436709 / 7.344463
    #   = 19.6%

    #   \sigma^2   / (\sigma_B^2 + \sigma_C^2 + \sigma^2)
    #   = 2.127771 / 7.344463
    #   = 29.0%






# SECTION 4 #
#------------------------------------------------------------------------------#
# Obtaining ANOVA results using the aov() command

# 4.1a - Creating the model. Note this will produce a WARNING message.
burger
fit2 <- aov(fat ~ region + Error(region:town + town:rest), data=burger)


# 4.1b - Obtaining required statistics
summary(fit2)

    ## Error: region:town
    ##            Df Sum Sq   Mean Sq   F value   Pr(>F)
    ## Region     1  10.68    10.68     0.162     0.708
    ## Residuals  4  264.04   66.01 

    ## Error: rest
    ##            Df  Sum Sq  Mean Sq   F value   Pr(>F)
    ## Residuals  12  111.7   9.311

    ## Error: Within
    ##            Df  Sum Sq  Mean Sq   F value   Pr(>F)
    ## Residuals  72  153.2   2.128   

    ## F-Statistic = 0.162, P-value = 0.708 (same as above)
    ## No evidence against the null hypothesis, i.e. no evidence to show that
    ## the mean fat content of burgers differs between regions.

    ## MSA = 10.68
    ## MSB = 66.01
    ## MSC = 9.311
    ## MSE = 2.128


