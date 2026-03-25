# CHAPTER 4 - Randomised Block Design

# PROGRAM   4.6 - Cholesterol Study (RBD Example)

# SUMMARY   A study is undertaken to determine whether there is any difference
#           in the ability of two prescription drugs (Atorvastatin and Pravastatin)
#           to reduce the level of cholesterol in the blood stream of patients.
#           Since cholesterol levels are known to increase with age, the age
#           of study participants is a nuisance variable.
#
#           To control for this a randomised block design (RBD) has been employed.
#           Participants in the study were divided into one of 5 age groups:
#                   18-30     31-40     41-50     51-60     65+
#           There were 200 participants in each age group (block). Participants
#           in each block were randomly assigned one of the two treatments,
#           i.e. 100 from each block were assigned Atorvastatin and 100 were
#           assigned Pravastatin. It is assumed that there is no interaction 
#           between age and prescribed drug.
#
#           After three months of taking the drugs, the level of cholesterol
#           in the blood stream of each patient was measured. This data is
#           available in the csv entitled 'cholesterol' (and also available in
#           'wide' format in the csv entitled 'cholesterol - wide').
#
#           This program is structured as follows.
#
#           Section 1 - Read in the data.
#
#           Section 2 - Demonstrate that the ANOVA conditions of independence,
#           normality and homogeneous variance are satisfied in each cell.
#
#           We then wish to test between the hypotheses:
#               H0: Prescribed Drug has no effect on Cholesterol level
#               H1: Prescribed Drug has an effect on Cholesterol level
#
#           Section 3 - Construction of a fixed effects model
#
#           Section 4 - Construction of a mixed effects model
#
#           Section 5 - Construction of a permutation test. Note this is not
#           strictly needed as it it explicitly demonstrated in Section 2 that
#           the ANOVA conditions are satisfied. However, this is good practice.
#
#           All parametric and permutation tests yield strong evidence against
#           the null hypothesis. We therefore conclude that prescribed drug
#           has an effect on cholesterol levels. In particular, Atorvastatin
#           performs better than Pravastatin in this regard.





# Clear all variables before starting
rm(list = ls())


# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data 

# 1.1a - Read in the 'cholesterol.csv' 
Chol1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 4 - Randomised Block Design\\cholesterol.csv")
Chol1a
str(Chol1a)





# SECTION 2 #
#------------------------------------------------------------------------------#
# Demonstrating the the ANOVA conditions are met

# 2.1a - Read in the csv data set 'cholesterol wide' 
Chol2a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 4 - Randomised Block Design\\cholesterol wide.csv")
Chol2a


# 2.1b - Refining to required variables, i.e. dropping the 'Subject' column
Chol2b = subset(Chol2a, select = -c(Subject))
Chol2b


# 2.1c - Creating a box plots of the data in each group
boxplot(Chol2b, col = rainbow(ncol(Chol2b)) ,  ylab='Cholesterol Level')

  # Boxplot indicate roughly constant variance
  # Also indicates Cholesterol increases with age
  # Also suggests Cholesterol is lower with Atorvastatin vs Pravastatin


# 2.1d  - Creating histograms of the data in each cell. Make sure you have 
# installed the Hmisc package
library(Hmisc)
hist.data.frame(Chol2b)

  # Histograms suggest the data could arise from normal distribution


# 2.1e - Obtain Summary statistics of the data students in each cell
Chol2e <- summary(Chol2b)
Chol2e 


# 2.1f - Obtain the standard deviation of the IQs of students in each cell
Chol2f <- sapply(Chol2b, sd)
Chol2f

  # Summary statistics suggest constant variance. No relationship between mean
  # and variance in each cell


# 2.1g - Stacking the data and performing a Levene's test for homogenous 
# variance. Make sure you have the "car" package installed.
Chol2g <- stack(Chol2b)
Chol2g <- Chol2g[, c("ind", "values")]                    # Reorder columns
names(Chol2g)[names(Chol2g)=='ind'] <- 'Cell'             # Rename ind = Cell
names(Chol2g)[names(Chol2g)=='values'] <- 'Cholesterol'   # Rename values = Cholesterol
Chol2g

library(car)
leveneTest(Chol2g$Cholesterol, as.factor(Chol2g$Cell))

  # No evidence against the null hypothesis of homogeneous means



# 2.1h - Fit linear model and obtain residual vs fitted value plot and Q-Q plot
Chol2h <- lm(Cholesterol ~ Drug + Age, data=Chol1a)

    # Create two graphs side by side
    par(mfrow=c(1,2))
    
    # Construct residual vs fitted value plot
    plot(Chol2h$res ~ Chol2h$fitted, xlab="Fitted values", ylab="Residuals")
    abline(h=0, lty=2, col="grey")
    
    # Construct QQ-plot
    qqnorm(Chol2h$res, main=" ", ylab="Residuals",
           xlab="Quantiles of Standard Normal"); qqline(Chol2h$res)

  # Plot of residuals vs fitted values suggests constant variance
  # QQ plot suggests normality of the data




# SECTION 3 #
#------------------------------------------------------------------------------#
# Conducting Fixed Effects RBD ANOVA on the data

# 3.1a - Fitting a linear model to the data
Chol1a
Chol3a <- lm(Cholesterol ~ Drug + Age, data=Chol1a)

  

# 3.1b - Obtain the ANOVA table
anova(Chol3a)

    # Analysis of Variance Table
    #             Df  Sum Sq  Mean Sq   F value   Pr(>F)    
    # Drug        1   8521    8520.6    44.703    3.818e-11 ***
  
    # Strong evidence against the null hypothesis


    # Unblock the line below to see the effect of neglecting age based variation
    # Chol3a1 <- lm(Cholesterol ~ Drug, data=Chol1a)
    # anova(Chol3a1)
    # => Blocking gives us more power to test for treatment effect


# 3.1c - Obtain model coefficients from the summary table
summary(Chol3a)
    




# SECTION 4 #
#------------------------------------------------------------------------------#
# Conducting Mixed Effects RBD ANOVA on the data

# 4.1a - Fitting a linear model to the data. Make sure you have installed the
# nlme package before running. Note the 'random=~1|Age' option specifies 
# that Age should be treated as a random effect rather than a fixed effect.
Chol1a
library(nlme)
Chol4a <- lme(Cholesterol ~ Drug, random=~1|Age, data=Chol1a)


# 4.1b - Obtain the ANOVA table
anova(Chol4a)

    # Analysis of Variance Table
    #               numDF denDF   F-value     p-value
    # (Intercept)   1     994     2158.5071   <.0001
    # Drug          1     994     44.7028     <.0001

    # Same F-statistic as above.
    # Strong evidence against the null hypothesis



# 4.1c - Obtain model coefficients from the summary table
summary(Chol4a)


    ## Random effects:
    ##            (Intercept) Residual
    ## StdDev:    7.774422    13.80597
    ##
    ## % Variance Due to Blocks       = 7.774422^2 / (7.774422^2 + 13.80597^2) = 24.1%
    ## % Variance Due to Residual Var = 13.80597^2 / (7.774422^2 + 13.80597^2) = 75.9%



# SECTION 5 #
#------------------------------------------------------------------------------#
# Conducting a Permutation Test using the lmPerm package

# 5.1a - Conducting the permutation test. Make sure that you have installed the
# lmPerm package before running
Chol1a
library(lmPerm)
set.seed(3141579)
Chol5a <- aovp(Cholesterol ~ Drug + Error(Age/Drug), data=Chol1a)

# 5.1b - obtaining the results of the permutation test
summary(Chol5a)

    # Results of permutation test
    #             Df  Sum Sq    Mean Sq   Pr(Exact)   
    # Drug1       1   8520.6    8520.6    0.008333 **
    # Residuals   4   1338.5    334.6    

    # Strong evidence against the null hypothesis.

