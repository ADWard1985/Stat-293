# CHAPTER 2 - One Way ANOVA (Theory & Model Fitting in R)

# PROGRAM   2.5b - Smoking Data (ANOVA Example)

# SUMMARY   An investigator wishes to study lung function in relation to smoking
##          behavior. People are classified into four categories (populations): 
##              1.  Nonsmokers 
##              2.  Early smokers - former smokers who stopped smoking more than 
##                  two years ago
##              3.  Recent smokers - former smokers who stopped smoking within 
##                  the past two years
##              4.  Current smokers
##          Samples of size 6 are drawn from each of these four populations and 
##          the lung function of each person is measured. The volume of air that
##          can be forcibly expired in the first second of exhalation (referred 
##          to as FEV1) is the measure of lung function that is determined for
##          each individual.
##
##          In the first section of this program we read in the data and produce 
##          basic numeric and graphical summaries of the data.
##
##          In the second section of the program we conduct an ANOVA test to
##          test between the hypotheses:
##
##              H0: There is no difference in the mean FEV1 lung function of the
##                  various groups
##
##              H1: There is a difference in the mean FEV1 lung function of the
##                  different groups.
##
##          In the third section of this program we conduct post-hoc Tukey
##          Honestly Significant Difference tests to determine which groups
##          have significantly different mean FEV1 lung function.




# Clear all variables before starting
rm(list = ls())




# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data and producing numeric and graphical summaries

# 1.1a - Read in the 'smoking.csv' data set
Smoke1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 2 - ANOVA\\smoking.csv")
Smoke1a



# 1.1b - Refining to required variables, i.e. dropping the 'Individual' column
Smoke1b = subset(Smoke1a, select = c(Nonsmokers, Early.smokers, Recent.smokers, Smokers) )
Smoke1b
  
  # Creating a box plots of the data in each group
  boxplot(Smoke1b, col = rainbow(ncol(Smoke1b)) ,  ylab='FEV1')



# 1.1c - Obtain Summary statistics of the data in each group
Smoke1c <- summary(Smoke1b)
Smoke1c



# 1.1d - Obtain the standard deviation of each group
Smoke1d <- sapply(Smoke1b, sd)
Smoke1d






   

# SECTION 2 #
#------------------------------------------------------------------------------#
# Model Fitting

# 2.1a - Stacking the variables on top of each other
Smoke2a <- stack(Smoke1b)
Smoke2a


# 2.1b - Re-ordering the columns and renaming them
Smoke2b <- Smoke2a[, c("ind", "values")]            # Reorder columns
names(Smoke2b)[names(Smoke2b)=='ind'] <- 'Group'    # Rename ind = Group
names(Smoke2b)[names(Smoke2b)=='values'] <- 'FEV1'  # Rename values = FEV1
Smoke2b


# 2.1c - Creating the ANOVA model
Smoke2c <- lm(FEV1 ~ Group, data=Smoke2b)
summary(Smoke2c)

    ## F-statistic: 3.623 on 3 and 20 DF,  p-value: 0.03085




# 2.1d - Obtaining the summary ANOVA table
Smoke2d <- anova(Smoke2c)
Smoke2d

    ##            Df  Sum Sq    Mean Sq   F value     Pr(>F)    
    ## Group      3   3.6890    1.22967   3.6231      0.03085
    ## Residuals  20  6.7879    0.33939  

    ## ANOVA indicates that there is strong evidence against the null hypothesis
    ## that the means of all groups are equal. We therefore proceed to post-hoc
    ## tests to determine which groups have different mean FEV1 values.
    



# SECTION 3 #
#------------------------------------------------------------------------------#
# The p-value of the F-statistic calculated in the previous section is 0.03085.
# This provides some evidence against the null hypothesis that the mean lung 
# function of the groups are equal. In this section we conduct post-hoc tests to
# determine which of the groups of have different mean FEV1 values.

# 3.1a - Conducting post-hoc Tukey HSD tests
Smoke3a <- aov(FEV1 ~ Group, data=Smoke2b)
TukeyHSD(Smoke3a)







