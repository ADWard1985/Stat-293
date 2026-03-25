# CHAPTER 2 - One Way ANOVA (Theory & Model Fitting in R)

# PROGRAM   2.5a - Student IQ Tests (ANOVA Example)

# SUMMARY   A group of researchers are interested in whether the mean IQs of 
##          students studying different degrees differ. They take a sample of 1,000
##          Art, Biology, Chemistry, Philosophy and Statistics students and make
##          them sit an IQ test. The data pertaining to these tests is contained 
##          in the csv file entitled 'IQ Tests (All Subjects)'.
##
##          In the first section of this program we produce basic numeric and
##          graphical summaries of the data.
##
##          In the second section of the program we conduct an ANOVA test to
##          test between the hypotheses:
##
##              H0: There is no difference in the mean IQ of students studying
##                  different degrees. 
##
##              H1: There is a difference in the mean IQ of students studying
##                  different degrees.
##
##          In the third section of this program we conduct post-hoc Tukey
##          Honestly Significant Difference tests to determine which groups
##          of students have significantly different mean IQs.




# Clear all variables before starting
rm(list = ls())




# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data and producing numeric and graphical summaries

# 1.1a - Read in the 'IQ Tests (All Subjects).csv' data set 
IQ1a <- read.csv("G:\\Academic\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 2 - ANOVA\\IQ Tests (All Subjects).csv")
IQ1a



# 1.1b - Refining to required variables, i.e. dropping the 'Student' column
IQ1b <- subset(IQ1a, select = c(Art, Biology, Chemistry, Philosophy, Statistics))
IQ1b

  # Creating a box plots of the data in each group
  boxplot(IQ1b, col = rainbow(ncol(IQ1b)) ,  ylab='IQ Score')



# 1.1c - Obtain Summary statistics of the data students in each group
IQ1c <- summary(IQ1b)
IQ1c

# 1.1d - Obtain the standard deviation of the IQs of students in each group
IQ1d <- sapply(IQ1b, sd)
IQ1d








   

# SECTION 2 #
#------------------------------------------------------------------------------#
# Model Fitting

# 2.1a - Stacking the variables on top of each other
IQ2a <- stack(IQ1b)
IQ2a


# 2.1b - Re-ordering the columns and renaming them
IQ2b <- IQ2a[, c("ind", "values")]          # Reorder columns
names(IQ2b)[names(IQ2b)=='ind'] <- 'Group'  # Rename ind = Group
names(IQ2b)[names(IQ2b)=='values'] <- 'IQ'  # Rename values = IQ
IQ2b


# 2.1c - Creating the ANOVA model
IQ2c <- lm(IQ ~ Group, data=IQ2b)
summary(IQ2c)

    ## F-statistic: 22.79 on 4 and 4995 DF,  p-value: < 2.2e-16


# 2.1d - Obtaining the summary ANOVA table
IQ2d <- anova(IQ2c)
IQ2d

    ##            Df    Sum Sq    Mean Sq   F value     Pr(>F)    
    ## Group      4     9056      2263.89   22.794      < 2.2e-16 ***
    ## Residuals  4995  496098    99.32  

    ## ANOVA indicates that there is strong evidence against the null hypothesis
    ## that the means of all groups are equal. We therefore proceed to post-hoc
    ## tests to determine which groups have different means.
    



# SECTION 3 #
#------------------------------------------------------------------------------#
# The p-value of the F-statistic calculated in the previous section is < 0.001.
# This provides very strong evidence against the null hypothesis that the IQs
# of all students are equal. In this section we conduct post-hoc tests to
# determine which of the groups of have different means.

# 3.1a - Conducting post-hoc Tukey HSD tests
IQ3a <- aov(IQ ~ Group, data=IQ2b)
TukeyHSD(IQ3a)







