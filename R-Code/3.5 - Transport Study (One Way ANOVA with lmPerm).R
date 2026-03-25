# CHAPTER 3 - Permutation Testing

# PROGRAM   3.5 - Transport Study (One Way ANOVA with lmPerm)

# SUMMARY   As part of a transport study, data were collected on travel times 
#           from a certain suburb to the city centre during the morning rush hour. 
#           Three methods were to be compared, bus, private car and bicycle. The
#           data pertaining to this experiment is contained in the csv entitled
#           'Transport Study', but is also shown below:
#
#               Bus   21.00  23.00  27.00  22.00  31.00
#               Car   23.00  28.00  22.00  25.00  24.50
#               Cycle 29.00  27.00  30.00  29.00  28.75
#
#           This program is structured as follows:
#
#           Section 1 - Read in the data
#
#           Section 2 - The sample size within each group is small. As such,
#           we have no way to tell whether or not the ANOVA conditions are
#           satisfied. However, in this section we produce simple visual and 
#           numerical descriptions of the data which SUGGEST (but do not prove)
#           that the ANOVA conditions are violated.
#
#           Section 3 - Basic Data Manipulation
#
#           Section 4 - Conducting ANOVA and Permutation tests using lmPerm



# Clear all variables before starting
rm(list = ls())


# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data

# 1.1a - Read in the csv data set entitled 'Transport Study'
tra1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 3 - Permutation Testing\\Transport Study.csv")
tra1a

# 1.1b - Refining to required variables, i.e. dropping the 'Student' column
tra1b = subset(tra1a, select = c(Bus, Car, Cycle))
tra1b 




# SECTION 2 #
#------------------------------------------------------------------------------#
# The sample size in each group (Bus, Car, Cycle) is very small. As such, we
# have no way to determine whether or not the ANOVA conditions hold. In this
# section we produce simple visual and numerical descriptions of the data which
# SUGGEST that the ANOVA conditions are violated.

# 2.1a - Creating a box plots of the data in each group
boxplot(tra1b , col = rainbow(ncol(tra1b )) ,  ylab='Travel Time')

  # Box plot suggests non-constant variance between groups.



# 2.1b - Obtain Summary statistics of the data in each group
tra2b <- summary(tra1b)
tra2b

# 2.1c - Obtain the standard deviation of the data in each group
tra2c <- sapply(tra1b, sd)
tra2c

  # Sample standard deviations non-constant across groups





# SECTION 3 #
#------------------------------------------------------------------------------#
# Basic data manipulation steps

# 3.1a - Stacking the data
tra3a <- stack(tra1b)
tra3a


# 3.1b - Re-ordering the columns, renaming them and omitting missing values
tra3b <- tra3a[, c("ind", "values")]            # Reorder columns
names(tra3b)[names(tra3b)=='ind'] <- 'Group'    # Rename ind = Group
names(tra3b)[names(tra3b)=='values'] <- 'Time'  # Rename values = Time
tra3b <- na.omit(tra3b) 
tra3b





# SECTION 4 #
#------------------------------------------------------------------------------#
# Conducting ANOVA and Permutation tests on the data using the lmPerm package

# 4.1a - Calling the lmPerm package and setting the seed
library(lmPerm)
set.seed(12345)


# 4.1b - Running the lmp() command with contrast setting = Sum
tra4b <- lmp(Time ~ Group, data=tra3b, perm="Prob")


  # 4.1b1 - Running the lmp() command with contrast setting = treatment
  tra4b1 <- lmp(Time ~ Group, data=tra3b, perm="Prob",
               center=FALSE, contrasts=list(Group="contr.treatment"))



# 4.1c - Obtaining the coefficients of the model with contrast setting = Sum
coef(tra4b)

  # 4.1c1 - Obtaining the coefficients of the model contrast setting = treatment
  coef(tra4b1)



  
# 4.1d - Obtaining the ANOVA F-statistic (i.e. F_obs), dfs and the associated p-value
summary(tra4b)

  # F-statistic: 3.57 on 2 and 12 DF,  p-value: 0.06073
  # If the ANOVA conditions are satisfied (we do not know whether or not they are)
  # this would suggest that there is weak evidence against the null hypothesis




# 4.1e - Obtaining the results of the permutation test
anova(tra4b)

  #           Df   Sum Sq   Mean Sq   Iter  Pr(Prob)
  # Group      2   56.258   28.1292   1644  0.07968 .
  # Residuals 12   94.550   7.8792
  

  # NORMAL THEORY ANOVA RESULTS:
  # -----------------------------
  # Note F = MST / MSE = 28.1292 / 7.8792 = 3.57 as above. This is the observed
  # F-statistic of the data.


  # PERMUTATION TEST RESULTS:
  # -----------------------------
  # The p-value associated to the permutation test is 0.07968.
  # The process was terminated after creating 1644 re-ordered samples
  # Since the permutation test makes no assumptions about the nature of the data
  # we conclude that there is weak evidence against the null hypothesis











