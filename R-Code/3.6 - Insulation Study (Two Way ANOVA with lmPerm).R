# CHAPTER 3 - Permutation Testing

# PROGRAM   3.6 - Insulation Study (Two Way ANOVA with lmPerm)

# SUMMARY   A company is interested in whether five production lots (labelled 1,
#           2, 3, 4, 5) and two cutting methods (lengthwise or crosswise) have
#           any effect on insulation strength. A balanced sample of data is
#           taken and is available on the webpage:
#           http://www.statsci.org/data/general/insulate.txt
#           but is also available in the csv entitled 'factinsulate'.
#
#           The sample size within each cell (lot-cut combination) is small.
#           As such, there is no way to tell if the Two-Way ANOVA conditions
#           are satisfied (i.e. the data in each cell arises from independent,
#           normally distributed populations with homogenous variance).
#           Consequently, in this program we perform two way ANOVA but support
#           this with a permutation test to determine whether:
#
#             1.  There is any interaction effect between lot and cut on
#                 insulation strength.
#             2.  Lot has any effect on insulation strength
#             3.  Cut has any effect on insulation strength.



# Clear all variables before starting
rm(list = ls())




# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data

# 1.1a - Read in the csv data set entitled 'factinsulate'
ins <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 3 - Permutation Testing\\factinsulate.csv")
ins




# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting ANOVA on the data using the lm() command. Note because this is a
# balanced design the order in which variables are entered into the model
# does not matter.

# 2.1a - Constructing the linear model
# as.factor(lot) esnures Lot is interpreted as a categorical variable
fit0 <- lm(Strength ~ as.factor(Lot) + Cut + as.factor(Lot)*Cut, data=ins)
anova(fit0)

  # Analysis of Variance Table
  #                       Df  Sum Sq    Mean Sq   F value   Pr(>F)    
  #   as.factor(Lot)      4   2.79117   0.69779   70.2689   < 2.2e-16 ***
  #   Cut                 1   0.04537   0.04537   4.5687    0.035272 *  
  #   as.factor(Lot):Cut  4   0.14175   0.03544   3.5685    0.009504 ** 
  #   Residuals           90  0.89373   0.00993 

  # The ANOVA table shows
  #   i.    Strong evidence for the effect of Lot on the response variable
  #   ii.   Some evidence for the effect of Cut on the response variable
  #   iii.  Strong evidence for the presence of interaction between Lot and Cut
  # However, since we do not know if the two-way ANOVA conditions are satisfied
  # this analysis must be supplemented with a permutation test. We will do so
  # using the lmPerm package.


# 2.1b - For completeness, satisfy yourself that when entering variables
# into the model in the opposite order does not affect the output given that
# this is a balanced design.
fit1 <- lm(Strength ~ Cut + as.factor(Lot) + as.factor(Lot)*Cut, data=ins)
anova(fit1)





# SECTION 3 #
#------------------------------------------------------------------------------#
# Conducting Permutation tests on the data using the lmPerm package

# 3.1a - Calling the lmPerm package and setting the seed
library(lmPerm)
set.seed(123456789)


# 3.1b - Running the lmp() command with contrast setting = Sum
fit1<-lmp(Strength ~ as.factor(Lot) + Cut + as.factor(Lot)*Cut,
          data=ins, perm="Prob")


# 3.1c - Obtaining the summary table to identify model coefficients
summary(fit1)

    ## Coefficients:
    ##                         Estimate Iter   Pr(Prob)    
    ##  as.factor(Lot)1        0.0469   5000   <2e-16 ***
    ##  as.factor(Lot)2        0.2254   5000   <2e-16 ***
    ##  as.factor(Lot)3       -0.1271   4989   0.0198 *  
    ##  as.factor(Lot)4        0.1004   5000   <2e-16 ***
    ##  Cut1                  -0.0213   3278   0.0299 *  
    ##  as.factor(Lot)1:Cut1  -0.0667   5000   0.0080 ** 
    ##  as.factor(Lot)2:Cut1   0.0338   827    0.1088    
    ##  as.factor(Lot)3:Cut1  -0.0117   51     0.6863    
    ##  as.factor(Lot)4:Cut1   0.0358   1337   0.0703


# 3.1d - Obtaining the results of the permutation test
anova(fit1)

  ## Analysis of Variance Table
  ##                      Df  Sum Sq    Mean Sq   Iter   Pr(Prob)    
  ##  as.factor(Lot)       4  2.79117   0.69779   5000   <2e-16 ***
  ##  Cut1                 1  0.04537   0.04537   3278   0.0299 *  
  ##  as.factor(Lot):Cut1  4  0.14175   0.03544   5000   0.0064 ** 
  ##  Residuals           90  0.89373   0.00993 


  ## NORMAL THEORY ANOVA RESULTS:
  ## -----------------------------
  ## F_Lot      = MS_Lot     / MSE = 0.69779 / 0.00993 =  70.27 (as above) 
  ## F_Cut      = MS_Cut     / MSE = 0.04537 / 0.00993 =  4.57  (as above)
  ## F_Lot*Cut  = MS_Lot*Cut / MSE = 0.03544 / 0.00993 =  3.57  (as above)


  ## PERMUTATION TEST RESULTS:
  ## -----------------------------
  ## Permutation test provides strong evidence for interaction between Lot and Cut
  ## => Lot and Cut are too strongly intertwined for their effects on the response
  ## variable separately

