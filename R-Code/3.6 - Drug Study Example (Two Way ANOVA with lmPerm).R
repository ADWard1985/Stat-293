# CHAPTER 3 - PERMUTATION TESTING

# PROGRAM     3.6 - Drug Study Example (Two Way ANOVA with lmPerm)

# SUMMARY     Researchers are interested in determining whether:
#                 i.  Genotype (AA, Aa, aa)
#                 ii. Two different prescription drugs (A and B)
#             have an effect on the level of chemical X within the blood streams
#             of patients. An unbalanced sample of data has been taken showing
#             the concentration of chemical X within the blood two hours after 
#             taking a standard dose of Drug A or Drug B for people of each 
#             genotype. This data is available in the csv entitled 'Drug Study'
#
#             Given the small sample size (particularly in certain cells) there 
#             is no way to tell if the Two-Way ANOVA conditions are satisfied 
#             (i.e. the data in each cell arises from independent, normally
#             distributed populations with homogenous variance).
#             
#             Consequently, in this program we perform a permutation test to
#             determine whether:
#               1.  There is any interaction effect between genotype and drug on
#                   the level of chemical X in a patients blood stream
#               2.  Genotype has any effect on the level of chemical X in a 
#                   patients blood stream
#               3.  Drug has any effect on the level of chemical X in a 
#                   patients blood stream


# Clear all variables before starting
rm(list = ls())


# SECTION 1 #
#--------------------------------------------------------------------------------------------#
# Reading in the data 

# 1.1a - Read in the csv data set entitled 'Drug Study'
Chem1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 3 - Permutation Testing\\Drug Study.csv")
Chem1a





# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting two-way ANOVA on the data using the lm() command

# 2.1a - Constructing the linear model with genotype entered first
Chem2a <- lm(chem ~  genotype + drug + genotype * drug, data=Chem1a)
anova(Chem2a)

    ##                Df  Sum Sq    Mean Sq   F value    Pr(>F)    
    ## genotype       2   145.430   72.715    11.6209    0.0005747 ***
    ## drug           1   0.033     0.033     0.0053     0.9429299    
    ## genotype:drug  2   4.865     2.432     0.3887     0.6834795    
    ## Residuals      18  112.631   6.257  


# 2.1b - Constructing the linear model with genotype entered second
Chem2b <- lm(chem ~  drug + genotype + genotype * drug, data=Chem1a)
anova(Chem2b)

    ##                Df  Sum Sq    Mean Sq   F value    Pr(>F)  
    ## genotype       2   145.088   72.544    11.5935    0.0005816 ***
    ## drug           1   0.375     0.375     0.0599     0.8093753    
    ## drug:genotype  2   4.865     2.432     0.3887     0.6834795    
    ## Residuals      18  112.631   6.257  

# We note that the sum of squares (and hence the F-statistics) constructed in
# the tests above differ. This is because When the linear model is fitted 
# using lm(), sequential SS (SAS Type I) are calculated by default. Here each 
# effect is adjusted for those that appear earlier in the model. In this case 
# the order in which the factors are specified in the model matters.





# SECTION 3 #
#------------------------------------------------------------------------------#
# Conducting two-way ANOVA and Permutation tests on the data using the 
# lmPerm package

# 3.1a - Calling the lmPerm package and setting the seed
library(lmPerm)
set.seed(123456789)


# 3.1b - Running the lmp() command with genotype first and obtaining results
Chem3b <- lmp(chem ~  genotype + drug + genotype * drug, Chem1a, perm="Prob")
anova(Chem3b)

    ## Analysis of Variance Table
    ##                  Df  Sum Sq    Mean Sq   Iter    Pr(Prob)    
    ## genotype1        2   145.598   72.799    5000    <2e-16
    ## drug1            1   2.675     2.675     170     0.3706    
    ## genotype1:Drug1  2   4.865     2.432     205     0.5659    
    ## Residuals        18  112.631   6.257   


    ## NORMAL THEORY ANOVA RESULTS:
    ## -----------------------------
    ## F_Genotype = MS_Genotype / MSE = 72.799 / 6.257 = 11.635
    ## F_Drug     = MS_Drug     / MSE = 2.675  / 6.257 = 0.4275

    ## PERMUTATION TEST RESULTS:
    ## -----------------------------
    ## No evidence of an interaction between Genotype and Drug
    ## No evidence for Drug having an effect on the level of Chemical X in the 
    ## stream blood of patients
    ## Strong evidence that genotype has an effect on the level of Chemical X 
    ## in the blood stream blood of patients






# 3.1c - Running the lmp() command with genotype second and obtaining results
Chem3c <- lmp(chem ~  drug + genotype + genotype * drug, Chem1a, perm="Prob")
anova(Chem3c)

    ## Analysis of Variance Table
    ##                  Df  Sum Sq    Mean Sq   Iter    Pr(Prob)    
    ## genotype1        2   145.598   72.799    5000    <2e-16 ***
    ## drug1            1   2.675     2.675     69      0.5942    
    ## drug1:genotype1  2   4.865     2.432     134     0.5597    
    ## Residuals        18  112.631   6.257     


    ## NORMAL THEORY ANOVA RESULTS:
    ## -----------------------------
    ## F_Genotype = MS_Genotype / MSE = 72.799 / 6.257 = 11.635 (same as above)
    ## F_Drug     = MS_Drug     / MSE = 2.675  / 6.257 = 0.4275 (same as above)
    
    ## PERMUTATION TEST RESULTS:
    ## -----------------------------
    ## No evidence of an interaction between Genotype and Drug
    ## No evidence for Drug having an effect on the level of Chemical X in the 
    ## stream blood of patients
    ## Strong evidence that genotype has an effect on the level of Chemical X 
    ## in the blood stream blood of patients



# Comparing the output of sections 3.1b and 3.1c we see that the lmp() command
# returns the same Normal Theory ANOVA results. This is because When the linear 
# model is fitted using lmp(), unique SS (SAS Type III) are calculated by default.
# In this case the order in which variables are entered into the model does not
# matter.

# Permutation test results differ slightly due to the fact that different subsets
# of reordered samples are drawn. The same conclusions are drawn however.


