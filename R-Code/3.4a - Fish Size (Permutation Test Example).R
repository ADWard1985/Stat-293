# CHAPTER 3 - PERMUTATION TESTING

# PROGRAM   3.4a - Fish Size (Permutation Test Example)

# SUMMARY   The length (in cm) of a particular species of fish inside (A) and 
#           outside (B) of a marine reserve are being studied. 10 fish from 
#           either group are caught and their length measured. The data
#           pertaining to this experiment is contained in the csv entitled
#           'Fish Data.csv'.
#
#           This program is structured as follows

#           Section 1:
#           Read in the data and show graphically that the ANOVA conditions are
#           not satisfied such that the p-values produced from ANOVA would not
#           be valid.

#           Section 2:
#           Conducting a permutation test to test between the hypotheses
#               H0: \mu_A = \mu_B
#               H1: \mu_A \neq \mu_B 



# Clear all variables before starting
rm(list = ls())



# SECTION 1 #
#--------------------------------------------------------------------------------------------#
# Reading in the data and showing graphically that the ANOVA conditions are violated

# 1.1a - Read in the data
Fish1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 3 - Permutation Testing\\Fish Data.csv")
Fish1a


# 1.1b - Refining to required variables, i.e. dropping the 'Observation' column
Fish1b = subset(Fish1a, select = c(A, B))
Fish1b



# 1.1c - Creating a box plots of the data in each group
boxplot(Fish1b, col = rainbow(ncol(Fish1b)) ,  ylab='Size of Fish')


    ## Box-plot indicates that groups do not have constant variance. Consequently,
    ## the p-values produced via ANOVA tests will be meaningless







# SECTION 2 #
#--------------------------------------------------------------------------------------------#
# Conducting a permutation test to test between the hypotheses
# H0: \mu_A = \mu_B
# H1: \mu_A \neq \mu_B 

# 2.1a - Stacking the variables on top of each other
Fish2a <- stack(Fish1b)
Fish2a


# 2.1b - Re-ordering the columns and renaming them
Fish2b <- Fish2a[, c("ind", "values")]            # Reorder columns
names(Fish2b)[names(Fish2b)=='ind'] <- 'Group'    # Rename ind = Group
names(Fish2b)[names(Fish2b)=='values'] <- 'Size'  # Rename values = Time
Fish2b


# 2.1c - Creating the ANOVA Model
Fish2c <- aov(Size ~ Group,data=Fish2b)
summary(Fish2c)


# 2.1d - Extracting the observed test statistic F_Obs
Fish2d <- summary(Fish2c)[[1]]["Group","F value"]
Fish2d

  ## F_obs = 5.167845


# 2.1e - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

  # Define number of re-ordered samples to take
  L <- 1000
  
  # Create a blank vector called Fish2e of length L
  Fish2e <- rep(NA,L)
  
  # Set seed to ensure we obtain the same re-ordered samples each time code is run
  set.seed(1234)
  
    # Do for i = 1,...,L
    for (i in 1:L) {
      
      # Obtain a re-ordered sample from the Fish2b data set
      group.perm <- sample(Fish2b$Group)
      
      
      # Conduct ANOVA on the re-ordered sample
      this.aov <- aov(Size ~ group.perm, data=Fish2b)
      
        
      # Extract the calculated F-Statistic and add to the ith element of the
      # vector Fish2e
      Fish2e[i] <- summary(this.aov)[[1]]["group","F value"]
    }
  
#i<-2
#Fish2b
#group.perm
#Fish2e


 



# 2.1f - Construct histogram showing the generated distribution of test 
# statistics and the position of F_obs
hist(Fish2e)
lines(c(Fish2d,Fish2d),c(0,800),col="red",lwd=3)


# 2.1g - Calculate the p-value for the test.
length(Fish2e [Fish2e > Fish2d] ) / L


    ##  Conclusion - p-value = 0.045
    ##  Permutation test provides some evidence against the null hypothesis.




