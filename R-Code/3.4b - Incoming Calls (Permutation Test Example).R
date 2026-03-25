# CHAPTER 3 - Permutation Testing

# PROGRAM   3.4b - Incoming Calls (Permutation Test Example)

# SUMMARY   A power company have 5 regional call centres across NZ 
##              A = Northland
##              B = Auckland 
##              C = Central & Wellington
##              D = Canterbury
##              E = Southern 
##          The time (in minutes) between incoming calls at each of the call 
##          centres is recorded in the csv entitled 'Call Centre Data'. The
##          power company is interested in whether the mean time between calls
##          differs between call centre.
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
##          Section 7 - Given that the ANOVA conditions are violated we conduct
##                      a permutation test to test between the hypotheses:
##
##                      H0: There is no difference in the mean time between
##                          incoming calls at each call centre
##
##                      H1: There is a difference in the mean time between 
##                          incoming calls across call centres




# Clear all variables before starting
rm(list = ls())




# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data 

# 1.1a - Read in the 'Call Centre Data.csv' data set 
Cal1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 3 - Permutation Testing\\Call Centre Data.csv")
Cal1a


# 1.1b - Refining to required variables, i.e. dropping the 'Observation' column
Cal1b = subset(Cal1a, select = c(A, B, C, D, E))
Cal1b





# SECTION 2 #
#------------------------------------------------------------------------------#
# Graphically assessing whether the ANOVA conditions hold

# 2.1a - Creating a box plots of the data in each group
boxplot(Cal1b, col = rainbow(ncol(Cal1b)) ,  ylab='Time Between Calls')

# 2.1b - Creating histograms of the data in each group. Make sure you have 
# installed the Hmisc package
library(Hmisc)
hist.data.frame(Cal1b)


    ## Box-plot and histograms suggest that the ANOVA conditions are violated
    ## violated. Box plot indicates different variances between groups.
    ## Histograms indicate data within each group is not normally distributed
    ## (suggesting of exponential distributions)




# SECTION 3 #
#------------------------------------------------------------------------------#
# Numerically assessing whether the ANOVA conditions hold

# 3.1a - Obtain Summary statistics of the data students in each group
Cal3a <- summary(Cal1b)
Cal3a

# 3.1b - Obtain the standard deviation for the data in each group
Cal3b <- sapply(Cal1b, sd)
Cal3b


    ## Variance in each group does not appear to be constant.
    ## The mean in each group appears to be very similar to the 
    ## standard deviation. This again suggests that the data in each group
    ## comes from exponential distributions with different rates.




# SECTION 4 #
#------------------------------------------------------------------------------#
# Fitting the ANOVA model

# 4.1a - Stacking the variables on top of each other
Cal4a <- stack(Cal1b)
Cal4a


# 4.1b - Re-ordering the columns and renaming them
Cal4b <- Cal4a[, c("ind", "values")]            # Reorder columns
names(Cal4b)[names(Cal4b)=='ind'] <- 'Group'    # Rename ind = Group
names(Cal4b)[names(Cal4b)=='values'] <- 'Time'  # Rename values = Time
Cal4b


# 4.1c - Creating the linear model
Cal4c <- lm(Time ~ Group, data=Cal4b)


  # Command below provides a list of the data sets stored within Cal4c
  names(Cal4c)






# SECTION 5 #
#------------------------------------------------------------------------------#
# Assessing whether the ANOVA conditions hold via residual analysis

# 5.1a - Obtaining a plot of residuals vs fitted values and QQ plot of residuals 

  # Create two plots side by side
  par(mfrow=c(1,2))
  
  # Create Residual vs Fitted Value Plot
  plot(Cal4c$res~Cal4c$fit,xlab="Fitted value",ylab="Residuals")
  abline(h=0,col="gray")
  
  # Create QQ-Plot
  qqnorm(Cal4c$res,main="")
  qqline(Cal4c$res)

  ## Plot of residuals vs fitted values => non-constant variance across groups
  ## QQ plot => residuals are not normally distributed.
   

  
  
  
  
  
# SECTION 6 #
#------------------------------------------------------------------------------#
# Assessing whether the ANOVA conditions hold using statistical tests

# 6.1a - Conducting Levene's test for homogenous variance. Make sure you
# have the "car" package installed.
library(car)
leveneTest(Cal4b$Time, as.factor(Cal4b$Group))

    ##    Df        F value   Pr(>F)
    ##    4,2495    118.09    2.2e-16
    ## => Very strong evidence against the null hypothesis that the variance in 
    ## each group is constant.






# SECTION 7 #
#------------------------------------------------------------------------------#
# In the previous sections we have shown that the ANOVA conditions are violated.
# As such, the p-values obtained from ANOVA and post-hoc tests would be 
# meaningless. Instead, we perform a permutation test to determine whether there
# are differences between the means of each group.


# 7.1a - Constructing the ANOVA Model
Cal7a <- aov(Time ~ Group,data=Cal4b)
summary(Cal7a)


# 7.1b - Extracting the observed test_statistic F_obs
Cal7b <- summary(Cal7a)[[1]]["Group","F value"]
Cal7b

    ## F_obs = 147.6801




# 7.1c - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

  # Define number of re-ordered samples to take
  L <- 1000
  
  # Create a blank vector called Cal7c of length L
  Cal7c <- rep(NA,L)
  
  # Set seed to ensure we obtain the same re-ordered samples each time code is run
  set.seed(12345)
  
  # Do for i = 1,...,L
  for (i in 1:L) {
    
    # Obtain a re-ordered sample from the Cal4b data set
    group.perm <- sample(Cal4b$Group)
    
    # Conduct ANOVA on the re-ordered sample
    this.aov <- aov(Time ~ group.perm, data=Cal4b)
    
    # Extract the calculated F-Statistic and add to the ith element of the
    # vector Cal7c
    Cal7c[i] <- summary(this.aov)[[1]]["group","F value"]
  }

#i<-1
#Cal4b
#group.perm
#Cal7c  
  

# 7.1d - Construct histogram showing the generated distribution of test 
# statistics 
hist(Cal7c)


# 7.1e - Calculate the p-value for the test.
length( Cal7c [Cal7c > Cal7b ] ) / L


    ## p-value = 0
    ## => very strong evidence against the null hypothesis that the means
    ## of all the groups are equal
