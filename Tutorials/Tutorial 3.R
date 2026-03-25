# Tutorial 3



# Question 1
#------------------------------------------------------------------------------
rm(list = ls())


# 1.1a - Read in the data 
smo1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\Tutorials\\Tutorial Data\\smoking.csv")
str(smo1a)



# 1.1b - Constructing the ANOVA Model
smo1b <- aov(fev1 ~ as.factor(group), data=smo1a)
summary(smo1b)



# 1.1c - Extracting the observed test_statistic F_obs
smo1c <- summary(smo1b)[[1]]["as.factor(group)","F value"]
smo1c
    # F-statistic = 3.623135




# 1.1d - Construct a distribution of similar test statistics from a cohort of 
# 1,000 re-ordered samples.

  # Define number of re-ordered samples to take
  L <- 1000
  
  # Create a blank vector called smo1d of length L
  smo1d <- rep(NA,L)

  # Set seed to ensure we obtain the same re-ordered samples each time code is run
  set.seed(12345)

  # Do for i = 1,...,L
  for (i in 1:L) {
    
    # Obtain a re-ordered sample from the smo1a data set
    group.perm <- sample(smo1a$group)
        
    # Conduct ANOVA on the re-ordered sample
    this.aov <- aov(fev1 ~ as.factor(group.perm), data=smo1a)
    
    # Extract the calculated F-Statistic 
    summary(this.aov)
    F.perm <- summary(this.aov)[[1]]["as.factor(group.perm)","F value"]
        
    # Add calculated F-statistic to the ith element of the vector smo1d
    smo1d[i] <- F.perm
  }


# 1.1e - Construct histogram showing the generated distribution of test statistics 
hist(smo1d) 


# 1.1f - Calculate the p-value for the test.
length(smo1d[smo1d>=smo1c])/L

  # p-value = 0.039
  





# Question 2
#------------------------------------------------------------------------------
rm(list = ls())

# 2.1a - Read in the data
hon1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\Tutorials\\Tutorial Data\\crdhoneycough.csv")
str(hon1a)


# 2.1b - Constructing the ANOVA Model
hon1b <- aov(score ~ treatment, data=hon1a)
summary(hon1b)



# 2.1c - Extracting the observed test_statistic F_obs
hon1c <- summary(hon1b)[[1]]["treatment","F value"]
hon1c
# Observed F-statistic = 17.50957




# 2.1d - Construct a distribution of similar test statistics from a cohort of 
# 5,000 re-ordered samples.

# Define number of re-ordered samples to take
L <- 5000

# Create a blank vector called hon1d of length L
hon1d <- rep(NA,L)

# Set seed to ensure we obtain the same re-ordered samples each time code is run
set.seed(12345)

# Do for i = 1,...,L
for (i in 1:L) {
  
      # Obtain a re-ordered sample from the hon1a data set
      reordered_sample <- sample(hon1a$treatment)
      
      # Conduct ANOVA on the re-ordered sample
      reordered_perm <- aov(score ~ reordered_sample, data=hon1a)
      
      # Extract the calculated F-Statistic 
      summary(reordered_perm)
      F_extract <- summary(reordered_perm)[[1]]["reordered_sample","F value"]
      
      # Add calculated F-statistic to the ith element of the vector hon1d
      hon1d[i] <- F_extract
    }


# 2.1e - Construct histogram showing the generated distribution of test statistics 
hist(hon1d)


# 2.1f - Calculate the p-value for the test.
length(hon1d[hon1d>=hon1c])/L

# p-value = 0














# Question 3
#------------------------------------------------------------------------------
rm(list = ls())

# 3.1a - Read in the data 
pos1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\Tutorials\\Tutorial Data\\factpoison.csv")
str(pos1a)
pos1a


# 3.1b - Create the linear model
pos1b <- lm(Time ~  as.factor(Poison) + as.factor(Treatment) + 
                    as.factor(Poison) * as.factor(Treatment), data=pos1a)



# 3.1c - Obtaining a plot of residuals vs fitted values and QQ plot of residuals
par(mfrow=c(1,2))

plot(pos1b$res ~ pos1b$fit, xlab="Fitted value", ylab="Residuals")
abline(h=0,col="gray")

qqnorm(pos1b$res ,main="")
qqline(pos1b$res)

    # Residual vs Fitted Value Plot implies non-constant variance
    # qq-plot implies data is not normally distributed



# 3.1d - Conducting Levene's test for homogeneous variance
library(car)
leveneTest(Time ~ as.factor(Poison) * as.factor(Treatment), data = pos1a)

    # Levene's test provides strong evidence against the null hypothesis that
    # the data is each group have equal variance




# 3.1e - Obtain ANOVA table from Normal Theory ANOVA 
anova(pos1b)
    # very strong evidence for effect of Poison on the response variable Time
    # Very strong evidence for effect of Treatment on the response variable Time
    # No evidence for interaction between Poison and Treatment


# 3.1f - Obtaining an ANOVA table from a permutation test
library(lmPerm)
set.seed(12345)
pos1c <- lmp(Time ~ as.factor(Poison) + as.factor(Treatment) + 
                    as.factor(Poison) * as.factor(Treatment), 
                    data=pos1a, perm="Prob")
anova(pos1c)

    # very strong evidence for effect of Poison on the response variable Time
    # Very strong evidence for effect of Treatment on the response variable Time
    # No evidence for interaction between Poison and Treatment



# Both Normal Theory ANOVA and permutation tests give the same conclusions.
# However, since the ANOVA assumptions of normality and homogeneous variance
# are violated, the p-values obtained from Normal Theory ANOVA are not valid.









