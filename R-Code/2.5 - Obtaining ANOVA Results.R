# CHAPTER 2 - One Way ANOVA (Theory & Model Fitting in R)

# PROGRAM   2.5 - Obtaining ANOVA Results

# SUMMARY   In this program we demonstrate two different ways to obtain the  
#           results of one-way ANOVA using a dummy data set.
#               METHOD 1 - Using the lm() command
#               METHOD 2 - Using the aov() command



# Clear all variables before starting
rm(list = ls())



# SECTION 0 #
#------------------------------------------------------------------------------#
# Reading in the data

# 1.1a - Read in the 'Dummy Data.csv' data set and visualising. This is a data
# set consisting of 100 observations of three groups A, B and C. The data for
# each group is taken from independent, normally distributed populations with
# equal variance.
Dum0a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 2 - ANOVA\\Dummy Data.csv")
Dum0a





# SECTION 1 #
#------------------------------------------------------------------------------#
# Obtaining ANOVA results using the lm() command

# 1.1a - Fit a linear model specifying value (y) as a function of group
Dum1a <- lm(value ~ group, data=Dum0a)

# 1.1b - The summary command will produce a table of model coefficients as well
# as the degrees of freedom, F-statistic and p-value of the ANOVA 
summary(Dum1a)

# 1.1c - The anova command will produce the ANOVA table which shows ALL of the
# ANOVA metrics, i.e. DFs, SST, SSE, MST, MSE, F-statistic, P-value
anova(Dum1a)






# SECTION 2 #
#------------------------------------------------------------------------------#
# Obtaining ANOVA results using the aov() command
  
# 2.1a - Creating the ANOVA model
Dum3a <- aov(value ~ group, data=Dum0a)

# 2.1b - Obtaining the table of coefficients
coef(summary.lm(Dum3a))

# 2.1c - In this case the summary() command and the anova() command will
# produce the same output, i.e. the ANOVA table
summary(Dum3a)
anova(Dum3a)

# 2.1c - The advantage of using the aov() command is that Tukey's HSD test can
# be applied to aov() objects but not to anova() objects.
TukeyHSD(Dum3a)









