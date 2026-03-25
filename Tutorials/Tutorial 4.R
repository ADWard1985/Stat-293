# Tutorial 4



# Question 3
#------------------------------------------------------------------------------
rm(list = ls())


# 3c.1 - Read in the data
jobs1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\Tutorials\\Tutorial Data\\costeng.csv")
str(jobs1a)
jobs1a


# 3c.2 - Fitting a mixed effects model
library(nlme)
jobs1b <- lme(Cost ~ Engineer, random=~1|Job, data=jobs1a)


# 3c.3 - Obtaining the ANOVA table for the model
anova(jobs1b)

# 3c.4 - Obtaining the summary table
summary(jobs1b)
