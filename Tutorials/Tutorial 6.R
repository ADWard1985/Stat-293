# Tutorial 6



# Question 3
#------------------------------------------------------------------------------
rm(list = ls())


# 3c.1 - Read in the data
sch1a <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\Tutorials\\Tutorial Data\\school.csv")
str(sch1a)
sch1a 


# 3c.2 - Fitting the model
sch1b <- aov(Score ~ School + Error(School:Teacher) , data=sch1a)


# 3c.3 - Obtaining Summary Statistics
summary(sch1b)