# CHAPTER 2 - One Way ANOVA (Theory & Model Fitting in R)

# PROGRAM   2.1 - Motivating Example (T-test)

# SUMMARY   A group of researchers are interested in whether the IQs of students
##          studying different degrees differ. They take a sample of 1,000
##          Chemistry students and 1,000 Statistics students and make them sit 
##          an IQ test. The data pertaining to these tests is contained in the
##          csv file entitled 'IQ Tests (Chemistry & Statistics)'.
##
##          It is assumed that the IQs of both sets of students are drawn from
##          independent populations which are normally distributed and have
##          equal variance.
##
##          In this program we obtain simple descriptive statistics of the data
##          and conduct a two-sample t-test to test between the following
##          hypotheses:
##
##            \mu_C = Mean IQ of Chemistry Students
##            \mu_S = Mean IQ of Statistics Students
##
##            H0: \mu_C = \mu_S
##            H1: \mu_C ^= \mu_S



# Clear all variables before starting
rm(list = ls())




# SECTION 1 #
#------------------------------------------------------------------------------#
# Reading in the data and obtaining basic summary statistics

# 1.1a - Read in 'IQ Tests (Chemistry & Statistics).csv' 
IQ1a <- read.csv("E:\\Lecture Courses\\STAT 293\\Additional Material\\R Data\\Chapter 2 - ANOVA\\IQ Tests (Chemistry & Statistics).csv")
IQ1a



# 1.1b - Creating a box plot of the IQs of the two groups of students

  # Refining just to the variables 'Chemistry' and 'Statistics' 
  IQ1b = subset(IQ1a, select = c(Chemistry,Statistics) )
  IQ1b

  # Creating a box plot
  boxplot(IQ1b, col = rainbow(ncol(IQ1b)))



# 1.1c - Obtain Summary statistics of Chemistry and Statistics students
IQ1c <- summary(IQ1b)
IQ1c



# 1.1d - Obtain the standard deviation of the IQs of Chemistry and Stats Students
IQ1d <- sapply(IQ1b, sd)
IQ1d





# SECTION 2 #
#------------------------------------------------------------------------------#
# Conducting the t-test to determine if the mean IQs of each group are the same

# 2.1a - Stacking the variables on top of each other, reordering and renaming
IQ2a <- stack(IQ1b)
IQ2a <- IQ2a[, c("ind", "values")]            # Reorder columns
names(IQ2a)[names(IQ2a)=='ind'] <- 'Subject'  # Rename ind = Group
names(IQ2a)[names(IQ2a)=='values'] <- 'IQ'    # Rename values = IQ
IQ2a

# 2.1b  - Conducting the t-test
IQ2b <- t.test(IQ ~ Subject, data=IQ2a, var.equal=T)
IQ2b


    ## t = -4.9013, df = 1998, p-value = 1.029e-06
    ## => Very strong evidence against the null hypothesis that Chemistry and
    ## Statistics students have the same mean IQ



