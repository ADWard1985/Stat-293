# Tutorial 1



# Question 3
#------------------------------------------------------------------------------
# Using R as a calculator #

# Clear all variables
rm(list = ls())

library(MASS) ##load the MASS package to your current session
data(Animals) ##load the Animals data set into your current session
str(Animals)  ##View the number and types of variables contained in the data set.
Animals       ##View the data set in the console window

# Create vector showing weight of animal brains
y <- Animals$brain
y

# 3a - Sum first 10 elements of the vector
sum(y[1:10])

# 3b - Sum the square of the first 10 elements
sum((y[1:10])^2)

# 3c - Sum of squared distance from the mean
sum((y-mean(y))^2)

# 3d - Alternative way of calculating the sum of squared distances from the mean
sum(y^2)-28*(mean(y))^2

# 3e - The total brain weight for the three dinosaur species: Dipliodocus, Triceratops & Brachiosaurus
sum(y[c(6,16,26)])

# 3f - The mean (brain weight)/(body weight) ratio over all species.
mean(Animals$brain / Animals$body)






# Question 4
#------------------------------------------------------------------------------
rm(list = ls())

# Read in the world1970.csv data set
world <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\Tutorials\\Tutorial Data\\world1970.csv")
str(world)
world


# 4ai - Create scatter plot of infant mortality vs per capita income
plot(infant ~ income, data=world, xlab="Per-capita income in US dollars",
     ylab="Infant mortality rate per 1000 live births",  pch=19)

  # In general infant mortality decreases as per-capita income increases.


# 4aii - Create box plot of infant mortality by region
boxplot(infant ~ region, data=world, xlab="World Region",
        ylab="Infant mortality rate per 1000 live births")

  # Median infant mortality rate is highest among countries in Africa and lowest in countries in Europe.





# 4bi - Create scatter plot of infant mortality vs per capita income using ggplot2
library(ggplot2)
ggplot(world, aes(x=income, y=infant, color=region))+
  geom_point()+
  labs(x="Per-capita income in US dollars", y="Infant mortality rate per 1000 live births")


# 4bii - Create box plot of infant mortality by region using ggplot2
ggplot(world, aes(x=region, y=infant))+
  geom_boxplot()+
  labs(x="World Region", y="Infant mortality rate per 1000 live births")








# Question 5
#------------------------------------------------------------------------------
rm(list = ls())

# Read in the world1970.csv data set oncemore
world <- read.csv("F:\\Lecture Courses\\STAT 293\\Additional Material\\Tutorials\\Tutorial Data\\world1970.csv")
world


# 5a - Calculate mean income for all countries
inc.mean<-mean(world$income)
round(inc.mean,2)


# 5b - Calculate mean income for oil exporting and non-oil exporting countries
inc.oil<-tapply(world$income, world$oil,mean)
round(inc.oil,2)


# 5c - Calculate the number and proportion of countries in each region
regions <- table(world$region) # 1-way frequency table
regions                        # no. of countries in each region
prop.table(regions)            # Proportions


# 5d - Calculate the number and proportion of countries in each region that export oil
# 2-way frequency table
regions.oil <- table(world$region, world$oil)
#no. of countries in each region that do or don't export oil
regions.oil     
# Proportions
prop.table(regions.oil,1)

