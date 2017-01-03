

# A line that starts with # is a comment - it will be ignored 
# Use the console to type command directly
# to run a command or a set of commands from the script file, select (and highlight) the lines and click "run" 



#####
# Top of file set up
# Each file benefits from a setwd(directory) command. 
# This way you are assured of where the data is relative to your path.
# Relying on the script location can be confusing. 

# We assume the file Example1.csv is located in folder C:/R/my_data
setwd("C:/R/my_data")

# Use the command read.table() to read the file. 
# to get more help and documentation on read.table use ? read.table 
# 
? read.table
# You can read more aout read.table at: https://www.datacamp.com/community/tutorials/r-data-import-tutorial#gs.9=TIoTI

# Now read the file and place it in a "data frame". A data frame is used to store tables
d01 <- read.table (file = "Example1.csv", header = TRUE, sep = ",")

# check the data in Global Environment window of RStudio and click on d01 to examine the components of d01
# Now double click d01 in Global Environment to view it in the main window

head(d01)
# what does head(d01) do?


X1 = d01[1];
X2 = d01[2] 
# What is X1 and X2? What does ";" do?

d01$x1

# Packages
# First of all, R derives a lot of its power from its rich set of packages. 
# You can import a package by using the line library(packagename)
# You can see a list of packages at: https://cran.r-project.org/web/packages/available_packages_by_name.html.
# Many of these libraries are centered around a specific model type (such as neural nets and decision trees) 
# but there also packages used for data cleaning and manipulation, but also things like cross-validation.
# Here's an example blog about one of those libraries: http://seananderson.ca/2013/12/01/plyr.html


# The following command should be executed once. After installing the package you can comment it (or delete it)
install.packages("ggplot2")
require(ggplot2)

hist(d01$x1)
hist(d01$x4, main = "Distribution of x4", xlab = "x4")

## Try
ggplot(d01, aes(x=y1, fill= x7)) + geom_histogram(binwidth=2) + xlim(0, 50.0) + labs(x="y1 value") 

## Now Try
ggplot(d01, aes(x=y1, fill= x7)) + geom_histogram(binwidth=2) + xlim(0, 50.0) + labs(x="y1 value")  + facet_wrap(~x7)


# Modelling the data
# Almost every algorithm of machine learning is available as a package in R. 
# The most basic one (and one that does not require a library important) is lm(). 
# The function is specified as (Y_feature ~ X_features). There are a number of ways to specify which 
# features are on either side of that function. Finally, you can view details about your model 
# by running summary(fit) where fit is the assigned output of the lm() function.

fit1 <- lm(y1 ~ x1 + x2 + x3 + x4 + x5, data=d01)
summary(fit1)

install.packages("coefplot")
require(coefplot)
coefplot(fit1)

# Now try this
fit2 <- lm(y1 ~ x1 + x2 + x3 + x7 + x5, data=d01)

# examine summary(fit2) carefully
summary(fit2)
coefplot(fit2)



