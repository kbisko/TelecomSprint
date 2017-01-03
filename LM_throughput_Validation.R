
### To compare the performance of LM to that of Decision Tree we need to cross validate and generate mean error for LM
### This R script is to demonstrate how to cross validate the LM algorithm (and how to generate emean error) 

setwd("C:/GSU/Oct-19")
d1 <- read.table("Throughput.csv", header = TRUE, sep = ",")


### Let's delete the first 8 columns. This is just for demonstration. I am deleting all categorical features. 
### We now know that LM is sensitive to categorical features with LARGE number of levels but it should be fine with categorical features with small number of levels
del1 = c(-1,-2,-3,-4,-5,-6,-7,-8)
d2 = d1[,del1] ## d2 is the resulting data set after deleting the first 8 columns 



#############################################################
#########  Cross Validation
#########  Now let's build a tree using a portion of the data set and use the other part to validate our model 
#########  We start with splitting the data into training data set and validation data set

set.seed(33) ## This is needed to be able to reproduce the same numbers
train_rows = sample(1:nrow(d2), nrow(d2)/2) 
# train_rows now contains the row numbers that we just selected randomly

train_dataSet    = d2[ train_rows,]  ## thus will copy from d2 all the rows we selected for training
validate_dataSet = d2[-train_rows,]  ## this will delete from d2 all the rows we selected for training

###############################################################
##### 1) TRAIN:
#####   Run LM  on the training data set
d3 <- train_dataSet[,-10]
fitTrain <- lm(train_dataSet[,10] ~ . , data=d3)
summary(fitTrain)

####  2) VALIDATE
##### Now use the input part of the validation data to predict the output 
## validate_dataSet[,10] is column 10 of validate_dataSet which is the output that we want to "hide" from LM
## validate_dataSet[,-10] is the input part (9 inputs) of the training data. We need to use that with "fitTrain"
## to predict the output
predicted_values = predict(fitTrain, validate_dataSet[,-10])

## With LM we are using numeric values for the output
## In order to compute the mean error for LM to be able to compare its  performace to the 
## performace of the Decision Tree algorithm, We need to convert both the predicted output 
## and the validation output into categories. 

library(arules)

## Categorize "predicted_values"
temp = discretize(predicted_values, method = "cluster", categories= 3)
predicted_values_class =  factor(temp,labels= c("B", "G", "E"))

## Categorize the validation output validate_dataSet[,10]
temp2 = discretize(validate_dataSet[,10], method = "cluster", categories= 3)
validation_values_class =  factor(temp2,labels= c("B", "G", "E"))

### Now compare the predicated values to the actual values and find the mean error
mean_error = mean(predicted_values_class != validation_values_class)
mean_error_percentage = 100*mean_error;
cat("Mean Error = ",mean_error_percentage,"%\n")




