########## 
# Let's try to use Decison Trees on the file "Throughput.csv" that you should have
#library(C50)
setwd("C:/GSU/Oct-12");
d1 <- read.table("Throughput.csv", header = TRUE, sep = ",")
head(d1)

## d1 contains 18 colums, and the desired output (throughput) is column # 18. 
## In this file, the throughput, is a numeric value

## the following will copy column 18 into the vector output_numeric
output_numeric = d1[,18]

# It's good to verify what we just did by printing, say, the first 10 numbers
print(output_numeric[1:10])

## Now we want to convert output_numeric into a Category using three levels: Bad, Good, Excellent
library(arules)
temp = discretize(output_numeric, method = "cluster", categories= 3)

output_class =  factor(temp,labels= c("B", "G", "E"))
print(output_class[1:10])

## And we want to replace column 18 (the numeric values) with the categorical output
d1 = data.frame(d1[,-18], output_class) 

##############################################################
##############################################################
## The following will build a tree model using all 17 features as input.
## This will take long to compute
m1 <- C5.0(d1[,-18], d1[,18])
summary(m1)
plot(m1)

## Let's simplify things a little. Let's delete the first 4 columns and name the new frame as d2
del1 = c(-1,-2,-3,-4)
d2 = d1[,del1] 

### Now build a tree using d2. Use the first 13 column as input and column 14 as output
m2 <- C5.0(d2[,-14], d2[,14])
summary(m2)
plot(m2)

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
#####  Now build the Tree Classifier using the Training Data Set "train_dataSet"
m3 <- C5.0(train_dataSet[,-14], train_dataSet[,14])
summary(m3)
plot(m3)

###############################################################
##### 2) VALIDATE:
##### Now use the input part of the validation data to predict the output 
##### Then compare the predicted values to the actual values of the output column
## validate_dataSet[,14] is column 14 of validate_dataSet which is the output that we want to "hide" from the algorithm
## validate_dataSet[,-14] is the input part (13 inputs) of the training data. We need to use that along with the model "m3"
## to predict the output

predicted_values = predict(m3, validate_dataSet[,-14], type = "class")

### Now compare the predicated values to the actual values and find the mean error
mean_error = mean(predicted_values != validate_dataSet[,14]);
mean_error_percentage = 100*mean_error;

cat("Mean Error = ",mean_error_percentage,"%\n")

