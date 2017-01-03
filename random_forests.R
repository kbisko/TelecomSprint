rm(list=ls())
setwd("C:/Users/CB/Desktop/School/Sprint")
#install.packages("coefplot")
#install.packages("arules")
#install.packages("C50")
#install.packages("caret")
#install.packages("randomForest")
#install.packages("ROSE")
#install.packages("DMwR")

thrput <- read.table (file = "Throughput.csv", header = TRUE, sep = ",")
retain <- read.table (file = "Retainability.csv", header = TRUE, sep = ",")
access <- read.table (file = "Accessibility.csv", header = TRUE, sep = ",")

## the following will copy column 18 into the vector output_numeric
output_numeric = thrput[,18]
output_numeric = retain[,18]
output_numeric = access[,18]

# It's good to verify what we just did by printing, say, the first 10 numbers
print(output_numeric[1:10])
table(output_numeric)
## Now we want to convert output_numeric into a Category using three levels: Bad, Good, Excellent
library(arules)

temp = discretize(output_numeric, method = "cluster", categories= 2)
output_class =  factor(temp,labels= c("B", "G"))

output_class = cut(output_numeric,c(0,99.99,100), labels=c("G","B"))

print(output_class[1:10])

perc<-table(output_class)/length(output_class)
## And we want to replace column 18 (the numeric values) with the categorical output
thrput = data.frame(thrput[,-18], output_class) 
retain = data.frame(retain[,-18], output_class) 
access = data.frame(access[,-18], output_class) 

## Let's simplify things a little. Let's delete the first 4 columns and name the new frame as d2
head(thrput)
head(retain)
head(access)

del1 = c(-1,-5)
del1 = c(-1,-5,-11,-16,-17)
del1 = c(-1,-5,-13,-14,-17)

d2 = thrput[,del1] 
d2 = retain[,del1] 
d2 = access[,del1] 

head(d2)
table(d2)
#############################################################
#########  Cross Validation
#########  Now let's build a tree using a portion of the data set and use the other part to validate our model 
#########  We start with splitting the data into training data set and validation data set
set.seed(33) ## This is needed to be able to reproduce the same numbers
train_rows = sample(1:nrow(d2), nrow(d2)/1.5) 
# train_rows now contains the row numbers that we just selected randomly

train_dataSet    = d2[ train_rows,]  ## thus will copy from d2 all the rows we selected for training
validate_dataSet = d2[-train_rows,]  ## this will delete from d2 all the rows we selected for training

#ROSE method of generating data synthetically
library(ROSE)
data_rose<-ROSE(output_class~.,data=train_dataSet,seed=1)$data
#SMOTE method of generating data synthetically
library(DMwR)
data_smote<-SMOTE(output_class~.,data=train_dataSet,perc.over=3000,perc.under=3000,k=5)

###### (B) RANDOM FOREST
library(randomForest)
##### 1) TRAIN:
m1 <- randomForest(output_class ~ .,train_dataSet, ntree = 200)
m2 <- randomForest(output_class ~ .,train_dataSet, ntree = 200)
m3 <- randomForest(output_class ~ .,train_dataSet, ntree = 200)

importance(m1)

importance(m2)
importance(m3)

getTree(m1,1,labelVar = TRUE)

library(caret)
##### 2) VALIDATE:
predicted_values21 = predict(m1, validate_dataSet[,-16], type = "class")
predicted_values22 = predict(m2, validate_dataSet[,-13], type = "class")
predicted_values23 = predict(m3, validate_dataSet[,-13], type = "class")

confusionMatrix(predicted_values21,validate_dataSet[,4])
mean_error21 = mean(predicted_values21 != validate_dataSet[,16]);
mean_error_percentage21 = 100*mean_error21;
cat("Mean Error = ",mean_error_percentage21,"%\n")
1.22912
accuracy.meas(validate_dataSet[,4], predicted_values21)
roc.curve(validate_dataSet[,4],predicted_values21, plotit=F)
0.500
#confusionMatrix(predicted_values2,validate_dataSet[,7])
mean_error22 = mean(predicted_values22 != validate_dataSet[,13]);
mean_error_percentage22 = 100*mean_error22;
cat("Mean Error = ",mean_error_percentage22,"%\n")
accuracy.meas(validate_dataSet[,13], predicted_values2)
roc.curve(validate_dataSet[,13],predicted_values22, plotit=F)

#confusionMatrix(predicted_values2,validate_dataSet[,7])
mean_error2 = mean(predicted_values2 != validate_dataSet[,5]);
mean_error_percentage2 = 100*mean_error2;
cat("Mean Error = ",mean_error_percentage2,"%\n")

accuracy.meas(validate_dataSet[,5], predicted_values2)
roc.curve(validate_dataSet[,5],predicted_values2, plotit=F)


##### throughput -- using 2 output levels, 200 trees, base is 26.2%
Mean Error =  25.39936 % With all but level_2_name & model
Mean Error =  25.40224 % when remove all but the below
ue_type                        121.0181
traffic_category               132.8654
functionality                  318.9146
os                            1029.4822 
hour                          2917.0457
vendor                        3023.3453

##### retainability -- using 2 output levels, 200 trees, base is 17%
Mean Error =  21.26701 % With all but level_2_name & model
Mean Error =  17.8201 % 
ue_type                        139.9939
os                             472.0761
traffic_category               738.8212
functionality                 2255.2769
vendor                        2821.8626
hour                          3508.1399

##### accessibility -- using 2 output levels, 200 trees, base is 1.2%  
Mean Error =  1.509372 % With all but level_2_name & model
Mean Error =  1.229122 %
traffic_category               2.785113
ue_type                        3.123051
os                            11.573420
functionality                 11.783768
vendor                        81.849458
hour                         120.502281

## accessibility is very skewed. lets try over/undersampling to get a better result
231222*.3
231222*.01
