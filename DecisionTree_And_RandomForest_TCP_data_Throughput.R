########## 
library(C50)
setwd("C:/GSU/Oct-12");
d1 <- read.table("Throughput.csv", header = TRUE, sep = ",")
head(d1)

## Let's delete the first 5 columns (this is for demonstration only) and name the new frame as d2
del1 = c(-1,-2,-3,-4,-5)
d2 = d1[,del1] 

## the following will copy throughut (the last column) into the vector output_numeric
N = dim(d2)[2]
output_numeric = d2[,N]


## Convert output_numeric into a Category using three levels: Bad, Good, Excellent
#library(arules)
#temp = discretize(output_numeric, method = "cluster", categories= 3)
#output_class =  factor(temp,labels= c("B", "G","E"))


threshold = median(d2[,N])
rows = which(d2[,N] < threshold);
output_class = rep("G",length(d2[,N]));
output_class[rows] = "B";


print(output_class[1:10])
d2 = data.frame(d2[,-N], output_class) 



#############################################################
#########  Cross Validation

set.seed(33) ## This is needed to be able to reproduce the same numbers
train_rows = sample(1:nrow(d2), nrow(d2)/4) 
# train_rows now contains the row numbers that we just selected randomly

train_dataSet    = d2[ train_rows,]  ## thus will copy from d2 all the rows we selected for training
validate_dataSet = d2[-train_rows,]  ## this will delete from d2 all the rows we selected for training

###### (A) Start with a Decison Tree Algorithm to have a reference
##### 1) TRAIN:
m3 <- C5.0(train_dataSet[,-N], train_dataSet[,N])
summary(m3)
###############################################################
##### 2) VALIDATE:
predicted_values = predict(m3, validate_dataSet[,-N], type = "class")
mean_error = mean(predicted_values != validate_dataSet[,N]);
mean_error_percentage = 100*mean_error;
cat("Mean Error = ",mean_error_percentage,"%\n")


###### (B) RANDOM FOREST
#install.packages("randomForest")
library(randomForest)
##### 1) TRAIN:
m4 <- randomForest(output_class ~ .,train_dataSet, ntree = 10)

importance(m4)
#getTree(m4,1,labelVar = TRUE)

##### 2) VALIDATE:
predicted_values2 = predict(m4, validate_dataSet[,-N], type = "class")
mean_error2 = mean(predicted_values2 != validate_dataSet[,N]);
mean_error_percentage2 = 100*mean_error2;
cat("Mean Error = ",mean_error_percentage2,"%\n")

## To get a comparable mean error as that of C5.0 algorithm try increasing ntree to, say, 200. 
## It should not take long to compute on d2 data set.


