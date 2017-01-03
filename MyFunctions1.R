##  Functions to Compute Entropy and Information Gain

## define probabilities function. This will generate the probabilities 
## of the unique items (levels or categories) for a given vector
## You need to run the function once
myFuncProb <- function(x){ table(x)/length(x) }

## Test the function. Find the probabilities for x1, x2, x3

## NOTE: x2 is a numberic feature but the function will treat the 
## numbers as unique items and will find the probability of each number (item)


## Define log (base 2). This function checks for zero to avoid returning -Inf
myLog <- function(x) { x[which(x==0)] = 1.0e-20; log(x , b = 2)}

## Entropy function which uses myLog
myFuncEntropy <- function(x){ -sum( myFuncProb(x) * myLog( myFuncProb(x) ))}


## Function to compute H(y|x) for any categorical feature x 
Hy_given_x <- function(dataSet,y,x)
{
  sumValues = 0.0;
  prob = myFuncProb(dataSet[,x])
  
  for(i in 1:dim(prob)) 
  {
    Subset = dataSet[dataSet[,x] == names(table(dataSet[,x]))[i],] #filtered to only the given x value
    sumValues = sumValues + (prob[i]*myFuncEntropy(Subset$y))
  }
 
  return(sumValues);
}

## Function to compute Information Gain = H(y) - H(y|x) 
myInformationGain <- function(dataSet,y,x)
{
  myFuncEntropy(dataSet$y) - Hy_given_x(dataSet,y,x);
}


myFindNumericThreshold <- function(dataSet,y,x)
{
  numericSet = as.numeric(names(table(dataSet[,x])))

  index = 1
  maxGain = 0
  threshold = 0
  for(i in 1:(length(numericSet)-1)) 
  {
    rows = which(dataSet[,x] <= numericSet[i]) 
    partition1 = dataSet[rows,]
    partition2 = dataSet[-rows,]
    
    L1 = dim(partition1)[1];
    L2 = dim(partition2)[1];
    h1 = myFuncEntropy(partition1$y); 
    h2 = myFuncEntropy(partition2$y);
    
    h_given_threshold = L1/(L1+L2) *h1 + L2/(L1+L2)*h2;
    gain = (myFuncEntropy(dataSet$y) - h_given_threshold);
    
    if(gain > maxGain) {index = i; maxGain = gain; threshold = numericSet[i]; }
  }
 
  ThresholdAndGain = c(threshold,gain);
}





