#entropy

entropy <- function(x){
  -sum(table(x)/length(x) * log(table(x)/length(x), b=2))
}



