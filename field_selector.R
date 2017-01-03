rm(list=ls())
setwd("C:/Users/CB/Desktop/School/Sprint")

install.packages("FSelector")
library(FSelector)

thrput <- read.table (file = "Throughput.csv", header = TRUE, sep = ",")
retain <- read.table (file = "Retainability.csv", header = TRUE, sep = ",")
access <- read.table (file = "Accessibility.csv", header = TRUE, sep = ",")

head(thrput)
wthrput <- information.gain(DLThr~.,thrput)
w2thrput <-symmetrical.uncertainty(DLThr~.,thrput)
wthrput
w2thrput

head(retain)
wretain <- information.gain(Retainability~.,retain)
w2retain <-symmetrical.uncertainty(Retainability~.,retain)
wretain
w2retain

?information.gain
head(access)
waccess <- information.gain(Accessibility~.,access)
w2access <-symmetrical.uncertainty(Accessibility~.,access)
waccess
w2access
