rm(list=ls())
setwd("H:/Desktop/School/Sprint")

data<-read.csv('Web_Browsing_S_R_KPIS_500k.csv', header=TRUE)
summary(data)


thrput <- read.table (file = "Throughput.csv", header = TRUE, sep = ",")
retain <- read.table (file = "Retainability.csv", header = TRUE, sep = ",")
access <- read.table (file = "Accessibility.csv", header = TRUE, sep = ",")

thrput1 <- lm(DLThr ~ vendor + os + hour+ traffic_volume_dl_mb + traffic_volume_ul_mb  +	ecno +	dl_iub_usage+ ul_iub_usage +	n_hsusers+	rscp+ul_interference, data=thrput)
summary(thrput1)
install.packages("coefplot")
require(coefplot)
coefplot(thrput1)

