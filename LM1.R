
# We assume the file Example1.csv is located in folder C:/R/my_data
setwd("C:/GSU/Oct-12")
d01 <- read.table (file = "Throughput.csv", header = TRUE, sep = ",")
#d01 <- read.table (file = "Retainability.csv", header = TRUE, sep = ",")
#d01 <- read.table (file = "Accessibility.csv", header = TRUE, sep = ",")

		
#### Let's try LM using only features that are numeric
fitAllNumericInput <- lm(DLThr ~ traffic_volume_dl_mb + traffic_volume_ul_mb	+	ecno +	dl_iub_usage+ ul_iub_usage +	n_hsusers+	rscp+ul_interference, data=d01)
summary(fitAllNumericInput)

#### Now try to add categorical features. Start with "os"
fitWithSomeCategoricalInput <- lm(DLThr ~ os + traffic_volume_dl_mb + traffic_volume_ul_mb  +	ecno +	dl_iub_usage+ ul_iub_usage +	n_hsusers+	rscp+ul_interference, data=d01)
summary(fitWithSomeCategoricalInput)
require(coefplot)
coefplot(fitWithSomeCategoricalInput)


#### See what happens when you add "model"
#### This will take much longer to run. Be patient
fitWithSomeCategoricalInput2 <- lm(DLThr ~ os + model + traffic_volume_dl_mb + traffic_volume_ul_mb  +	ecno +	dl_iub_usage+ ul_iub_usage +	n_hsusers+	rscp+ul_interference, data=d01)
summary(fitWithSomeCategoricalInput2)
coefplot(fitWithSomeCategoricalInput2)


#### Of course, if you try all inputs then LM will simply crash
fitAllInputs <- lm(DLThr ~ ., data=d01)
summary(fitAllInputs)


