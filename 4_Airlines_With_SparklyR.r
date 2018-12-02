library(ggplot2)
library(maps)
library(geosphere)

library(sparklyr)
library(dplyr)

# Connect to the cluster
# Getting ip address of the master node
cluster_url <- paste0("spark://", system("hostname -i", intern = TRUE), ":7077")
# connect
sc <- spark_connect(master = cluster_url)

# Import data to cluster
# For the full ~30GB dataset see https://packages.revolutionanalytics.com/datasets/
# install.packages("nycflights13")
flights <- copy_to(sp_c, flights, "flights")
airlines <- copy_to(sp_c, airlines, "airlines")

# Join flights to airlines and select columns
modified_flights <- left_join(flights,airlines,by="carrier")

# Munge using dplyr to get delays and distances by tail number
delay_by_tailno <- modified_flights %>% 
  group_by(tailnum) %>%
  summarise(count = n(), dist = mean(distance), delay = mean(arr_delay)) %>%
  filter(count > 20, dist < 2000, !is.na(delay)) %>%
  collect

#Get delays by carrier & state
delay_by_state <- modified_flights %>%
  group_by(CARRIER, ORIGIN_STATE_ABR) %>% 
  summarize(DEP_DELAY_AVG = mean(DEP_DELAY)) %>%
  top_n(10, DEP_DELAY_AVG) %>%
  collect()

# Train a random forest model.
# See https://www.rpubs.com/aymansir/usflightdelay
# for a local example
# Some pointers.... https://raw.githubusercontent.com/Azure/Azure-MachineLearning-DataScience/master/Misc/KDDCup2016/Code/SparkR/SparkR_sparklyr_NYCTaxi.Rmd
#
# partitions <- myDF %>% sdf_partition(training = 0.75, test = 0.25, seed = 1099)
# fit <- partitions$training %>% ml_linear_regression(response = "y_value", 
#  features = c("feature1", "feature2"), alpha = 0.5, lambda = 0.01)
# summary(fit)
# predictedVals <- sdf_predict(fit, newdata =  partitions$test)
# predictedDF <- as.data.frame(predictedVals)
# Rsqr = cor(predictedDF$y_value, predictedDF$prediction)^2; Rsqr;
