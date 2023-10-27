# Assignment 5 - K-Means Algorithm

# Load the required libraries
library(tidyverse)
library(stats)
library(ggfortify)
library(factoextra)

# 1. Load the dataset mammal_Sleep.csv into a data frame.

source_data <- read.csv("mammal_sleep.csv")

# 2. Create a new data frame mammalSleep with sleep_total and sleep_rem.
## first I will fill any null values in sleep_total and sleep_rem
## since they will interfere with the model

sleepTotal_mean <- mean(source_data$sleep_total, na.rm=TRUE)
sleepREM_mean <- mean(source_data$sleep_rem, na.rm=TRUE)

source_data <- source_data %>%
  mutate(sleep_total = replace_na(sleep_total, sleepTotal_mean))
source_data <- source_data %>%
  mutate(sleep_rem = replace_na(sleep_rem, sleepREM_mean))

# subset the data
mammalSleep <- source_data %>% select("sleep_total", "sleep_rem")

# 3. Fit a k-means clustering model with 4 clusters to the data subset mammalSleep.

# scale my data
mammalSleep_scaled <- scale(mammalSleep)

# create the model
set.seed(1)
mammal_km = kmeans(mammalSleep_scaled, centers = 4, nstart = 100)

# 4. Find the centroids of the clusters in the model on the chart 

# plot the clusters
km.clusters <- mammal_km$cluster
rownames(mammalSleep_scaled) <- paste(source_data$name,
                                     1:dim(source_data)[1],
                                     sep="_")

# I found it easier to see the centroids on my chart
# Without labels, if you'd like to see the labelled version,
# remove the geom=c("point") argument.
fviz_cluster(list(data=mammalSleep_scaled, 
                  cluster = km.clusters),
             geom=c("point"))

# centroid values
centroids <- mammal_km$centers
print(centroids)