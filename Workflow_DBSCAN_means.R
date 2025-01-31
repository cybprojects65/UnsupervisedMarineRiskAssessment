#DBSCAN
# Remove  all objects from R memory 
rm(list=ls())

library(EnvStats)
library(dplyr)
library(dbscan)
library(tidyverse)

# dataset
mediterranean_sea <- read.csv(file="./Dataset_Risk_Assessment_Mediterranean_Sea.csv", header=TRUE, sep=",")

# select columns 
data <- mediterranean_sea %>%
  select(longitude, latitude, 
         environment.2021_land_distance,
         environment.2021_mean_depth,
         environment.2021_net_primary_production,
         environment.2021_sea.bottom_dissolved_oxygen,
         fishing.activity.2021_total_fishing,
         species.richness.2021,
         stocks.richness.2021,
         thermohalinity_2021)

##### RANGE OF ACTION FOR DBSCAN #####
# Select only numeric columns
data_numeric <- data %>% select(-longitude, -latitude)

# Calculate the Euclidean distance matrix
dist_matrix <- dist(data_numeric, method = "euclidean")

# Convert the distance matrix to a conventional matrix
dist_matrix <- as.matrix(dist_matrix)

# Replace the diagonal of the matrix with NA to avoid considering the distance with itself
diag(dist_matrix) <- NA

# Find the minimum distance for each line
min_distance <- apply(dist_matrix, 1, min, na.rm = TRUE)

# Add the minimum distance as a new column to the original data frame
data <- data %>% mutate(MinimumDistance = min_distance)

##### Range for DBSCAN ####
eps_median <- median(data$MinimumDistance)     


data <- subset(data, select = -MinimumDistance)

##### DBSCAN #####
v <- data[, c("environment.2021_land_distance",
              "environment.2021_mean_depth",
              "environment.2021_net_primary_production",
              "environment.2021_sea.bottom_dissolved_oxygen",
              "fishing.activity.2021_total_fishing",
              "species.richness.2021",
              "stocks.richness.2021",
              "thermohalinity_2021")]

# Apply DBSCAN
dist_matrix <- dist(v)
dbscan_result <- dbscan(dist_matrix, eps = eps_median, minPts = 3)  ##### eps #####

# Add clustering results to the dataset
data_with_clusters <- cbind(data, cluster = dbscan_result$cluster)

max_cluster <- max(data_with_clusters$cluster)

# Replace the 0 values with the maximum + 1
data_with_clusters$cluster[data_with_clusters$cluster == 0] <- max_cluster + 1


# Calculate centroids for each cluster
cluster_centroids <- data_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(all_of(names(v)), mean, na.rm = TRUE))

cluster_centroids <- cluster_centroids %>% select(-cluster)

# Quantiles
v_quantili <- apply(v, 2, quantile)

# Prepare the centroids matrix with "M" for medium
centroidi_labelled <- matrix("M", nrow=nrow(cluster_centroids), ncol=ncol(cluster_centroids))

colnames(centroidi_labelled) <- colnames(cluster_centroids)              #################### aggiunto 16/09

###############################################################################
######                   INTERPRETATION OF QUANTILES                     ######
###############################################################################

# Multi K-means uses quartiles: 3 and 4

# Filling labeled centroids
for (centroide in 1:nrow(cluster_centroids)) {
  for (feat in 1:(ncol(v))) {
    if (cluster_centroids[centroide,feat]<v_quantili[3,feat]){    
      centroidi_labelled[centroide, feat] <- "L"
    }
    else if (cluster_centroids[centroide,feat]>v_quantili[4,feat]) {       
      centroidi_labelled[centroide, feat] <- "H"
    }
    
  }
  
}



# Counting L, M, H to decide the level of risk
c_H <- matrix(nrow = nrow(centroidi_labelled), ncol=1)
c_M <- matrix(nrow = nrow(centroidi_labelled), ncol=1)
c_L <- matrix(nrow = nrow(centroidi_labelled), ncol=1)

# Creating empty vector for risk interpretation centroids
centroide_interpretazione <- matrix(nrow = nrow(centroidi_labelled), ncol=1)

# Counting the letters from centroidi_labelled
for (r in 1:nrow(centroidi_labelled)) {
  c_H[r] <- sum(centroidi_labelled[r,] == "H")
  c_M[r] <- sum(centroidi_labelled[r,] == "M")
  c_L[r] <- sum(centroidi_labelled[r,] == "L")
}  

# Assignment 
for (i in 1:nrow(centroide_interpretazione)) {
  if (c_H[i]>c_L[i] & c_H[i]>c_M[i]) {
    centroide_interpretazione[i] <- "high risk"
  }
  else if (c_L[i]>=c_H[i] & c_L[i]>c_M[i]) {
    centroide_interpretazione[i] <- "low risk"
  }
  else{ 
    centroide_interpretazione[i] <- "medium risk"
  }
}

###############################################################################


data_with_clusters$distance_class_interpretation <- NA
for (i in 1:nrow(cluster_centroids)) {
  indici <- which(data_with_clusters$cluster == i)
  interpretazione <- centroide_interpretazione[i]
  data_with_clusters$distance_class_interpretation[indici] <- interpretazione
}

###############################################################################
######                       ADD COLUMN "HOTSPOT"                        ######
###############################################################################

# Add new column "hotspot" with initial values 0
data_with_clusters$hotspot <- 0


# Loop over each row of the dataframe
for (i in 1:nrow(data_with_clusters)) {
  # Check if the column "distance_class_interpretation" contains "high risk"
  if (data_with_clusters$distance_class_interpretation[i] == "high risk") {
    # Replace 0 with 1 in the "hotspot" column
    data_with_clusters$hotspot[i] <- 1
  }
}

###############################################################################

write.csv(data_with_clusters, file="./cluster_DBSCAN_2021.csv", row.names = FALSE)