#fuzzy C-means

rm(list = ls())
#install.packages("e1071")

library(e1071)
library(dplyr)

# file
mediterranean <- read.csv(file="./Dataset_Risk_Assessment_Mediterranean_Sea.csv", header=TRUE, sep=",")
# in Dataset_Risk_Assessment_Mediterranean_Sea.csv le colonne land distance e mean depth sono invertite 1/n

# land distance e mean depth sono invertiti cioè 1/n anche se nei nomi qui non è indicato
# select columns
data <- mediterranean %>%
  select(longitude, latitude, 
         environment.2018_land_distance,
         environment.2018_mean_depth,
         environment.2018_net_primary_production,
         environment.2018_sea.bottom_dissolved_oxygen,
         fishing.activity.2018_total_fishing,
         species.richness.2018,
         stocks.richness.2018,
         thermohalinity_2018)

#remove longitude and latitude
v <- data [, c( "environment.2018_land_distance",
                "environment.2018_mean_depth",
                "environment.2018_net_primary_production",
                "environment.2018_sea.bottom_dissolved_oxygen",
                "fishing.activity.2018_total_fishing",
                "species.richness.2018",
                "stocks.richness.2018",
                "thermohalinity_2018")]


#number of clusters
n_cluster <- 4    
#parameters
m <- 2
result <- cmeans(v, centers = n_cluster, m = m) 

# Data Membership Degree Matrix
fuzzy_membership_matrix <- result$membership

# Cluster Prototype Evolution Matrices
initial_centers <- result$centers
final_centers <- t(result$centers)
cluster_membership <- as.data.frame(result$membership)
data_with_clusters <- cbind(v, cluster_membership)
#head(data_with_clusters)

data_with_clusters_coordinates <- cbind(data[, c("latitude", "longitude")], data_with_clusters)


centroids <- result$centers
#print(centroids)


# Calculation of the quantiles of the dataset V
v_quantili <- apply(v, 2, quantile)

# Prepare the centriod matrix with "M" for medium
centroidi_labelled <- matrix("M", nrow=nrow(centroids), ncol=ncol(centroids))

# Filling labeled centroids
for (centroide in 1:nrow(centroids)) {
  for (feat in 1:(ncol(v)-1)) {
    if (centroids[centroide,feat]<v_quantili[3,feat]){
      centroidi_labelled[centroide, feat] <- "L"
    }
    else if (centroids[centroide,feat]>v_quantili[4,feat]) {
      centroidi_labelled[centroide, feat] <- "H"
    }
    
  }
  
}
# Count L, M, H to decide what risk it is
c_H <- matrix(nrow = nrow(centroidi_labelled), ncol=1)
c_M <- matrix(nrow = nrow(centroidi_labelled), ncol=1)
c_L <- matrix(nrow = nrow(centroidi_labelled), ncol=1)

# Creation of empty vector of centroid risk interpretation
centroide_interpretazione <- matrix(nrow = nrow(centroidi_labelled), ncol=1)

# Count the letters from centroidi_labelled
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

# keep columns and NA -> get a list of clusters if above the threshold
threshold <- 0.3
cluster_membership <- cluster_membership %>% mutate_all(~ replace(., . < threshold, NA))

# Rename the columns with the assigned risk
index_cluster_high <- c()
for (i in 1:length(centroide_interpretazione)) {
  if (centroide_interpretazione[i] == "high risk") {
    index_cluster_high <- c(index_cluster_high,i)
  }
  
}
cluster_membership_high <- cluster_membership[, index_cluster_high]

#colnames(cluster_membership) <- centroide_interpretazione


# Merge the Mediterranean file with the new fuzzy C-means columns
union <- cbind(data,cluster_membership_high)

#add column "hotspot"
union$hotspot <- 0

for(riga in 1:nrow(union)) {
  for(colonna in (ncol(v)+3):(ncol(union)-1)) {
    if(!is.na(union[riga,colonna])){
      union$hotspot[riga] <- 1
      
    }
  }
}


write.csv(union, file = "cluster_FCM_2018.csv", row.names = FALSE)