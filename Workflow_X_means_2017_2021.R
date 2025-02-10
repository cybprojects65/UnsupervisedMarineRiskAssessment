# Remove  all objects from R memory 
rm(list=ls())

#setwd("C:/Users/laura/Documents/Risk assessment_NUOVO/X means")

################################################################################
#####                        X-means con Java                              #####
################################################################################

cluster_data2<-"./Dataset_Risk_Assessment_Mediterranean_Sea.csv"

min_elements_in_cluster <- 2
minimum_n_of_clusters <- 2
maximum_n_of_clusters <- 500
maximum_iterations <- 100


# Create a folder for each year to differentiate all the years
for (anno in 2017:2021) {
  
  outfolder <- paste0("xmeans_clusters_",anno)
  
  features2 <- paste0("\"environment.",anno,"_land_distance\" \"environment.",anno,"_mean_depth\" \"environment.",anno,"_net_primary_production\" \"environment.",anno,"_sea.bottom_dissolved_oxygen\" \"fishing.activity.",anno,"_total_fishing\" \"species.richness.",anno,"\" \"stocks.richness.",anno,"\" \"thermohalinity_",anno,"\"")
  
  
  command<-paste0("java -jar ./XmeanCluster.jar \"",cluster_data2,"\" ",min_elements_in_cluster," ",minimum_n_of_clusters," ",maximum_n_of_clusters," ",maximum_iterations," ",outfolder," ",features2)
  XMeanCluster_execution<-system(command, intern = T,
                                 ignore.stdout = FALSE, ignore.stderr = FALSE,
                                 wait = TRUE, input = NULL, show.output.on.console = TRUE,
                                 minimized = FALSE, invisible = TRUE)
  
  execution_success<-(length(which(grepl(pattern="OK MaxEnt",x=XMeanCluster_execution)))>0)
  
}

################################################################################
#####              Analysis of centroids and clusters                      #####
################################################################################
# Remove  all objects from R memory 
rm(list=ls())

# Check which directory you are working in
setwd("C:/Users/laura/Documents/A_RIPROVA_XMEANS/xmeans_clusters_2021")

# Remove  all objects from R memory 
rm(list=ls())

library(dplyr)

# dataset
data <- read.csv(file="./clustering_table_xmeans.csv", header=TRUE, sep=",")

# rename column
data <- data %>%
  rename(cluster = clusterid)

data_with_clusters <- data

# Replace the 0 values with the maximum + 1
max_cluster <- max(data_with_clusters$cluster)
data_with_clusters$cluster[data_with_clusters$cluster == 0] <- max_cluster + 1

v <- data[, c("environment.2021_land_distance",
              "environment.2021_mean_depth",
              "environment.2021_net_primary_production",
              "environment.2021_sea.bottom_dissolved_oxygen",
              "fishing.activity.2021_total_fishing",
              "species.richness.2021",
              "stocks.richness.2021",
              "thermohalinity_2021")]

# Calculate centroids for each cluster
cluster_centroids <- data_with_clusters %>%
  group_by(cluster) %>%
  summarise(across(all_of(names(v)), mean, na.rm = TRUE))

cluster_centroids <- cluster_centroids %>% select(-cluster)

# Quantiles
v_quantili <- apply(v, 2, quantile)

# Prepare the centroids matrix with "M" for medium
centroidi_labelled <- matrix("M", nrow=nrow(cluster_centroids), ncol=ncol(cluster_centroids))


###############################################################################
######                  INTERPRETATION OF QUANTILES                      ######
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

write.csv(data_with_clusters, file="./cluster_Xmeans_2021.csv", row.names = FALSE)

