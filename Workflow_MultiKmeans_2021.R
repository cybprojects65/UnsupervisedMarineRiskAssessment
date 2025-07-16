rm(list = ls())
library(mclust)
options(warn = -1)

# File to select
mediterranean <- read.csv(file="./Dataset_Risk_Assessment_Mediterranean_Sea.csv", header=TRUE, sep=",")

# Selection of my study dataframe
# Bounding box coordinates
bbxmin=-180
bbxmax=180
bbymin=-90 # latitude minimum
bbymax=90

# Year of interest
year = "2021"

# Selection of my study area and features
selection<-mediterranean[which((mediterranean$longitude>=bbxmin)&(mediterranean$longitude<=bbxmax)&(mediterranean$latitude>=bbymin)&(mediterranean$latitude<=bbymax)), ]


#land distance e mean depth sono INVERTITI (1/n) anche se nei nomi qui non c'è scritto inv
selected_features<-c("longitude","latitude",
                     "environment.2021_land_distance",
                     "environment.2021_mean_depth",
                     "environment.2021_net_primary_production",
                     "environment.2021_sea.bottom_dissolved_oxygen",
                     "fishing.activity.2021_total_fishing",
                     "species.richness.2021",
                     "stocks.richness.2021",
                     "thermohalinity_2021"
)

cat(paste0("***Initialization***", "\n"))

# n_centroidi <- si sono utilizzati 4 centroidi
multi_centroidi<-  c(4)        #seq(from=2, to=20, by=1)
N <- 50

bics<-c()
for (n_centroidi in multi_centroidi){
  
  selected_features_coords <- selection[,selected_features]
  # Study of centroids
  cat(paste0("Study of centroids", "\n"))
  # Prepare the dataset
  v <- as.data.frame(selected_features_coords[,3:ncol(selected_features_coords)])
  
  cat("####I'm analyzing ",n_centroidi,"centroids\n")
  
  # Create an EMPTY matrix of centroids to be filled later
  centroidi <- matrix(nrow=n_centroidi, ncol=ncol(v))
  
  for (centroide in 1:nrow(centroidi)) {
    
    centroidi[centroide,] <- as.numeric(v[centroide,])
    
  }
  
  
  
  # Empty vector to be filled with the assignment of the centroid for each row of v
  d <- numeric(length = nrow(v))
  
  v$distance_class <- NA
  prev_centr_distr<-rep(0,n_centroidi)
  for (k in 1:N) {
    cat(paste0("I am executing loop number ", k, "\n"))
    
    cat(paste0("***Assignment***", "\n"))
    
    for (punto in 1:nrow(v)) {
      
      distanze<-sapply(1:n_centroidi, function(centroide){
        
        d_vi_centroide = sqrt( sum ((v[punto,1:(ncol(v)-1)]-centroidi[centroide,])*(v[punto,1:(ncol(v)-1)]-centroidi[centroide,])) )
        return(d_vi_centroide)
        
      },simplify = T)
      
      d[punto] <- which(distanze == min(distanze))[1]  # Index of the smallest value to assign to each point
    }
    
    cat(paste0("***FILE***", "\n"))
    
    # Assign the column with the centroid value to the original dataset.
    selected_features_coords$distance_class <- d
    
    v$distance_class <- d
    
    # Empty matrix to store the averages
    v_medie <- matrix(0,nrow = nrow(centroidi), ncol = ncol(centroidi))
    centroid_distribution<-c()
    for (centroide in 1:nrow(centroidi)) {
      cat(paste0("---- I am examining the centroid ", centroide, "\n"))
      # Take the rows with the centroid of interest: rows = the row of the centroid, columns = v minus the column with the centroid number
      v_centroide <- v[v$distance_class==centroide, -which(colnames(v)=="distance_class")]
      v_centroide<-as.matrix(v_centroide)
      if (nrow(v_centroide)==0){
        cat(paste0("The points assigned to the centroid ", centroide, " are ", nrow(v_centroide), "\n"))
        v_medie[centroide,]<-centroidi[centroide,]
        centroid_distribution<-c(centroid_distribution,0)
      }else{
        # Calculate the mean on this new set of data selected for the centroid.
        cat(paste0("The points assigned to the centroid ", centroide, " are ", nrow(v_centroide), "\n"))
        medie_centroide<-apply(v_centroide,2,mean)
        v_medie[centroide,] <- medie_centroide
        centroid_distribution<-c(centroid_distribution,nrow(v_centroide))
      }
    }
    cat(paste0("***Update***", "\n"))
    centroidi <- v_medie
    
    if (length(which(centroid_distribution!=prev_centr_distr))==0){
      cat(paste0("***Convergence***", "\n"))
      break
    }else{
      prev_centr_distr<-centroid_distribution
    }
  }
  
  centroidi_df<-as.data.frame(centroidi)
  names(centroidi_df) <- names(v)[1:(length(v)-1)]
  
  ks.test(centroid_distribution, "punif")
  
  #############
  
  
  # Quantiles
  v_quantili <- apply(v, 2, quantile)
  
  # Prepare the centroids matrix with "M" for medium
  centroidi_labelled <- matrix("M", nrow=nrow(centroidi), ncol=ncol(centroidi))
  
  # Filling labeled centroids
  for (centroide in 1:nrow(centroidi)) {
    for (feat in 1:(ncol(v)-1)) {
      if (centroidi[centroide,feat]<v_quantili[3,feat]){
        centroidi_labelled[centroide, feat] <- "L"
      }
      else if (centroidi[centroide,feat]>v_quantili[4,feat]) {
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
  
  
  
  v$distance_class_interpretation <- NA
  for (i in 1:nrow(centroidi)) {
    indici <- which(v$distance_class == i)
    interpretazione <- centroide_interpretazione[i]
    v$distance_class_interpretation[indici] <- interpretazione
  }
  
  cat("Saving the dataset\n")
  nuovo_v <- cbind(selected_features_coords[,1:2],v)
  names(nuovo_v)<-c(names(selected_features_coords),"distance_class_interpretation")
  output_file<-paste0("./centroid_classification_assignment_",n_centroidi,"_", year, ".csv")
  write.csv(nuovo_v, output_file, row.names = F)
  
  #### CALCULATING ChiSqr
  if (length(which(centroid_distribution<=2))>0 || 
      ( (min(centroid_distribution)/max(centroid_distribution) ) <0.007) 
  ){
    cat("Unsuitable distribution: low uniformity:",(min(centroid_distribution)/max(centroid_distribution))," --- outliers: ",length(which(centroid_distribution<=2)),"\n")
    bic<-0
  }else{
    centroid_distribution.norm<-centroid_distribution/sum(centroid_distribution)
    reference<-rep(mean(centroid_distribution),length(centroid_distribution) )
    reference.norm<-reference/sum(reference)
    #chi<-chisq.test(centroid_distribution.norm, p = reference.norm)
    #chi<-chisq.test(centroid_distribution.norm*1000, p = reference.norm*1000, rescale.p = TRUE)
    #bic<-chi$p.value
    chisq<-sum((centroid_distribution.norm*1000-reference.norm*1000)^2/(reference.norm*1000))/length(centroid_distribution.norm)
    bic<-1/chisq
  }
  cat("ChiSqr:",bic,"\n")
  bics<-c(bics,bic)
  cat("Done\n")
}

best_clusterisation<-multi_centroidi[which(bics == max(bics))]
cat("Ks: ",multi_centroidi,"\n")
cat("ChiSQRs: ",bics,"\n")
cat("Best clustering: K=",best_clusterisation,"\n")
best_clusterisation_file = paste0("./centroid_classification_assignment_",best_clusterisation,".csv")
cat("Best clustering file to take as result:",best_clusterisation_file,"\n")