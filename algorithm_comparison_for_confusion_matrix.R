rm(list=ls(all=TRUE))

# Carica i pacchetti necessari
library(dplyr)

calcPerformance<-function(A_1_B_1,A_0_B_1,A_1_B_0,A_0_B_0,comparisondf){
  accuracy<-(nrow(A_1_B_1)+nrow(A_0_B_0))*100/nrow(comparisondf)
  
  # Contingency table
  xtab <- as.table(rbind(c(nrow(A_1_B_1),nrow(A_1_B_0)), 
                         c(nrow(A_0_B_1), nrow(A_0_B_0))))
  # Descriptive statistics
  diagonal.counts <- diag(xtab)
  N <- sum(xtab)
  row.marginal.props <- rowSums(xtab)/N
  col.marginal.props <- colSums(xtab)/N
  # Compute kappa (k)
  Po <- sum(diagonal.counts)/N
  Pe <- sum(row.marginal.props*col.marginal.props)
  k <- (Po - Pe)/(1 - Pe)
  cat("Accuracy=",accuracy,"%\n")
  cat("Kappa=",k,"\n")
  return(c(accuracy,k))
}



# Definizione degli approcci e degli anni
approcci <- c("MultiKmeans", "FCM", "Xmeans", "DBSCAN","VAE","ANN")

anni <- 2017:2021

# Creazione di una nuova cartella per i file di output
output_folder <- "output_csv"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Funzione per cercare un file dato un approccio e un anno
trova_file <- function(approccio, anno) {
  file_list <- list.files(pattern = paste0(".*", approccio, ".*", anno, ".*\\.csv$"))
  if (length(file_list) == 1) {
    return(file_list[1])
  } else if (length(file_list) > 1) {
    stop(paste("Trovati più file per", approccio, "e", anno, ": specificare meglio i nomi."))
  } else {
    stop(paste("File non trovato per", approccio, "e", anno, "."))
  }
}

# Funzione per il confronto di due approcci
confronta_approcci <- function(approccio1, approccio2, anno) {
  cat("\n####",approccio1,"vs",approccio2,"in",anno,"\n")
  file1 <- trova_file(approccio1, anno)
  file2 <- trova_file(approccio2, anno)
  cat("reading files\n")
  data1 <- read.csv(file1)
  data2 <- read.csv(file2)
  xy_data1<-paste0(data1$longitude,";",data1$latitude)
  xy_data2<-paste0(data2$longitude,";",data2$latitude)
  ldt1<-length(which( (xy_data1%in%xy_data2)==T))
  ldt2<-length(which( (xy_data2%in%xy_data1)==T))
  if (ldt1!=length(xy_data1) || ldt2!=length(xy_data2) ||  length(xy_data1)!=length(xy_data2)){
    cat("ERROR WITH DATA LENGTH : INCOMPATIBLE DATA")
  }
  
  cat("merging files\n")
  
  #for(r in dim(data1)[1]){
  
  #prendere la riga corrispondente in data2
  #prendere il risultato di hotspot in data2
  #prendere il risultato di hotspot in data1
  #costruire una nuova riga (longitude, latitude, A, B)
  #aggiungere la riga ad un dataframe
  
  #}
  
  #check if one of the datasets is VAE
  if("reconstruction_log_probability" %in% names(data1)){
    probabilities<-data1$reconstruction_log_probability
    hotspots<-rep(0,length(probabilities))
    pthreshold<-as.numeric(quantile(probabilities)[3]) #50th perc
    hotspots[which(probabilities<=pthreshold)]<-1
    data1$hotspot<-hotspots
  }
  if("reconstruction_log_probability" %in% names(data2)){
    probabilities<-data2$reconstruction_log_probability
    hotspots<-rep(0,length(probabilities))
    pthreshold<-as.numeric(quantile(probabilities)[3]) #50th perc
    hotspots[which(probabilities<=pthreshold)]<-1
    data2$hotspot<-hotspots
  }
  if("distance_class_interpretation" %in% names(data1) && is.numeric(data1$distance_class_interpretation)){
    probabilities<-data1$distance_class_interpretation
    hotspots<-rep(0,length(probabilities))
    pthreshold<-as.numeric(quantile(probabilities)[3]) #50th perc
    hotspots[which(probabilities>=pthreshold)]<-1
    data1$hotspot<-hotspots
  }
  if("distance_class_interpretation" %in% names(data2) && is.numeric(data2$distance_class_interpretation)){
    probabilities<-data2$distance_class_interpretation
    hotspots<-rep(0,length(probabilities))
    pthreshold<-as.numeric(quantile(probabilities)[3]) #50th perc
    hotspots[which(probabilities>=pthreshold)]<-1
    data2$hotspot<-hotspots
  }
  
  # Inizializzazione del dataframe per raccogliere i risultati
  comparisondf <- data.frame(longitude = numeric(0), latitude = numeric(0), A = numeric(0), B = numeric(0))
  
  # Ciclo per confrontare le righe corrispondenti
  for (r in 1:nrow(data1)) {
    # Prendere la riga corrispondente in data2
    row_data1 <- data1[r, ]
    xy_data1_row<-xy_data1[r]
    r2<-which(xy_data2 == xy_data1_row)
    row_data2 <- data2[r2, ]
    
    # Prendere i risultati di hotspot (A per data1 e B per data2)
    A <- row_data1$hotspot
    
    B <- row_data2$hotspot
    
    # Costruire una nuova riga
    new_row <- data.frame(longitude = row_data1$longitude, latitude = row_data1$latitude, A = A, B = B)
    
    # Aggiungere la riga al dataframe
    comparisondf <- rbind(comparisondf, new_row)
  }
  
  cat("comparing files\n")
  threshold1_A=0.9
  threshold1_B=0.9
  
  A_1_B_1 = comparisondf[which(comparisondf$A>threshold1_A & 
                                 comparisondf$B>threshold1_B),]
  
  A_0_B_1 = comparisondf[which(comparisondf$A<=threshold1_A & 
                                 comparisondf$B>threshold1_B),]
  
  A_1_B_0 = comparisondf[which(comparisondf$A>threshold1_A & 
                                 comparisondf$B<=threshold1_B),]
  
  A_0_B_0 = comparisondf[which(comparisondf$A<=threshold1_A & 
                                 comparisondf$B<=threshold1_B),]
  accuracy_k<-calcPerformance(A_1_B_1,A_0_B_1,A_1_B_0,A_0_B_0,comparisondf)
  return(accuracy_k)
  #output_file <- file.path(output_folder, paste0(approccio1, "_", approccio2, "_", anno, ".csv"))
  #write.csv(merged_data, file = output_file, row.names = FALSE)
}

# Ciclo su tutte le combinazioni di approcci e anni
comparison_df_total<-data.frame(method1=as.character(),method2=as.character(),year=as.integer(),accuracy=as.numeric(),kappa=as.numeric())
for (anno in anni) {
  combinazioni <- combn(approcci, 2, simplify = FALSE)
  for (coppia in combinazioni) {
    approccio1 <- coppia[1]
    approccio2 <- coppia[2]
    accuracy_k<-confronta_approcci(approccio1, approccio2, anno)
    rowtoinsert<-c(approccio1,approccio2,anno,accuracy_k[1],accuracy_k[2])
    comparison_df_total<-rbind(comparison_df_total,rowtoinsert)
  }
  #stooop
}

names(comparison_df_total)<-c("method1","method2","year","accuracy","kappa")
write.csv(file="comparisons.csv",x=comparison_df_total,row.names = F)