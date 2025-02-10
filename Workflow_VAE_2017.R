################################################################################
#####                        VAE con Java                                  #####
################################################################################

################################################################################
#####                        Training                                      #####
################################################################################
#########PREREQUISITE: VAE-MODEL JAR FILE DOWNLOADABLE FROM#####################
#https://data.d4science.org/shub/E_RmlXSjJSbFVhZmVyT25YTFJJYlY1a3BJRWc0T0xueUVIOWNXamR3dStNV3RMZDl2WThJRE5rckY0b1cwWVU1Kw==
################################################################################

anno <- "2017"      #il 2017 è il training, per passare agli anni dopo sostituisci qui 


#input_file_path <-"Complete_dataset_mediterranean_sea_2021_2021_2021_2021_2021_2050RCP8.5.csv"
input_file_path <-paste0("centroid_classification_assignment_4_",anno,".CSV")
#variable_names<- "environment 2021_land_distance,environment 2021_mean_depth"
variable_names<- paste0("environment.",anno,"_land_distance,environment.",anno,"_mean_depth,environment.",anno,"_net_primary_production,environment.",anno,"_sea.bottom_dissolved_oxygen,fishing.activity.",anno,"_total_fishing,species.richness.",anno,",stocks.richness.",anno,",thermohalinity_",anno)
valutazione <- FALSE

number_of_hidden_nodes <- 8
number_of_epochs <- 1000
output_folder <- paste0("./out_",anno,"_n",number_of_hidden_nodes,"_test/")

model_folder <- paste0("./out_2017_n",number_of_hidden_nodes,"_test/")
number_of_reconstruction_samples <- 16
trained_model_file<-paste0(model_folder,"model_norm_1037X8_633f32729927533133342832429035e37b29233d30c28a30#8.bin")
#prima di lanciare la fase di test/predizione ricordarsi di rinominare tutti i file nella cartella out come semplicemente "model"
training_mode_active<-"true"


if(training_mode_active=="true"){
  
  
  command_training<-paste0("java -cp vae.jar it.cnr.anomaly.JavaVAE -i\"./",input_file_path,"\" -v\"",variable_names,"\" -o\"",output_folder,"\" -h",number_of_hidden_nodes," -e",number_of_epochs," -r",number_of_reconstruction_samples," -t",training_mode_active)
  
  
  VAU_execution_train<-system(command_training, intern = TRUE,
                              ignore.stdout = FALSE, ignore.stderr = FALSE,
                              wait = TRUE, input = NULL, show.output.on.console = TRUE,
                              minimized = FALSE, invisible = TRUE)
  
  
  execution_train_success<-(length(which(grepl(pattern="OK VAU Training",x=VAU_execution_train)))>0)
  log_file <- paste0(output_folder,"log_file_training_",anno,".txt")
  writeLines(VAU_execution_train, log_file)
}else{
  dir.create(output_folder)
  ################################################################################
  #####                           Test                                       #####
  ################################################################################
  
  
  
  command_test <- paste0("java -cp vae.jar it.cnr.anomaly.JavaVAE -i\"./",input_file_path,"\" -v\"",variable_names,"\" -o\"",output_folder,"\" -r",number_of_reconstruction_samples," -t",training_mode_active," -m\"",trained_model_file,"\"")
  
  
  VAU_execution_test<-system(command_test, intern = T,
                             ignore.stdout = FALSE, ignore.stderr = FALSE,
                             wait = TRUE, input = NULL, show.output.on.console = TRUE,
                             minimized = FALSE, invisible = TRUE)
  
  execution_train_success<-(length(which(grepl(pattern="OK VAU Test",x=VAU_execution_test)))>0)
  log_file <- paste0(output_folder,"log_file_test_",anno,".txt")
  writeLines(VAU_execution_test, log_file)
  
  ################################################################################
  #####                           valutazione                                #####
  ################################################################################
  
  
  file_pattern <- "classification_test_"
  files <- list.files(path = output_folder, pattern = paste0("^", file_pattern))
  if (length(files) == 1) {
    # Costruire il percorso completo del file
    file_path <- file.path(output_folder, files[1])
    
    # Leggi il file CSV
    data_projected <- read.csv(file_path,header = TRUE)
    # Visualizza i primi 6 dati
    head(data)
  } else {
    cat("Più di un file trovato o nessun file trovato.")
  }
  namelist<- unlist(strsplit(variable_names, split = ","))
  data_projected_rdx <- data_projected[,namelist]
  
  data_input<-read.csv(input_file_path,header = TRUE)
  data_input <- data_input[,namelist]
  
  vettore_differenza <- data_projected_rdx - data_input
  vettore_differenza_vector <- unlist(vettore_differenza)
  vettore_differenza_numeric <- as.numeric(vettore_differenza_vector)
  errore <- mean((as.numeric(vettore_differenza_numeric))^2)
  
  
  rec_prob_avg<-mean(data_projected$reconstruction_log_probability)
  cat(paste0("error =",errore,", average probability recostruction =",rec_prob_avg),"\n")
  
  
  file_pattern <- "classification_test_"
  files <- list.files(path = output_folder, pattern = paste0("^", file_pattern))
  
  data2 <- read.csv(paste0(output_folder,files), header=TRUE, sep=",")    # questo è il file che mi genera il vae come output
  
  data3 <- read.csv(input_file_path, header=TRUE, sep=",")    #questo è l'originale input multi k means
  
  data4 <- cbind(data3$longitude, data3$latitude, data2$reconstruction_log_probability)
  colnames(data4) <- c("longitude", "latitude", "reconstruction_log_probability")
  
  write.csv(data4, paste0(output_folder, "output_VAE_", anno, ".csv"), row.names = FALSE)
}