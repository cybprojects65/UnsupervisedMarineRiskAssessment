#per l'addestramento settare l'year 2017 e training (riga 7) a true
# per gli anni successuvi al 2017 cambiare l'year con il replace (tranne nella dicitura in riga 99) e mettere training a F

library(neuralnet)
#setwd("C:/Users/UTENTE/Desktop/now/Reti per RISK ASSESSMENT")
year<-"2021"

training<- F
data = read.csv(paste0("cluster_MultiKmeans_",year,".csv"))
nn_saved<-"ffnn10 16_8_lr_001(2017).bin"

#hard learning example
training_features<-c(
  paste0("environment.",year,"_land_distance_inv"),
  paste0("environment.",year,"_mean_depth_inv"),
  paste0("environment.",year,"_net_primary_production"),
  paste0("environment.",year,"_sea.bottom_dissolved_oxygen"),
  paste0("fishing.activity.",year,"_total_fishing"),
  paste0("species.richness.",year),
  paste0("stocks.richness.",year),
  paste0("thermohalinity_",year )
  )

target_features<-c("hotspot")

#rule of thumb: start from 2*n+1   qui sopra è 17 perchè 8 variabili 
hidden_neurons_per_layer<-c(16,8)

cat("Training with hidden neurons =",paste0(hidden_neurons_per_layer,collapse = ","),"\n")

data_training_features<-(subset(data, select = c(training_features,target_features) ))

dependency_target_vs_training<-paste(paste0(target_features,collapse = "+"), "~", paste(training_features, collapse = " + ") )

cat("Simulation setup =",dependency_target_vs_training,"\n")

f <- as.formula(dependency_target_vs_training)

### Artificial Neural Network Parameters ###
# rp - number of repetitions for the training
rp=10
# thld - threshold for minimum decrease in overall error, default 0.01 = 1%
thld=0.01     #lavora su questo per minimizzare il fatto di trovare minimi locali, prova quindi ad aumentarlo
# stp - the maximum steps for the training of the neural network, default 1e+06
stp=1e+06
# alg - possible: backprop, rprop+, rprop-, sag, or slr
alg ="backprop"
# act.fct - possible: "logistic" (=sigmoid) or "tanh"; linear.output must be 
act.fct ="logistic"
#learning rate
learningrate=0.02



#train the ANN with the hidden neurons
if(training){
nn <- neuralnet(f,
                data = data_training_features,
                hidden = hidden_neurons_per_layer,
                threshold = thld,
                stepmax = stp,
                rep = rp,
                learningrate = learningrate,
                act.fct = act.fct,
                linear.output = FALSE, #activation function will be present also on the output nodes
                lifesign = "minimal",
                algorithm = alg)

}else{
  load(file = nn_saved)
}
# Compute predictions on the training data
data_selftest_features<-(subset(data, select = c(training_features) ))
prediction_self<- compute(nn, data_selftest_features)

data_selftest_output<-cbind(data_selftest_features,prediction_self$net.result)
names(data_selftest_output)<-c(training_features,target_features)


#####Accuracy calculation for one-output ANN
#test multiple decision thresholds based on the values over the training set
if(training){
decision_thresholds<-as.numeric(quantile(prediction_self$net.result,probs=c(0.01,0.05,0.1,0.2,0.5,0.75)))
optimal_accuracy<-0
optimal_threshold<-0
}else{
#thresholds with only 2017 optimal value
  optimal_accuracy<-0
decision_thresholds<-c(optimal_threshold)}
for (decision_threshold in decision_thresholds){
  
  data_selftest_output$classification<-0
  data_selftest_output$classification[which(data_selftest_output[target_features]>=decision_threshold)]<-1
  
  accuracy<-1-(sum(abs(
    data_training_features[target_features]-data_selftest_output$classification)
    )/length(data_selftest_output$classification))
  
  cat("Accuracy with threshold",decision_threshold,"=",accuracy,"\n")
  if (accuracy>optimal_accuracy){
    optimal_accuracy<-accuracy
    optimal_threshold<-decision_threshold
  }
}

cat("\nOptimal accuracy=",optimal_accuracy,", reached with threshold=",optimal_threshold,"\n")
data_selftest_output$classification<-0
data_selftest_output$classification[which(data_selftest_output[target_features]>=optimal_threshold)]<-1
names(data_selftest_output)[names(data_selftest_output) == target_features] <- 'distance_class_interpretation'

#save the ANN for later use:
if(training){
  save(file = nn_saved,list = c("nn","optimal_threshold"))
}

data_selftest_output_lonlat <- cbind(data$longitude,data$latitude,data_selftest_output)
names(data_selftest_output_lonlat)[1]<-"longitude"
names(data_selftest_output_lonlat)[2]<-"latitude"
write.csv(data_selftest_output_lonlat,paste0("cluster_ANN_",year,".csv"),row.names = FALSE)


