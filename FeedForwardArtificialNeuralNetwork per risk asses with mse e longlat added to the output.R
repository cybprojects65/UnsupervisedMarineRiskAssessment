#per l'addestramento settare l'anno 2017 e training (riga 7) a true
# per gli anni successuvi al 2017 cambiare l'anno con il replace (tranne nella dicitura in riga 99) e mettere training a F

library(neuralnet)
#setwd("C:/Users/UTENTE/Desktop/now/Reti per RISK ASSESSMENT")
anno<-"2017"
training<- T
data = read.csv("cluster_MultiKmeans_2017.csv")

#hard learning example
training_features<-c("environment.2017_land_distance_inv","environment.2017_mean_depth_inv","environment.2017_net_primary_production","environment.2017_sea.bottom_dissolved_oxygen","fishing.activity.2017_total_fishing","species.richness.2017","stocks.richness.2017","thermohalinity_2017")

target_features<-c("distance_class_interpretation")

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
  
  accuracy<-1-(sum(abs(data_training_features[target_features]-data_selftest_output$classification))/length(data_selftest_output$classification))
  
  cat("Accuracy with threshold",decision_threshold,"=",accuracy,"\n")
  if (accuracy>optimal_accuracy){
    optimal_accuracy<-accuracy
    optimal_threshold<-decision_threshold
  }
}

cat("\nOptimal accuracy=",optimal_accuracy,", reached with threshold=",optimal_threshold,"\n")
data_selftest_output$classification<-0
data_selftest_output$classification[which(data_selftest_output[target_features]>=optimal_threshold)]<-1


#save the ANN for later use:
save(file = "ffnn10 16_8_lr_001(2017).bin",list = c("nn"))
data_selftest_output_lonlat <- cbind(data$longitude,data$latitude,data_selftest_output)
names(data_selftest_output_lonlat)[1]<-"longitude"
names(data_selftest_output_lonlat)[2]<-"latitude"
write.csv(data_selftest_output_lonlat,"classification 2017 with ANN trained on 2017 data.csv",row.names = FALSE)



mse <- mean((data$distance_class_interpretation - prediction_self$net.result)^2)
cat(paste0("l'errore quadratico medio della predizione rispetto ai dati originali del cluster = ",mse,"\n"))


