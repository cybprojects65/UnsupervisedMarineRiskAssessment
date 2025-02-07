rm(list=ls(all=TRUE))
# Carica la libreria necessaria
library(readr)

# Leggi il file CSV
data <- read.csv(file="./comparisons.csv", header=TRUE, sep=",")

# Lista degli anni presenti nei dati
years <- unique(data$year)

# Funzione per creare la matrice di confusione
create_confusion_matrix <- function(df, metric) {
  # Creiamo una matrice vuota
  methods <- unique(c(df$method1, df$method2))
  cm <- matrix(0, nrow = length(methods), ncol = length(methods),
               dimnames = list(methods, methods))
  
  # Popoliamo la matrice con i valori di accuracies o kappa
  for (i in 1:nrow(df)) {
    method1 <- df$method1[i]
    method2 <- df$method2[i]
    value <- df[[metric]][i]
    cm[method1, method2] <- value
    cm[method2, method1] <- value  # poiché la matrice è simmetrica
  }
  return(cm)
}

# Crea e salva le matrici di confusione in un file di testo
output_file <- "confusion_matrices.txt"
sink(output_file)

# Scrivi il titolo per ogni anno e metrica
for (year in years) {
  cat("Confusion Matrix per l'anno:", year, "\n")
  
  # Filtro i dati per l'anno corrente
  year_data <- subset(data, year == year)
  
  # Crea e scrive la matrice di confusione per l'accuracy
  accuracy_cm <- create_confusion_matrix(year_data, "accuracy")
  cat("\nMatrice di Confusione per l'Accuracy:\n")
  print(accuracy_cm)
  
  # Crea e scrive la matrice di confusione per il kappa
  kappa_cm <- create_confusion_matrix(year_data, "kappa")
  cat("\nMatrice di Confusione per il Kappa:\n")
  print(kappa_cm)
  
  cat("\n")
}

sink()  # Termina la scrittura sul file
cat("Le matrici di confusione sono state salvate nel file:", output_file, "\n")
