rm(list=ls(all=TRUE))

# Carica le librerie necessarie
library(readr)

# Leggi il file CSV
data <- read.csv(file="comparisons.csv", header=TRUE, sep=",")

# Lista degli anni presenti nei dati
years <- unique(data$year)

# Funzione per creare la matrice di confusione
create_confusion_matrix <- function(df, metric) {
  # Creiamo una matrice vuota
  methods <- unique(c(df$method1, df$method2))
  cm <- matrix(NA, nrow = length(methods), ncol = length(methods),
               dimnames = list(methods, methods))
  
  # Popoliamo la matrice con i valori di accuracy o kappa
  for (i in 1:nrow(df)) {
    method1 <- df$method1[i]
    method2 <- df$method2[i]
    value <- df[[metric]][i]
    cm[method1, method2] <- value
    cm[method2, method1] <- value  # Poiché la matrice è simmetrica
  }
  
  diag(cm) <- 100  # Opzionale: impostare i valori della diagonale a 100 per l'accuracy
  
  return(as.data.frame(cm))
}

# Creiamo un dataframe vuoto per raccogliere tutte le matrici
final_results <- data.frame()

# Creiamo e salviamo le matrici di confusione per ogni anno
for (year in years) {
  
  # Filtro i dati per l'anno corrente
  year_data <- subset(data, year == year)
  
  # Accuracy
  accuracy_cm <- create_confusion_matrix(year_data, "accuracy")
  accuracy_cm$metric <- "accuracy"
  accuracy_cm$year <- year
  accuracy_cm <- cbind(method = rownames(accuracy_cm), accuracy_cm)
  
  # Kappa
  kappa_cm <- create_confusion_matrix(year_data, "kappa")
  kappa_cm$metric <- "kappa"
  kappa_cm$year <- year
  kappa_cm <- cbind(method = rownames(kappa_cm), kappa_cm)
  
  # Aggiungiamo i risultati al dataframe finale
  final_results <- rbind(final_results, accuracy_cm, kappa_cm)
}

# Salva tutte le matrici in un unico file CSV
write.csv(final_results, "confusion_matrix.csv", row.names = FALSE)

cat("Le matrici di confusione sono state salvate in 'confusion_matrix.csv'\n")
