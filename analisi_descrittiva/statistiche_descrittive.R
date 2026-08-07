if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

# Import e prima ispezione del dataset grezzo 
df_raw <- load_raw_data()

dim(df_raw) # numero di righe e colonne
str(df_raw) # tipo di ciascuna colonna
head(df_raw, 3) # prime 3 righe
dplyr::glimpse(df_raw) # anteprima compatta di tutte le colonne

# Ricarico il dataset e aggiungo il BMI, serve solo per i grafici di questo script
df <- load_raw_data()
df$BMI <- df$Weight / (df$Height^2)

# Istogramma per ogni variabile continua (piu' il BMI)
for (v in c(get_continuous_vars(), "BMI")) {
  salva_png(paste0("hist_", v, ".png"), disegna = function() {
    hist(df[[v]], main = paste("Distribuzione di", v), xlab = v,
         col = "#2a78d6", border = "white")
  })
}

# Grafico di frequenza per ogni variabile categorica: pie chart se ha meno di 4 categorie, barplot altrimenti
for (v in get_categorical_predictors()) {
  prefisso <- if (length(unique(na.omit(df[[v]]))) >= 4) "bar_" else "pie_"
  salva_png(paste0(prefisso, v, ".png"), disegna = function() {
    plot_categorical(df[[v]], main = paste("Frequenze di", v))
  })
}


# Statistiche descrittive
cat("\nStatistiche descrittive — variabili continue\n")
print(describe_continuous(df))

# Frequenze per le variabili categoriche
cat("\nFrequenze — variabili categoriche\n")
for (v in get_categorical_vars()) {
  cat("\n", v, "\n")
  print(table(df[[v]]))
  print(round(prop.table(table(df[[v]])) * 100, 1))
}

# Bilanciamento del target
cat("\nBilanciamento del target\n")
print(table(df$target))
print(round(prop.table(table(df$target)) * 100, 1))


# Screening bivariato esplorativo su dati ancora grezzi: confronta ogni variabile continua tra i due gruppi target No/Yes
cat("\nScreening bivariato — continue vs target\n")
screening_cont <- do.call(rbind, lapply(get_continuous_vars(), function(v) {
  res <- compare_continuous_by_group(df[[v]], df$target)
  cbind(variabile = v, res)
}))
print(screening_cont[order(screening_cont$wilcoxon_p), ])


# Testa l'associazione di ogni variabile categorica con il target
cat("\nScreening bivariato — categoriche vs target\n")
screening_cat <- do.call(rbind, lapply(get_categorical_predictors(), function(v) {
  res <- screen_categorical(df[[v]], df$target)
  cbind(variabile = v, res)
}))
print(screening_cat[order(-screening_cat$cramer_v), ])
