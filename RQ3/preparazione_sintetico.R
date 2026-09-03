if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

dir_output_rq3 <- here::here("RQ3", "output")
dir_data_rq3 <- here::here("RQ3", "data")

# Carico il dataset sintetico generato da SDV, e il dataset reale, per confrontarli
sintetico_grezzo <- readr::read_csv(here::here("dataset_sintetico", "dataset_sintetico.csv"), show_col_types = FALSE)
reale_grezzo <- load_raw_data()

cat("\nRQ3, dataset sintetico caricato:", nrow(sintetico_grezzo), "righe,", ncol(sintetico_grezzo), "colonne\n")
cat("Stesse colonne del reale:", identical(names(sintetico_grezzo), names(reale_grezzo)), "\n")

cat("\nRQ3, controllo qualita' del sintetico\n")
cat("Valori mancanti:", sum(is.na(sintetico_grezzo)), "\n")
cat("Righe duplicate:", sum(duplicated(sintetico_grezzo)), "\n")
cat("Righe duplicate rispetto al reale:", sum(duplicated(rbind(reale_grezzo, sintetico_grezzo)))-sum(duplicated(reale_grezzo)), "\n")

# Verifica che i valori categorici generati stiano nei domini attesi, senza categorie inventate dal modello
domini_attesi <- list(
  Gender = c("Female", "Male"),
  family_history_with_overweight = c("yes", "no"),
  FAVC = c("yes", "no"),
  CAEC = c("no", "Sometimes", "Frequently", "Always"),
  SMOKE = c("yes", "no"),
  SCC = c("yes", "no"),
  CALC = c("no", "Sometimes", "Frequently", "Always"),
  MTRANS = c("Automobile", "Motorbike", "Bike", "Public_Transportation", "Walking")
)
# Controllo che le categorie generate siano tutte nei domini attesi, e stampo i range di valori per le variabili continue
for (v in names(domini_attesi)) {
  fuori_dominio <- setdiff(unique(sintetico_grezzo[[v]]), domini_attesi[[v]])
  if (length(fuori_dominio) > 0) {
    cat("Categorie fuori dominio in", v, ":", paste(fuori_dominio, collapse = ", "), "\n")
  }
}
# Stampo i range di valori per le variabili continue, sintetico contro reale
cat("Range Age sintetico:", paste(range(sintetico_grezzo$Age), collapse = " - "),
    ", reale:", paste(range(reale_grezzo$Age), collapse = " - "), "\n")
cat("Range Height sintetico:", paste(range(sintetico_grezzo$Height), collapse = " - "),
    ", reale:", paste(range(reale_grezzo$Height), collapse = " - "), "\n")
cat("Range Weight sintetico:", paste(range(sintetico_grezzo$Weight), collapse = " - "),
    ", reale:", paste(range(reale_grezzo$Weight), collapse = " - "), "\n")

cat("\nPrevalenza target sintetico:", round(100 * mean(sintetico_grezzo$target), 1), "%\n")
cat("Prevalenza target reale:", round(100 * mean(reale_grezzo$target), 1), "%\n")

# Stesso feature engineering del dataset reale, per garantire un confronto sulle stesse colonne derivate e la stessa codifica dei fattori
sintetico <- engineer_features(sintetico_grezzo)
sintetico$origine <- "Sintetico"

reale <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(reale_grezzo)
)
reale$origine <- "Reale"
# Aggiungo una colonna per distinguere le due origini, utile per i test statistici
save_processed(sintetico, "sintetico_features", dir = dir_data_rq3)
cat("\nDataset sintetico dopo feature engineering salvato in RQ3/data/\n")
