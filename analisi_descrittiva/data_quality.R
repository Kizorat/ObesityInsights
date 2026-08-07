if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

df_raw <- load_raw_data()

# Controllo di qualità sul dataset grezzo

cat("\nMissing values per colonna\n")
print(colSums(is.na(df_raw)))

cat("\nDuplicati esatti\n")
print(sum(duplicated(df_raw)))

cat("\nOutlier, regola IQR 1.5x, per variabile continua\n")
print(sapply(df_raw[, get_continuous_vars()], check_outliers_iqr))

# Percentuale di valori non interi: alta per una scala Likert (es. 1,2,3) è un segnale da controllare
cat("\nPercentuale di valori non interi\n")
print(sapply(df_raw[, get_continuous_vars()], pct_non_integer))

cat("\nCategorie rare, < 1% delle osservazioni\n")
for (v in get_categorical_vars()) {
  rare <- rare_categories(df_raw[[v]])
  if (length(rare) > 0) {
    cat("\n", v, "\n")
    print(rare)
  }
}

# Range di valori plausibili dichiarati per ciascuna variabile continua
domini_attesi <- list(
  Age = c(14, 61), Height = c(1.45, 1.98), Weight = c(39, 173),
  FCVC = c(1.0, 3.0), NCP = c(1.0, 4.0), CH2O = c(1.0, 3.0),
  FAF = c(0.0, 3.0), TUE = c(0.0, 2.0)
)
cat("\nViolazioni del dominio atteso\n")
print(sapply(names(domini_attesi), function(v) {
  rng <- domini_attesi[[v]]
  sum(df_raw[[v]] < rng[1] | df_raw[[v]] > rng[2])
}))

# Feature engineering: applico le trasformazioni definite in utils.R
df_fe <- engineer_features(df_raw)

# Verifico che l'arrotondamento abbia eliminato i valori non interi
cat("\nVerifica: percentuale di valori non interi dopo l'arrotondamento\n")
print(sapply(df_fe[, c("Age", "FCVC", "NCP", "CH2O", "FAF", "TUE")], pct_non_integer))

cat("\nStruttura del dataset dopo il feature engineering\n")
str(df_fe)

# Salvo il dataset pulito, cosi' gli script successivi lo ricaricano senza ripetere questi passaggi
save_processed(df_fe, "df_features")
