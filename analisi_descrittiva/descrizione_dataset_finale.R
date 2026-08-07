if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

df <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)
# Statistiche descrittive e frequenze per il dataset finale, utile per la documentazione del progetto
cat("\nStatistiche descrittive — dataset finale, continue + BMI\n")
print(describe_continuous(df, vars = c(get_continuous_vars(), "BMI")))
# Frequenze per le variabili categoriche
cat("\nFrequenze — dataset finale, categoriche\n")
for (v in get_categorical_vars()) {
  cat("\n", v, "\n")
  print(table(df[[v]]))
  print(round(prop.table(table(df[[v]])) * 100, 1))
}
# Bilanciamento del target
cat("\nBilanciamento del target — dataset finale\n")
print(table(df$target))
print(round(prop.table(table(df$target)) * 100, 1))
