# Analisi di dipendenza tra le variabili e il target, sul dataset pulito
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

# Carico il dataset con le features ingegnerizzate, o lo ricreo se data_quality.R non e' stato eseguito
df <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)

# Cartella dove vengono salvate le figure generate da questo script
fig_dir <- here::here("output", "figures")

# Soglie minime per decidere se un'associazione e' abbastanza forte da meritare un grafico dedicato
SOGLIA_SPEARMAN <- 0.1
SOGLIA_CRAMER <- 0.1

# Correlazione tra le variabili continue
cont_vars <- df[, c(get_continuous_vars(), "BMI")]
cat("\nMatrice di correlazione, Pearson\n")
print(round(cor(cont_vars), 2))

# Salvo la heatmap della matrice di correlazione
salva_png("corr_heatmap.png", disegna = function() {
  plot_corr_heatmap(cor(cont_vars), main = "Correlazione fra variabili continue")
})

# Correlazione tra variabili continue e target: converto il target in 0/1 per poterlo correlare con le variabili numeriche
target_num <- as.numeric(factor(df$target, levels = c("No", "Yes"))) - 1
cat("\nCorrelazione con target — Pearson vs Spearman\n")
corr_target <- do.call(rbind, lapply(c(get_continuous_vars(), "BMI"), function(v) {
  data.frame(
    variabile = v,
    pearson  = round(cor(df[[v]], target_num, method = "pearson"), 3),
    spearman = round(cor(df[[v]], target_num, method = "spearman"), 3)
  )
}))
corr_target <- corr_target[order(-abs(corr_target$spearman)), ]
print(corr_target)

# Tengo solo le variabili continue con correlazione di Spearman, oltre la soglia e ne salvo il boxplot
vars_da_plottare_cont <- corr_target$variabile[abs(corr_target$spearman) >= SOGLIA_SPEARMAN]
cat("\nGrafico generato solo per |spearman| >=", SOGLIA_SPEARMAN, ":",
    paste(vars_da_plottare_cont, collapse = ", "), "\n")
for (v in vars_da_plottare_cont) {
  salva_png(paste0("assoc_", v, "_target.png"), disegna = function() {
    boxplot(df[[v]] ~ factor(df$target, levels = names(target_colors)),
            main = paste(v, "per classe target — associazione con il target"),
            xlab = "target", ylab = v, col = target_colors)
  })
}

# Associazione tra variabili categoriche e target
cat("\nCategoriche vs target — test + V di Cramer\n")
dipendenza_cat <- do.call(rbind, lapply(get_categorical_predictors(), function(v) {
  res <- screen_categorical(df[[v]], df$target)
  cbind(variabile = v, res)
}))
dipendenza_cat <- dipendenza_cat[order(-dipendenza_cat$cramer_v), ]
print(dipendenza_cat)

# Tengo solo le variabili categoriche con V di Cramer oltre la soglia e ne salvo il barplot delle proporzioni per target
vars_da_plottare_cat <- dipendenza_cat$variabile[dipendenza_cat$cramer_v >= SOGLIA_CRAMER]
cat("\nGrafico generato solo per V di Cramer >=", SOGLIA_CRAMER, ":",
    paste(vars_da_plottare_cat, collapse = ", "), "\n")

for (v in vars_da_plottare_cat) {
  salva_png(paste0("assoc_", v, "_target.png"), disegna = function() {
    plot_prop_by_target(df[[v]], df$target, main = paste("Proporzione di target per", v))
  })
}

# Approfondimenti sulle associazioni categoriche piu' forti: Gender, quasi tutti i casi target=Yes sono maschi
cat("\nGender vs target\n")
tab_gender <- table(df$Gender, df$target)
print(chisq.test(tab_gender))
cat("V di Cramer:", round(cramer_v(tab_gender), 3), "\n")

# family_history: uso Fisher perche' la tabella ha una cella con pochissime osservazioni, il chi-quadro classico non sarebbe affidabile
cat("\nfamily_history vs target\n")
tab_fh <- table(df$family_history_with_overweight, df$target)
print(fisher.test(tab_fh))
cat("V di Cramer:", round(cramer_v(tab_fh), 3), "\n")

# MTRANS: tabella 5x2 con alcune categorie molto rare, Bike e Motorbike, quindi uso Fisher con simulazione Monte Carlo invece del chi-quadro
cat("\nMTRANS vs target\n")
tab_mtrans <- table(df$MTRANS, df$target)
print(fisher.test(tab_mtrans, simulate.p.value = TRUE))
cat("V di Cramer:", round(cramer_v(tab_mtrans), 3), "\n")

cat("\nGrafici di associazione salvati in:", fig_dir, "\n")
