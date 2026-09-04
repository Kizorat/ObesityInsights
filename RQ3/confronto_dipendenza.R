if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot", repos = "https://cloud.r-project.org")
}
library(corrplot)

dir_output_rq3 <- here::here("RQ3", "output")
dir_data_rq3 <- here::here("RQ3", "data")
# Carico i dataset reale e sintetico, con le features ingegnerizzate
sintetico <- tryCatch(
  load_processed("sintetico_features", dir = dir_data_rq3),
  error = function(e) stop("Esegui prima preparazione_sintetico.R")
)
reale <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)

# Cramer's V di Gender-target e family_history-target, reale contro sintetico, sono le due dipendenze più forti individuate in analisi_descrittiva
tab_gender_reale <- table(reale$Gender, reale$target)
tab_gender_sint <- table(sintetico$Gender, sintetico$target)
tab_fh_reale <- table(reale$family_history_with_overweight, reale$target)
tab_fh_sint <- table(sintetico$family_history_with_overweight, sintetico$target)

dipendenze <- data.frame(
  relazione = c("Gender - target", "family_history - target"),
  cramer_v_reale = c(cramer_v(tab_gender_reale), cramer_v(tab_fh_reale)),
  cramer_v_sintetico = c(cramer_v(tab_gender_sint), cramer_v(tab_fh_sint))
)
dipendenze$cramer_v_reale <- round(dipendenze$cramer_v_reale, 3)
dipendenze$cramer_v_sintetico <- round(dipendenze$cramer_v_sintetico, 3)
cat("\nRQ3, forza delle dipendenze più rilevanti, reale contro sintetico\n")
print(dipendenze)

cat("\nRQ3, tabella Gender per target, reale\n")
print(tab_gender_reale)
cat("\nRQ3, tabella Gender per target, sintetico\n")
print(tab_gender_sint)

# Matrice di correlazione di Pearson tra le variabili continue, reale contro sintetico, e differenza assoluta cella per cella
vars_continue <- c("Age", "Height", "Weight", "BMI", "FCVC", "NCP", "CH2O", "FAF", "TUE")
corr_reale <- cor(reale[, vars_continue])
corr_sint <- cor(sintetico[, vars_continue])
corr_diff <- abs(corr_reale - corr_sint)

cat("\nRQ3, differenza assoluta media tra le due matrici di correlazione:",
    round(mean(corr_diff[upper.tri(corr_diff)]), 3), "\n")
diff_upper <- corr_diff
diff_upper[!upper.tri(diff_upper)] <- NA
indice_max <- which(diff_upper == max(diff_upper, na.rm = TRUE), arr.ind = TRUE)[1, ]
cat("Coppia con la differenza più grande:", rownames(corr_diff)[indice_max[1]],
    "-", colnames(corr_diff)[indice_max[2]],
    ", differenza", round(max(diff_upper, na.rm = TRUE), 3), "\n")

# Normalità delle variabili continue nel sintetico, Shapiro-Wilk
normalita_sintetico <- do.call(rbind, lapply(vars_continue, function(v) {
  test <- shapiro.test(sintetico[[v]])
  data.frame(variabile = v, statistica_W = round(unname(test$statistic), 3), p_value = test$p.value)
}))
cat("\nRQ3, test di Shapiro-Wilk sulle variabili continue del sintetico\n")
print(normalita_sintetico)

salva_png("confronto_correlazioni.png", dir = dir_output_rq3,
          larghezza = 1100, altezza = 550, disegna = function() {
  par(mfrow = c(1, 2))
  corrplot(corr_reale, method = "color", type = "upper", addCoef.col = "black",
           number.cex = 0.6, tl.cex = 0.8, tl.col = "black", mar = c(0, 0, 2, 0))
  title("Reale", line = -1)
  corrplot(corr_sint, method = "color", type = "upper", addCoef.col = "black",
           number.cex = 0.6, tl.cex = 0.8, tl.col = "black", mar = c(0, 0, 2, 0))
  title("Sintetico", line = -1)
})

salva_png("confronto_dipendenze_target.png", dir = dir_output_rq3,
          larghezza = 700, altezza = 600, disegna = function() {
  mat <- t(as.matrix(dipendenze[, c("cramer_v_reale", "cramer_v_sintetico")]))
  colnames(mat) <- c("Gender", "family_history")
  rownames(mat) <- c("Reale", "Sintetico")
  par(mar = c(5, 5, 4, 2))
  barplot(mat, beside = TRUE, col = c("#2a78d6", "#eb6834"), border = "white",
          ylab = "V di Cramer", main = "Forza della dipendenza con il target")
  legend("topright", legend = rownames(mat), fill = c("#2a78d6", "#eb6834"), bty = "n")
})

salva_png("qqplot_sintetico.png", dir = dir_output_rq3,
          larghezza = 900, altezza = 350, disegna = function() {
  par(mfrow = c(1, 3))
  for (v in c("Age", "Height", "Weight")) {
    qqnorm(sintetico[[v]], main = v, pch = 19, cex = 0.5, col = "#eb6834")
    qqline(sintetico[[v]], col = "#2a78d6", lwd = 2)
  }
})

save_processed(dipendenze, "confronto_dipendenze", dir = dir_data_rq3)
save_processed(normalita_sintetico, "normalita_sintetico", dir = dir_data_rq3)
cat("\nGrafici e tabelle salvati in RQ3/output/ e RQ3/data/\n")
