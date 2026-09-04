if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

if (!requireNamespace("pROC", quietly = TRUE)) {
  install.packages("pROC", repos = "https://cloud.r-project.org")
}
library(pROC)
# Carico i dataset reale e sintetico, con le features ingegnerizzate, per la validazione cross-domain
dir_output_rq3 <- here::here("RQ3", "output")
dir_data_rq3 <- here::here("RQ3", "data")

sintetico <- tryCatch(
  load_processed("sintetico_features", dir = dir_data_rq3),
  error = function(e) stop("Esegui prima preparazione_sintetico.R")
)
reale <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)

# Stessi 11 predittori selezionati dallo screening univariato di RQ1, per usare esattamente lo stesso modello
screening <- tryCatch(
  load_processed("rq1_screening_univariato"),
  error = function(e) stop("Esegui prima RQ1/screening_univariato.R")
)
predittori <- screening$variabile[screening$p_value < 0.05]
cat("\nRQ3, predittori usati per la validazione cross-domain, stessi di RQ1\n")
print(predittori)

f_multi <- as.formula(paste("target ~", paste(predittori, collapse = " + ")))

reale$target <- factor(reale$target, levels = c("No", "Yes"))
sintetico$target <- factor(sintetico$target, levels = c("No", "Yes"))

# Modello addestrato sul reale, valutato sul sintetico
m_reale <- glm(f_multi, data = reale, family = binomial)
prob_su_sintetico <- predict(m_reale, newdata = sintetico, type = "response")
roc_reale_su_sintetico <- pROC::roc(sintetico$target, prob_su_sintetico, levels = c("No", "Yes"),
                                     direction = "<", quiet = TRUE)
cat("\nRQ3, modello addestrato sul reale, AUC sul sintetico:", round(pROC::auc(roc_reale_su_sintetico), 3), "\n")

# Modello addestrato sul sintetico, valutato sul reale
m_sintetico <- glm(f_multi, data = sintetico, family = binomial)
prob_su_reale <- predict(m_sintetico, newdata = reale, type = "response")
roc_sintetico_su_reale <- pROC::roc(reale$target, prob_su_reale, levels = c("No", "Yes"),
                                     direction = "<", quiet = TRUE)
cat("RQ3, modello addestrato sul sintetico, AUC sul reale:", round(pROC::auc(roc_sintetico_su_reale), 3), "\n")

# Riferimento: AUC del modello RQ1 stimato e valutato entrambi sul reale, gia' calcolata in RQ1 con cross-validation a 10 fold
cat("\nRiferimento, AUC del modello RQ1 stimato e valutato sul reale, cross-validato: 0.928\n")

# Scomposizione dell'AUC cross-domain per capire quanta parte dipende dai predittori la cui relazione col target era gia' dichiarata nel prompt
auc_cross <- function(predittori_sub) {
  f <- as.formula(paste("target ~", paste(predittori_sub, collapse = " + ")))
  m <- glm(f, data = reale, family = binomial)
  p <- predict(m, newdata = sintetico, type = "response")
  as.numeric(pROC::auc(pROC::roc(sintetico$target, p, levels = c("No", "Yes"),
                                 direction = "<", quiet = TRUE)))
}
scomposizione <- data.frame(
  modello = c("Tutti gli 11 predittori", "Solo Gender", "Gender piu' Age", "Tutti tranne Gender"),
  auc = round(c(auc_cross(predittori), auc_cross("Gender"),
                auc_cross(c("Gender", "Age")), auc_cross(setdiff(predittori, "Gender"))), 3)
)
cat("\nRQ3, scomposizione dell'AUC reale su sintetico\n")
print(scomposizione)
save_processed(scomposizione, "rq3_scomposizione_auc", dir = dir_data_rq3)

salva_png("roc_cross_domain.png", dir = dir_output_rq3,
          larghezza = 700, altezza = 700, disegna = function() {
  pROC::plot.roc(roc_reale_su_sintetico, col = "#2a78d6", lwd = 3,
                  legacy.axes = TRUE, xlab = "1 - Specificita'", ylab = "Sensibilita'",
                  main = "Validazione cross-domain")
  pROC::lines.roc(roc_sintetico_su_reale, col = "#eb6834", lwd = 3)
  abline(0, 1, col = "#b0b0b0", lty = 2)
  legend("bottomright",
         legend = c(sprintf("Reale su sintetico, AUC = %.3f", pROC::auc(roc_reale_su_sintetico)),
                    sprintf("Sintetico su reale, AUC = %.3f", pROC::auc(roc_sintetico_su_reale))),
         col = c("#2a78d6", "#eb6834"), lwd = 3, bty = "n", cex = 0.85)
})

cat("\nGrafico salvato in RQ3/output/\n")
