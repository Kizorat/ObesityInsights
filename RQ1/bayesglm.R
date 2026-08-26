if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

# Carico il dataset con le features ingegnerizzate, o lo ricreo se data_quality.R non è stato eseguito
if (!requireNamespace("arm", quietly = TRUE)) {
  install.packages("arm", repos = "https://cloud.r-project.org")
}
library(arm)

dir_output_rq1 <- here::here("RQ1", "output")

df <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)

# Yes codificato come evento (1): e' la classe di interesse clinico (obesità severa)
df$target_num <- as.numeric(factor(df$target, levels = c("No", "Yes"))) - 1

screening <- tryCatch(
  load_processed("rq1_screening_univariato"),
  error = function(e) stop("Esegui prima screening_univariato.R")
)

SOGLIA_SCREENING <- 0.05
predittori_selezionati <- screening$variabile[screening$p_value < SOGLIA_SCREENING]

f_multi <- as.formula(paste("target_num ~", paste(predittori_selezionati, collapse = " + ")))

cat("\nRQ1 — Correzione con regressione logistica bayesiana\n")
# prior.df = 1 rende il prior una Cauchy invece che una Normale: code più pesanti, penalizza meno i coefficienti realmente grandi come Gender ma corregge comunque quelli instabili come MTRANS Motorbike
m_bayes <- arm::bayesglm(f_multi, data = df, family = binomial(link = "logit"),
                          prior.scale = 2.5, prior.df = 1)
print(summary(m_bayes))

# Confronto diretto degli standard error di MTRANS tra il modello GLM standard e il modello bayesiano, per verificare che la correzione abbia davvero risolto il problema
cat("\nConfronto standard error: GLM standard vs bayesglm, MTRANS\n")
m_glm <- glm(f_multi, data = df, family = binomial)
se_glm <- summary(m_glm)$coefficients[, "Std. Error"]
se_bayes <- summary(m_bayes)$coefficients[, "Std. Error"]
termini_mtrans <- grep("^MTRANS", names(se_glm), value = TRUE)
tab_confronto <- data.frame(
  termine = termini_mtrans,
  se_glm = se_glm[termini_mtrans],
  se_bayes = se_bayes[termini_mtrans]
)
print(tab_confronto)

# Grafico: SE prima/dopo la correzione, in scala log altrimenti la barra di Motorbike nel GLM standard schiaccerebbe tutte le altre
salva_png("se_confronto.png", dir = dir_output_rq1,
          larghezza = 850, altezza = 650, disegna = function() {
  mat <- t(as.matrix(tab_confronto[, c("se_glm", "se_bayes")]))
  colnames(mat) <- gsub("^MTRANS", "", tab_confronto$termine)
  rownames(mat) <- c("GLM standard", "Bayesiano")

  par(mar = c(5, 5, 4, 2))
  barplot(mat, beside = TRUE, col = c("#e34948", "#2a78d6"), border = "white",
          log = "y", las = 1,
          ylab = "Standard error",
          xlab = "Mezzo di trasporto",
          main = "RQ1 — Effetto della correzione bayesiana sugli standard error")
  legend("topright", legend = rownames(mat), fill = c("#e34948", "#2a78d6"),
         border = "white", bty = "n")
})

# broom::tidy() serve per esportare i coefficienti in formato tabellare
if (!requireNamespace("broom", quietly = TRUE)) {
  install.packages("broom", repos = "https://cloud.r-project.org")
}
# Salvo i coefficienti del modello bayesiano per riusarli negli step successivi: odds ratio e forest plot
save_processed(broom::tidy(m_bayes), "rq1_bayesglm_coeff")
cat("\nGrafico salvato in RQ1/output/\n")
