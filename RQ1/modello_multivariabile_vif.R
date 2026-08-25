if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

# car::vif() serve per il controllo di multicollinearità
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car", repos = "https://cloud.r-project.org")
}
library(car)

dir_output_rq1 <- here::here("RQ1", "output")

df <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)

# Yes codificato come evento (1): e' la classe di interesse clinico (obesità severa)
df$target_num <- as.numeric(factor(df$target, levels = c("No", "Yes"))) - 1

# Ricarico i risultati dello screening univariato
screening <- tryCatch(
  load_processed("rq1_screening_univariato"),
  error = function(e) stop("Esegui prima screening_univariato.R")
)

# Uso solo i predittori che avevano superato la soglia dello screening
SOGLIA_SCREENING <- 0.05
predittori_selezionati <- screening$variabile[screening$p_value < SOGLIA_SCREENING]

# Costruisco e stimo il modello logistico con tutti i predittori insieme
cat("\nRQ1 — Modello multivariabile\n")
f_multi <- as.formula(paste("target_num ~", paste(predittori_selezionati, collapse = " + ")))
print(f_multi)

m_glm <- glm(f_multi, data = df, family = binomial)
print(summary(m_glm))

# Controllo di multicollinearità: un VIF alto, di solito > 5, indica che un predittore è spiegato troppo dagli altri predittori del modello
cat("\nRQ1 — Controllo di multicollinearità, VIF soglia < 5\n")
vif_out <- vif(m_glm)
print(vif_out)

# vif() restituisce una matrice quando il modello ha predittori con più di un grado di libertà, altrimenti un vettore semplice
if (is.matrix(vif_out)) {
  vif_confrontabile <- vif_out[, "GVIF^(1/(2*Df))"]
} else {
  vif_confrontabile <- sqrt(vif_out)
}

# Grafico 1: VIF per predittore, confrontato con sqrt(5), la soglia equivalente a VIF = 5 su questa scala
salva_png("vif.png", dir = dir_output_rq1,
          larghezza = 950, altezza = 600, disegna = function() {
  ord <- order(vif_confrontabile)
  par(mar = c(5, 17, 4, 2))
  # xlim forzato oltre la soglia, altrimenti la linea di riferimento cadrebbe fuori dall'area visibile: qui i VIF sono tutti bassi
  barplot(vif_confrontabile[ord], horiz = TRUE, names.arg = names(vif_confrontabile)[ord],
          col = "#2a78d6", border = "white", las = 1, cex.names = 0.85,
          xlim = c(0, sqrt(5) * 1.15),
          xlab = "Indice di collinearità",
          main = "RQ1 — Controllo di multicollinearità")
  abline(v = sqrt(5), col = "#e34948", lty = 2, lwd = 2)
  text(x = sqrt(5), y = 0.5, labels = "soglia VIF = 5", col = "#e34948",
       pos = 2, cex = 0.8, xpd = TRUE)
})

# Grafico 2: standard error dei coefficienti, mostra a colpo d'occhio l'anomalia di MTRANS Motorbike: una barra molto più lunga delle altre segnala instabilità numerica per quasi-separazione
salva_png("coefficienti_se.png", dir = dir_output_rq1,
          larghezza = 1050, altezza = 700, disegna = function() {
  co <- summary(m_glm)$coefficients
  co <- co[rownames(co) != "(Intercept)", , drop = FALSE]
  ord <- order(co[, "Std. Error"])
  colori <- ifelse(co[ord, "Std. Error"] > 50, "#e34948", "#2a78d6")

  par(mar = c(5, 20, 4, 2))
  barplot(co[ord, "Std. Error"], horiz = TRUE, names.arg = rownames(co)[ord],
          col = colori, border = "white", las = 1, cex.names = 0.75,
          xlab = "Standard error del coefficiente",
          main = "RQ1 — Standard error dei coefficienti")
  mtext("Barra rossa = instabilità numerica per quasi-separazione",
        side = 3, line = 0.3, cex = 0.8)
})

cat("\nGrafici salvati in RQ1/output/\n")
