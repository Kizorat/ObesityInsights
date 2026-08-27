if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

# caret/pROC per la cross-validation, arm per il modello bayesiano
for (pkg in c("caret", "pROC", "arm")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(caret)
library(arm)

dir_output_rq1 <- here::here("RQ1", "output")

df <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)

screening <- tryCatch(
  load_processed("rq1_screening_univariato"),
  error = function(e) stop("Esegui prima screening_univariato.R")
)

SOGLIA_SCREENING <- 0.05
predittori_selezionati <- screening$variabile[screening$p_value < SOGLIA_SCREENING]

# Validazione: AUC-ROC cross-validata a 10 fold. Uso target invece di target_num perchè caret con classProbs = TRUE lavora con le etichette delle classi, non con 0/1
f_target <- as.formula(paste("target ~", paste(predittori_selezionati, collapse = " + ")))

set.seed(123)
# savePredictions = "final" conserva le probabilità previste fuori campione per ogni fold, necessarie per disegnare la curva ROC
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE,
                      summaryFunction = twoClassSummary,
                      savePredictions = "final")
cv_model <- train(f_target, data = df, method = "glm", family = "binomial",
                   trControl = ctrl, metric = "ROC")

cat("\nRQ1 — Validazione: AUC-ROC cross-validata, 10 fold\n")
print(cv_model$results)

# Curva ROC calcolata sulle previsioni fuori campione, non sui dati di training: é la stessa AUC riportata sopra. Classe di interesse: Yes, obesità severa
pred_cv <- cv_model$pred[order(cv_model$pred$rowIndex), ]
roc_obj <- pROC::roc(pred_cv$obs, pred_cv$Yes, levels = c("No", "Yes"),
                      direction = "<", quiet = TRUE)

# Matrice di confusione e metriche di classificazione sulla stessa previsione out-of-fold usata per la curva ROC, classe positiva target = Yes, soglia 0.3 invece di 0.5 perchè 0.5 é troppo conservativa su un target sbilanciato 85.9%/14.1% e penalizza fortemente la Recall
SOGLIA_CLASSIFICAZIONE <- 0.3
pred_soglia <- factor(ifelse(pred_cv$Yes >= SOGLIA_CLASSIFICAZIONE, "Yes", "No"), levels = c("No", "Yes"))
matrice_confusione <- table(Predetto = pred_soglia, Osservato = factor(pred_cv$obs, levels = c("No", "Yes")))
cat("\nRQ1 — Matrice di confusione, soglia 0.3, out-of-fold\n")
print(matrice_confusione)

TP <- matrice_confusione["Yes", "Yes"]
FP <- matrice_confusione["Yes", "No"]
FN <- matrice_confusione["No", "Yes"]
TN <- matrice_confusione["No", "No"]

accuracy  <- (TP + TN) / sum(matrice_confusione)
precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1        <- 2 * precision * recall / (precision + recall)

metriche <- data.frame(
  metrica = c("Accuracy", "Precision", "Recall", "F1"),
  valore  = c(accuracy, precision, recall, f1)
)
cat("\nRQ1 — Metriche di classificazione, soglia 0.3\n")
print(metriche)

# Interpretazione: odds ratio aggiustati dal modello bayesiano, lo stesso modello corretto in bayesglm.R con prior Cauchy che stabilizza le categorie con poche osservazioni, es. MTRANS Motorbike, Yes codificato come evento essendo la classe di interesse clinico
df$target_num <- as.numeric(factor(df$target, levels = c("No", "Yes"))) - 1
f_multi <- as.formula(paste("target_num ~", paste(predittori_selezionati, collapse = " + ")))
m_bayes <- arm::bayesglm(f_multi, data = df, family = binomial(link = "logit"),
                          prior.scale = 2.5, prior.df = 1)

# Costruisco la tabella degli odds ratio a mano, IC di Wald stima +/- 1.96*SE sulla scala log-odds poi esponenziati, invece di broom::tidy() con conf.int = TRUE che per bayesglm chiama confint.glm() -> profile.glm() e fallisce sul fitting penalizzato
co <- summary(m_bayes)$coefficients
z975 <- qnorm(0.975)
or_df <- data.frame(
  term = rownames(co),
  estimate = exp(co[, "Estimate"]),
  conf.low = exp(co[, "Estimate"] - z975 * co[, "Std. Error"]),
  conf.high = exp(co[, "Estimate"] + z975 * co[, "Std. Error"]),
  p.value = co[, "Pr(>|z|)"],
  row.names = NULL
)
or_df <- or_df[or_df$term != "(Intercept)", ]
or_df <- or_df[order(or_df$estimate), ]

cat("\nRQ1 — Odds ratio aggiustati, modello bayesiano, IC 95%\n")
print(or_df[, c("term", "estimate", "conf.low", "conf.high", "p.value")])

# Etichette "Variabile: livello", per restare ancorati ai nomi di colonna usati in tutto il resto del progetto invece di parafrasare in una descrizione libera
etichette_termini <- c(
  GenderMale = "Gender: Male",
  family_history_with_overweightyes = "family_history_with_overweight: yes",
  Age = "Age",
  CAECFrequently = "CAEC: Frequently",
  CAECno = "CAEC: no",
  CAECSometimes = "CAEC: Sometimes",
  FAVCyes = "FAVC: yes",
  TUE = "TUE",
  MTRANSBike = "MTRANS: Bike",
  MTRANSMotorbike = "MTRANS: Motorbike",
  MTRANSPublic_Transportation = "MTRANS: Public_Transportation",
  MTRANSWalking = "MTRANS: Walking",
  SCCyes = "SCC: yes",
  CH2O = "CH2O",
  CALCno = "CALC: no",
  CALCSometimes = "CALC: Sometimes",
  SMOKEyes = "SMOKE: yes",
  NCP = "NCP"
)
or_df$etichetta <- unname(etichette_termini[or_df$term])

# Categorizzo ogni predittore in base a dove cade il suo IC 95% rispetto a 1: sopra = fattore di rischio, sotto = fattore protettivo, se include 1 l'effetto non è conclusivo
or_df$direzione <- ifelse(or_df$conf.low > 1, "Fattore di rischio",
                    ifelse(or_df$conf.high < 1, "Fattore protettivo",
                           "Non conclusivo"))
or_df$direzione <- factor(or_df$direzione,
                           levels = c("Fattore protettivo", "Non conclusivo", "Fattore di rischio"))

# Fisso l'ordine dei termini nel grafico 
or_df$etichetta <- factor(or_df$etichetta, levels = or_df$etichetta)

# Grafico 1: curva ROC cross-validata
salva_png("roc_curve.png", dir = dir_output_rq1,
          larghezza = 700, altezza = 700, disegna = function() {
  pROC::plot.roc(roc_obj, col = "#2a78d6", lwd = 3, print.auc = TRUE,
                  print.auc.col = "#2a78d6", print.auc.cex = 1.2,
                  legacy.axes = TRUE,
                  xlab = "1 - Specificita'", ylab = "Sensibilita'",
                  main = "RQ1 — Curva ROC cross-validata")
  abline(0, 1, col = "#b0b0b0", lty = 2)
})

# Grafico 2: matrice di confusione, classe positiva Yes. Griglia 2x2 disegnata a mano: righe = classe predetta, colonne = classe osservata, blu per le predizioni corrette TN/TP, rosso per gli errori FP/FN
salva_png("matrice_confusione.png", dir = dir_output_rq1,
          larghezza = 650, altezza = 600, disegna = function() {
  conteggi   <- matrix(c(TN, FN, FP, TP), nrow = 2, byrow = TRUE)
  etichette  <- matrix(c("Veri Negativi", "Falsi Negativi",
                          "Falsi Positivi", "Veri Positivi"),
                        nrow = 2, byrow = TRUE)
  corretto   <- matrix(c(TRUE, FALSE, FALSE, TRUE), nrow = 2, byrow = TRUE)
  colori     <- ifelse(corretto, "#2a78d6", "#e34948")

  par(mar = c(5, 7, 4, 2))
  plot(NA, xlim = c(0, 2), ylim = c(0, 2), asp = 1, axes = FALSE,
       xlab = "Osservato", ylab = "Predetto",
       main = "RQ1 — Matrice di confusione")
  for (r in 1:2) for (co in 1:2) {
    rect(co - 1, 2 - r, co, 2 - r + 1, col = colori[r, co], border = "white", lwd = 3)
    text(co - 0.5, 2 - r + 0.6, etichette[r, co], cex = 0.85, col = "white", font = 2)
    text(co - 0.5, 2 - r + 0.35, conteggi[r, co], cex = 1.7, col = "white", font = 2)
  }
  axis(1, at = c(0.5, 1.5), labels = c("No", "Yes"), tick = FALSE, cex.axis = 1.1)
  axis(2, at = c(1.5, 0.5), labels = c("No", "Yes"), tick = FALSE, las = 1, cex.axis = 1.1)
})

# Grafico 3: metriche di classificazione alla soglia 0.3
salva_png("metriche_classificazione.png", dir = dir_output_rq1,
          larghezza = 650, altezza = 600, disegna = function() {
  par(mar = c(5, 5, 4, 2))
  bp <- barplot(metriche$valore, names.arg = metriche$metrica, col = "#2a78d6",
                border = "white", ylim = c(0, 1),
                ylab = "Valore", cex.names = 1.05,
                main = "RQ1 — Metriche di classificazione")
  text(bp, metriche$valore, labels = sprintf("%.3f", metriche$valore), pos = 3, cex = 1)
})

# Grafico 4: odds ratio come barre divergenti "a farfalla", diverso dal classico forest plot punto+baffi: ogni barra parte da OR=1 e si estende verso l'OR stimato in scala log10, cosi' le barre dividono naturalmente i fattori protettivi, a sinistra, da quelli di rischio, a destra; i baffi sopra ogni barra restano l'intervallo di confidenza
salva_png("forest_plot.png", dir = dir_output_rq1,
          larghezza = 950, altezza = 700, disegna = function() {
  ord <- order(or_df$estimate)
  stime  <- log10(or_df$estimate[ord])
  low    <- log10(or_df$conf.low[ord])
  high   <- log10(or_df$conf.high[ord])
  nomi   <- as.character(or_df$etichetta[ord])
  palette_direzione <- c("Fattore protettivo" = "#2a78d6",
                          "Non conclusivo" = "#b0b0b0",
                          "Fattore di rischio"  = "#e34948")
  colori <- palette_direzione[as.character(or_df$direzione[ord])]

  par(mar = c(5, 15, 4, 2))
  bp <- barplot(stime, horiz = TRUE, names.arg = nomi, col = colori,
                border = "white", las = 1, cex.names = 0.85,
                xlim = range(c(low, high)) + c(-0.3, 0.3),
                xaxt = "n",
                xlab = "Odds Ratio",
                main = "RQ1 — Odds ratio aggiustati")
  # Baffi dell'intervallo di confidenza sopra ogni barra
  segments(low, bp, high, bp, col = "grey30", lwd = 1.5)
  segments(low, bp - 0.2, low, bp + 0.2, col = "grey30", lwd = 1.5)
  segments(high, bp - 0.2, high, bp + 0.2, col = "grey30", lwd = 1.5)
  # Asse X con i valori originali dell'odds ratio, non il log10 grezzo
  assi_or <- c(0.01, 0.1, 1, 10, 100, 1000)
  axis(1, at = log10(assi_or), labels = assi_or)
  abline(v = 0, col = "grey20", lty = 2)
  legend("bottomright", legend = names(palette_direzione),
         fill = palette_direzione, border = "white", bty = "n", cex = 0.85)
})

save_processed(or_df, "rq1_odds_ratio")
cat("\nGrafici salvati in RQ1/output/\n")
