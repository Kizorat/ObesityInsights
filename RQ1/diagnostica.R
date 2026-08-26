if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

dir_output_rq1 <- here::here("RQ1", "output")

df <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)

# Yes codificato come evento (1): è la classe di interesse clinico (obesità severa)
df$target_num <- as.numeric(factor(df$target, levels = c("No", "Yes"))) - 1

screening <- tryCatch(
  load_processed("rq1_screening_univariato"),
  error = function(e) stop("Esegui prima screening_univariato.R")
)

SOGLIA_SCREENING <- 0.05
predittori_selezionati <- screening$variabile[screening$p_value < SOGLIA_SCREENING]
f_multi <- as.formula(paste("target_num ~", paste(predittori_selezionati, collapse = " + ")))

# Ristimo il modello GLM standard (la diagnostica va fatta su di lui)
m_glm <- glm(f_multi, data = df, family = binomial)

# Test di Hosmer-Lemeshow: confronta, in 10 gruppi ordinati per probabilità predetta, i casi osservati con quelli attesi; un p-value basso segnala un cattivo adattamento del modello
hosmer_lemeshow <- function(y, phat, g = 10) {
  ord <- order(phat)
  y <- y[ord]
  phat <- phat[ord]
  gr <- cut(seq_along(phat),
            breaks = quantile(seq_along(phat), probs = seq(0, 1, 1 / g)),
            include.lowest = TRUE, labels = FALSE)
  oss1 <- tapply(y, gr, sum)
  att1 <- tapply(phat, gr, sum)
  n_g <- tapply(y, gr, length)
  oss0 <- n_g - oss1
  att0 <- n_g - att1
  stat <- sum((oss1 - att1)^2 / att1 + (oss0 - att0)^2 / att0)
  list(statistic = stat, df = g - 2, p_value = 1 - pchisq(stat, g - 2))
}

cat("\nRQ1 — Test di Hosmer-Lemeshow, bonta' di adattamento\n")
hl <- hosmer_lemeshow(df$target_num, fitted(m_glm))
print(hl)

# Residui devianza: per ogni osservazione misurano quanto la previsione si discosta dal valore osservato; per convenzione |residuo| > 2 è considerato grande, soglia analoga a 2 deviazioni standard
cat("\nRQ1 — Osservazioni con residuo devianza piu' grande\n")
resid_dev <- residuals(m_glm, type = "deviance")
SOGLIA_RESIDUO <- 2
top5 <- order(abs(resid_dev), decreasing = TRUE)[1:5]
tab_influenti <- data.frame(
  riga = top5,
  residuo_devianza = round(resid_dev[top5], 3),
  Gender = as.character(df$Gender[top5]),
  Age = df$Age[top5],
  target = as.character(df$target[top5]),
  prob_predetta = signif(fitted(m_glm)[top5], 4)
)
print(tab_influenti)
cat(sprintf("\nOsservazioni con |residuo devianza| > %d: %d su %d, %.1f%%\n",
            SOGLIA_RESIDUO, sum(abs(resid_dev) > SOGLIA_RESIDUO), nrow(df),
            100 * mean(abs(resid_dev) > SOGLIA_RESIDUO)))

# Grafico 1: calibrazione, quello che il test di Hosmer-Lemeshow misura concretamente
salva_png("calibrazione.png", dir = dir_output_rq1,
          larghezza = 700, altezza = 650, disegna = function() {
  ord <- order(fitted(m_glm))
  y <- df$target_num[ord]
  phat <- fitted(m_glm)[ord]
  g <- 10
  gr <- cut(seq_along(phat),
            breaks = quantile(seq_along(phat), probs = seq(0, 1, 1 / g)),
            include.lowest = TRUE, labels = FALSE)
  oss <- tapply(y, gr, mean)
  att <- tapply(phat, gr, mean)

  plot(att, oss, pch = 19, col = "#2a78d6", cex = 1.6,
       xlim = c(0, 1), ylim = c(0, 1),
       xlab = "Probabilita' media attesa dal modello",
       ylab = "Proporzione osservata di target = Yes",
       main = "RQ1 — Calibrazione del modello")
  abline(0, 1, col = "#e34948", lty = 2, lwd = 2)
  legend("topleft", legend = c("Gruppi di rischio", "Calibrazione perfetta"),
         pch = c(19, NA), lty = c(NA, 2), col = c("#2a78d6", "#e34948"),
         bty = "n")
})

# Grafico 2: residui devianza per osservazione, con le 5 più grandi in valore assoluto evidenziate
salva_png("residui_devianza.png", dir = dir_output_rq1,
          larghezza = 850, altezza = 600, disegna = function() {
  colori_punti <- ifelse(abs(resid_dev) > SOGLIA_RESIDUO, "#e34948", "#b0b0b0")
  plot(resid_dev, pch = 19, cex = 0.6, col = colori_punti,
       xlab = "Osservazione",
       ylab = "Residuo devianza",
       main = "RQ1 — Residui devianza del modello GLM standard")
  abline(h = c(-SOGLIA_RESIDUO, 0, SOGLIA_RESIDUO),
         col = c("#e34948", "#666666", "#e34948"), lty = c(2, 1, 2))
  # Alterno le etichette sopra/sotto il punto per ridurre le sovrapposizioni tra osservazioni vicine
  posizioni <- rep(c(3, 1), length.out = length(top5))
  text(top5, resid_dev[top5],
       labels = paste0(df$Gender[top5], ", target=", df$target[top5]),
       pos = posizioni, cex = 0.7, col = "#e34948", xpd = TRUE)
  legend("bottomright", legend = c(sprintf("|residuo| > %d", SOGLIA_RESIDUO), "entro soglia"),
         pch = 19, col = c("#e34948", "#b0b0b0"), bty = "n", cex = 0.85)
})

# Dettaglio delle osservazioni con residuo più grande, per capire se il cattivo adattamento del modello è diffuso o concentrato in un sottogruppo specifico
cat("\nRQ1 — Dettaglio completo delle osservazioni con residuo più grande\n")
colonne_dettaglio <- c(predittori_selezionati, "target", "BMI")
dettaglio_top5 <- df[top5, colonne_dettaglio]
dettaglio_top5$residuo_devianza <- round(resid_dev[top5], 3)
print(dettaglio_top5)
fasce_eta <- cut(df$Age, breaks = c(0, 20, 25, 30, 40, 100),
                  labels = c("<20", "20-24", "25-29", "30-39", "40+"), right = FALSE)


riepilogo_sottogruppo <- function(gruppo) {
  do.call(rbind, lapply(split(seq_along(resid_dev), gruppo), function(idx) {
    data.frame(
      n = length(idx),
      pct_yes_osservato = round(100 * mean(df$target_num[idx]), 1),
      prob_media_predetta_yes = round(mean(fitted(m_glm)[idx]), 3),
      residuo_medio = round(mean(resid_dev[idx]), 3),
      pct_residuo_grande = round(100 * mean(abs(resid_dev[idx]) > SOGLIA_RESIDUO), 1)
    )
  }))
}

cat("\nRQ1 — Adattamento per sottogruppo: Gender\n")
print(riepilogo_sottogruppo(df$Gender))

cat("\nRQ1 — Adattamento per sottogruppo: MTRANS\n")
print(riepilogo_sottogruppo(df$MTRANS))

cat("\nRQ1 — Adattamento per sottogruppo: fascia d'eta'\n")
print(riepilogo_sottogruppo(fasce_eta))

# Grafico 3: residui devianza per Gender
salva_png("residui_per_gender.png", dir = dir_output_rq1,
          larghezza = 600, altezza = 550, disegna = function() {
  boxplot(resid_dev ~ df$Gender, col = c("#eb6834", "#2a78d6"), border = "grey30",
          xlab = "Gender", ylab = "Residuo devianza",
          main = "RQ1 — Residui devianza per Gender")
  abline(h = c(-SOGLIA_RESIDUO, 0, SOGLIA_RESIDUO),
         col = c("#e34948", "#666666", "#e34948"), lty = c(2, 1, 2))
})

# Grafico 4: residui devianza per MTRANS
salva_png("residui_per_mtrans.png", dir = dir_output_rq1,
          larghezza = 750, altezza = 550, disegna = function() {
  par(mar = c(8, 5, 4, 2))
  boxplot(resid_dev ~ df$MTRANS, col = "#2a78d6", border = "grey30", las = 2,
          xlab = "", ylab = "Residuo devianza",
          main = "RQ1 — Residui devianza per MTRANS")
  abline(h = c(-SOGLIA_RESIDUO, 0, SOGLIA_RESIDUO),
         col = c("#e34948", "#666666", "#e34948"), lty = c(2, 1, 2))
})

# Grafico 5: residui devianza per fascia d'età
salva_png("residui_per_eta.png", dir = dir_output_rq1,
          larghezza = 650, altezza = 550, disegna = function() {
  boxplot(resid_dev ~ fasce_eta, col = "#2a78d6", border = "grey30",
          xlab = "Fascia d'età", ylab = "Residuo devianza",
          main = "RQ1 — Residui devianza per fascia d'età")
  abline(h = c(-SOGLIA_RESIDUO, 0, SOGLIA_RESIDUO),
         col = c("#e34948", "#666666", "#e34948"), lty = c(2, 1, 2))
})

# Verifica della linearità nel logit per Age: il modello assume che ogni anno in piú sposti il log-odds della stessa quantità, assunzione che va controllata e non data per scontata
if (!requireNamespace("splines", quietly = TRUE)) {
  install.packages("splines", repos = "https://cloud.r-project.org")
}
library(splines)

# Test di Box-Tidwell: si aggiunge il termine x per log(x), se risulta significativo la relazione non è lineare sulla scala del logit
f_bt <- as.formula(paste("target_num ~", paste(predittori_selezionati, collapse = " + "),
                         "+ I(Age * log(Age))"))
m_bt <- glm(f_bt, data = df, family = binomial)
coef_bt <- summary(m_bt)$coefficients["I(Age * log(Age))", ]
cat("\nRQ1 — Test di Box-Tidwell sulla linearita' di Age nel logit\n")
print(round(coef_bt, 4))

# Confronto fra modello lineare in Age e modello con spline naturale a 3 gradi di libertà
predittori_senza_age <- setdiff(predittori_selezionati, "Age")
f_spline <- as.formula(paste("target_num ~", paste(predittori_senza_age, collapse = " + "),
                             "+ ns(Age, df = 3)"))
m_spline <- glm(f_spline, data = df, family = binomial)
confronto_spline <- anova(m_glm, m_spline, test = "LRT")
cat("\nRQ1 — Confronto fra Age lineare e Age con spline naturale\n")
print(confronto_spline)

linearita <- data.frame(
  test = c("Box-Tidwell", "LRT lineare contro spline"),
  statistica = round(c(coef_bt["z value"], confronto_spline$Deviance[2]), 3),
  p_value = signif(c(coef_bt["Pr(>|z|)"], confronto_spline$`Pr(>Chi)`[2]), 4)
)
save_processed(linearita, "rq1_linearita_logit")

# Andamento del logit stimato in funzione dell'età, tenendo gli altri predittori al loro valore di riferimento
salva_png("linearita_age.png", dir = dir_output_rq1,
          larghezza = 750, altezza = 600, disegna = function() {
  griglia <- data.frame(Age = seq(min(df$Age), max(df$Age), length.out = 200))
  for (v in predittori_senza_age) {
    griglia[[v]] <- if (is.numeric(df[[v]])) median(df[[v]]) else names(sort(table(df[[v]]), decreasing = TRUE))[1]
    if (is.factor(df[[v]])) griglia[[v]] <- factor(griglia[[v]], levels = levels(df[[v]]))
  }
  logit_lin <- predict(m_glm, newdata = griglia, type = "link")
  logit_spl <- predict(m_spline, newdata = griglia, type = "link")

  plot(griglia$Age, logit_spl, type = "l", lwd = 3, col = "#2a78d6",
       ylim = range(c(logit_lin, logit_spl)),
       xlab = "Età in anni", ylab = "Logit del rischio stimato",
       main = "RQ1 — Linearità di Age sulla scala del logit")
  lines(griglia$Age, logit_lin, lwd = 3, col = "#e34948", lty = 2)
  rug(df$Age, col = "#b0b0b0")
  abline(v = quantile(df$Age, 0.99), col = "#666666", lty = 3)
  text(quantile(df$Age, 0.99), min(logit_spl), "99% del campione", pos = 4, cex = 0.8, col = "#666666")
  legend("bottomright", legend = c("Spline naturale, 3 gradi di liberta'", "Termine lineare"),
         col = c("#2a78d6", "#e34948"), lwd = 3, lty = c(1, 2), bty = "n", cex = 0.9)
})

cat("\nGrafici salvati in RQ1/output/\n")
