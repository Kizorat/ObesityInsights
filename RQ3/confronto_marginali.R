if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

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
reale$origine <- "Reale"
# Aggiungo una colonna per distinguere le due origini, utile per i test statistici
vars_continue <- c("Age", "Height", "Weight", "BMI", "FCVC", "NCP", "CH2O", "FAF", "TUE")
vars_categoriche <- c("Gender", "family_history_with_overweight", "FAVC", "CAEC",
                       "SMOKE", "SCC", "CALC", "MTRANS", "target")

# Test di Kolmogorov-Smirnov a due campioni per ciascuna variabile continua
risultati_ks <- do.call(rbind, lapply(vars_continue, function(v) {
  test <- suppressWarnings(ks.test(reale[[v]], sintetico[[v]]))
  data.frame(
    variabile = v,
    media_reale = round(mean(reale[[v]]), 2),
    media_sintetico = round(mean(sintetico[[v]]), 2),
    sd_reale = round(sd(reale[[v]]), 2),
    sd_sintetico = round(sd(sintetico[[v]]), 2),
    statistica_D = round(unname(test$statistic), 3),
    p_value = test$p.value
  )
}))
cat("\nRQ3, confronto marginali, variabili continue, test di Kolmogorov-Smirnov\n")
print(risultati_ks)

# Chi-quadro o Fisher tra origine, reale contro sintetico, e ciascuna categorica, stessa logica di screen_categorical usata in analisi_descrittiva
risultati_cat <- do.call(rbind, lapply(vars_categoriche, function(v) {
  x <- c(as.character(reale[[v]]), as.character(sintetico[[v]]))
  origine <- c(rep("Reale", nrow(reale)), rep("Sintetico", nrow(sintetico)))
  res <- screen_categorical(x, origine)
  data.frame(variabile = v, metodo = res$metodo, p_value = res$p_value, cramer_v = round(res$cramer_v, 3))
}))
cat("\nRQ3, confronto marginali, variabili categoriche, differenza di distribuzione tra reale e sintetico\n")
print(risultati_cat)

# Un PNG per variabile continua
for (v in vars_continue) {
  salva_png(paste0("confronto_", v, ".png"), dir = dir_output_rq3,
            larghezza = 500, altezza = 420, disegna = function() {
    d_reale <- density(reale[[v]])
    d_sint <- density(sintetico[[v]])
    ylim <- range(c(d_reale$y, d_sint$y))
    xlim <- range(c(reale[[v]], sintetico[[v]]))
    par(mar = c(4, 4, 3, 1))
    plot(d_reale, col = "#2a78d6", lwd = 2, main = v, xlab = "", ylab = "Densita'",
         xlim = xlim, ylim = ylim)
    lines(d_sint, col = "#eb6834", lwd = 2)
    legend("topright", legend = c("Reale", "Sintetico"), col = c("#2a78d6", "#eb6834"),
           lwd = 2, bty = "n", cex = 0.8)
  })
}

# Un PNG per variabile categorica
for (v in vars_categoriche) {
  salva_png(paste0("confronto_", v, ".png"), dir = dir_output_rq3,
            larghezza = 500, altezza = 420, disegna = function() {
    p_reale <- prop.table(table(as.character(reale[[v]])))
    p_sint <- prop.table(table(as.character(sintetico[[v]])))
    livelli <- union(names(p_reale), names(p_sint))
    mat <- rbind(p_reale[livelli], p_sint[livelli])
    mat[is.na(mat)] <- 0
    rownames(mat) <- c("Reale", "Sintetico")
    par(mar = c(5, 4, 3, 1))
    barplot(mat, beside = TRUE, col = c("#2a78d6", "#eb6834"), border = "white",
            las = 2, cex.names = 0.85, main = v, ylab = "Proporzione")
    legend("topright", legend = c("Reale", "Sintetico"), fill = c("#2a78d6", "#eb6834"),
           bty = "n", cex = 0.8)
  })
}

save_processed(risultati_ks, "confronto_marginali_continue", dir = dir_data_rq3)
save_processed(risultati_cat, "confronto_marginali_categoriche", dir = dir_data_rq3)
cat("\nGrafici e tabelle salvati in RQ3/output/ e RQ3/data/\n")
