if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

# Cartella locale per grafici e riepiloghi di RQ1, non nella output/ condivisa con analisi_descrittiva
dir_output_rq1 <- here::here("RQ1", "output")

# Carico il dataset pulito, o lo ricreo se data_quality.R non è stato ancora eseguito
df <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)

# Il target come 0/1 numerico per glm(family = binomial): codifico Yes come evento (1), essendo la classe di interesse clinico (obesità severa)
df$target_num <- as.numeric(factor(df$target, levels = c("No", "Yes"))) - 1

# Predittori candidati per RQ1: solo abitudini e demografia non modificabile, Weight/Height/BMI restano esclusi perchè ridondanti col target
predittori_rq1 <- c("Age", "Gender", "family_history_with_overweight", "FAVC",
                     "FCVC", "NCP", "CAEC", "SMOKE", "CH2O", "SCC", "FAF",
                     "TUE", "CALC", "MTRANS")

# Per ogni predittore stimo un modello logistico univariato e calcolo il p-value con il test del rapporto di verosimiglianza, più affidabile del test di Wald con categorie molto sbilanciate
cat("\nRQ1 — Screening univariato, test del rapporto di verosimiglianza\n")
screening <- do.call(rbind, lapply(predittori_rq1, function(v) {
  m <- glm(as.formula(paste("target_num ~", v)), data = df, family = binomial)
  data.frame(variabile = v, p_value = drop1(m, test = "Chisq")[v, "Pr(>Chi)"])
}))
# Ordino le variabili in base al p-value crescente
screening <- screening[order(screening$p_value), ]
print(screening)

# Seleziono le variabili con p < 0.05: sono quelle che passano al modello multivariabile
SOGLIA_SCREENING <- 0.05
selezionate <- screening$variabile[screening$p_value < SOGLIA_SCREENING]
cat("\nVariabili che superano p <", SOGLIA_SCREENING, ", passano al modello multivariabile:\n")
print(selezionate)

# Grafico: p-value dello screening, ordinati e colorati per esito, in scala -log10(p) così le variabili più significative hanno barre più lunghe
salva_png("screening_pvalue.png", dir = dir_output_rq1,
          larghezza = 1000, altezza = 650, disegna = function() {
  ord <- screening[order(screening$p_value), ]
  log_p <- -log10(ord$p_value)
  passa <- ord$p_value < SOGLIA_SCREENING
  colori <- ifelse(passa, "#2a78d6", "#b0b0b0")

  par(mar = c(5, 17, 4, 2))
  barplot(rev(log_p), horiz = TRUE, names.arg = rev(ord$variabile),
          col = rev(colori), border = "white", las = 1, cex.names = 0.8,
          xlab = "Forza dell'evidenza statistica",
          main = "RQ1 — Screening univariato: quali predittori passano?")
  abline(v = -log10(SOGLIA_SCREENING), col = "#e34948", lty = 2, lwd = 2)
  text(x = -log10(SOGLIA_SCREENING), y = 0.5, labels = "soglia p = 0.05",
       col = "#e34948", pos = 4, cex = 0.8, xpd = TRUE)
  legend("bottomright",
         legend = c("p < 0.05", "p >= 0.05"),
         fill = c("#2a78d6", "#b0b0b0"), border = "white", bty = "n", cex = 0.9)
})

# Salvo la tabella di screening, così gli script successivi la ricaricano senza rifare tutti i modelli univariati
save_processed(screening, "rq1_screening_univariato")
cat("\nTabella di screening salvata in data/processed/rq1_screening_univariato.csv\n")
cat("Grafico salvato in RQ1/output/\n")
