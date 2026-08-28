if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

dir_output_rq2 <- here::here("RQ2", "output")
dir_data_rq2 <- here::here("RQ2", "data")

df <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)

# Solo le abitudini comportamentali, non i fattori demografici, qui si cercano profili di abitudini
vars_num <- df %>% dplyr::transmute(
  FAVC_n  = ifelse(FAVC == "yes", 1, 0),
  SMOKE_n = ifelse(SMOKE == "yes", 1, 0),
  SCC_n   = ifelse(SCC == "yes", 1, 0),
  # I livelli di CAEC e CALC vanno re-imposti perchè il CSV non conserva l'ordine del fattore
  CAEC_n  = as.numeric(factor(CAEC, levels = c("no", "Sometimes", "Frequently", "Always"))) - 1,
  CALC_n  = as.numeric(factor(CALC, levels = c("no", "Sometimes", "Frequently_o_Always"))) - 1,
  FCVC = FCVC, NCP = NCP, CH2O = CH2O, FAF = FAF, TUE = TUE
)

# MTRANS è nominale senza ordine naturale, quindi si codifica con one-hot invece di un intero
mtrans_dummy <- model.matrix(~ MTRANS - 1, data = df)
colnames(mtrans_dummy) <- sub("^MTRANS", "MTRANS_", colnames(mtrans_dummy))
vars_num <- cbind(vars_num, mtrans_dummy)

# La standardizzazione è obbligatoria per la distanza Euclidea usata da k-means
vars_scaled <- scale(vars_num)

cat("\nRQ2, variabili comportamentali codificate e standardizzate:",
    ncol(vars_scaled), "colonne,", nrow(vars_scaled), "osservazioni\n")
print(colnames(vars_scaled))

save_processed(as.data.frame(vars_scaled), "rq2_variabili_standardizzate", dir = dir_data_rq2)
cat("\nTabella salvata in RQ2/data/\n")
