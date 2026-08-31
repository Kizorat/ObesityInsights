if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

if (!requireNamespace("fpc", quietly = TRUE)) {
  install.packages("fpc", repos = "https://cloud.r-project.org")
}
library(fpc)

dir_output_rq2 <- here::here("RQ2", "output")
dir_data_rq2 <- here::here("RQ2", "data")

# Carico le variabili standardizzate e il numero di cluster
vars_scaled <- tryCatch(
  as.matrix(load_processed("rq2_variabili_standardizzate", dir = dir_data_rq2)),
  error = function(e) stop("Esegui prima preparazione_encoding.R")
)
k_scelto <- tryCatch(
  readRDS(file.path(dir_data_rq2, "rq2_k_scelto.rds")),
  error = function(e) stop("Esegui prima scelta_k.R")
)

# Carico il dataset con le features ingegnerizzate
cat("\nRQ2, verifica di stabilità bootstrap Jaccard, k =", k_scelto, ", B = 100\n")
set.seed(123)
cb <- fpc::clusterboot(vars_scaled, B = 100, clustermethod = fpc::kmeansCBI,
                        k = k_scelto, count = FALSE)
print(cb$bootmean)
# Jaccard medio sotto 0.6 indica un cluster instabile, da non interpretare come un profilo robusto
stabilita <- data.frame(cluster = seq_along(cb$bootmean), jaccard_medio = round(cb$bootmean, 3))
cat("\nRQ2, stabilità bootstrap per cluster, Jaccard medio\n")
print(stabilita)

# Salvo il grafico della stabilità bootstrap per eventuali confronti con altre soluzioni di clustering
salva_png("stabilita_bootstrap.png", dir = dir_output_rq2,
          larghezza = 650, altezza = 600, disegna = function() {
  colori <- ifelse(stabilita$jaccard_medio >= 0.75, "#34a853",
             ifelse(stabilita$jaccard_medio >= 0.6, "#f9ab00", "#e34948"))
  par(mar = c(5, 5, 4, 2))
  bp <- barplot(stabilita$jaccard_medio, names.arg = paste("Cluster", stabilita$cluster),
                col = colori, border = "white", ylim = c(0, 1),
                ylab = "Jaccard medio su 100 repliche bootstrap",
                main = "RQ2, stabilità bootstrap dei cluster")
  abline(h = 0.6, col = "grey40", lty = 2)
  text(x = max(bp), y = 0.6, labels = "soglia instabilità 0.6", pos = 3, cex = 0.8, col = "grey40")
  text(bp, stabilita$jaccard_medio, labels = sprintf("%.2f", stabilita$jaccard_medio), pos = 3, cex = 1)
})
save_processed(stabilita, "rq2_stabilita_bootstrap", dir = dir_data_rq2)
cat("\nGrafico salvato in RQ2/output/\n")
