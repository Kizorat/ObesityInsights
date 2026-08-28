if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

if (!requireNamespace("cluster", quietly = TRUE)) {
  install.packages("cluster", repos = "https://cloud.r-project.org")
}
library(cluster)

dir_output_rq2 <- here::here("RQ2", "output")
dir_data_rq2 <- here::here("RQ2", "data")

# Carico le variabili standardizzate e il numero di cluster
vars_scaled <- tryCatch(
  as.matrix(load_processed("rq2_variabili_standardizzate", dir = dir_data_rq2)),
  error = function(e) stop("Esegui prima preparazione_encoding.R")
)

# Il WCSS è definito rispetto a un centroide Euclideo, coerente con dati numerici standardizzati
d_eucl <- dist(vars_scaled, method = "euclidean")

# nstart alto stabilizza WCSS e silhouette, perchè con poche colonne dummy k-means converge spesso a ottimi locali diversi
set.seed(123)
k_range <- 2:10
wcss <- numeric(length(k_range))
sil  <- numeric(length(k_range))
min_cluster <- numeric(length(k_range))
for (i in seq_along(k_range)) {
  km <- kmeans(vars_scaled, centers = k_range[i], nstart = 100)
  wcss[i] <- km$tot.withinss
  sil[i]  <- mean(silhouette(km$cluster, d_eucl)[, 3])
  min_cluster[i] <- min(table(km$cluster))
}

risultati_k <- data.frame(k = k_range, wcss = round(wcss, 0),
                           silhouette = round(sil, 3), cluster_minimo = min_cluster)
cat("\nRQ2, scelta di k da 2 a 10, WCSS, silhouette media e dimensione del cluster più piccolo\n")
print(risultati_k)

# La soglia minima per un cluster non degenere è fissata a 1% del dataset
soglia_cluster_minimo <- round(0.01 * nrow(vars_scaled))
validi <- risultati_k$cluster_minimo >= soglia_cluster_minimo
k_scelto <- risultati_k$k[validi][which.max(risultati_k$silhouette[validi])]
cat("\nSoglia minima per cluster non degenere:", soglia_cluster_minimo, "osservazioni\n")
cat("k scelto, silhouette massima tra le soluzioni senza cluster degeneri:", k_scelto, "\n")

# Salvo il grafico del gomito per eventuali confronti con altre soluzioni di clustering
salva_png("gomito.png", dir = dir_output_rq2,
          larghezza = 650, altezza = 550, disegna = function() {
  par(mar = c(5, 5, 4, 2))
  plot(risultati_k$k, risultati_k$wcss, type = "b", pch = 19, col = "#2a78d6", lwd = 2,
       xlab = "Numero di cluster", ylab = "WCSS, somma dei quadrati interni al cluster",
       main = "RQ2, metodo del gomito")
})

# Salvo il grafico di silhouette per eventuali confronti con altre soluzioni di clustering
salva_png("silhouette.png", dir = dir_output_rq2,
          larghezza = 650, altezza = 550, disegna = function() {
  colori_punti <- ifelse(validi, "#2a78d6", "#b0b0b0")
  par(mar = c(5, 5, 4, 2))
  plot(risultati_k$k, risultati_k$silhouette, type = "b", pch = 19, col = "#2a78d6", lwd = 2,
       xlab = "Numero di cluster", ylab = "Silhouette media",
       main = "RQ2, silhouette media")
  points(risultati_k$k, risultati_k$silhouette, pch = 19, col = colori_punti, cex = 1.6)
  abline(v = k_scelto, col = "#e34948", lty = 2, lwd = 2)
  legend("bottomright", legend = c("cluster minimo sostanziale", "cluster minimo degenere, sotto 1%"),
         col = c("#2a78d6", "#b0b0b0"), pch = 19, bty = "n", cex = 0.8)
})

# Salvo i risultati della scelta di k per eventuali confronti con altre soluzioni di clustering
save_processed(risultati_k, "rq2_scelta_k", dir = dir_data_rq2)
saveRDS(k_scelto, file.path(dir_data_rq2, "rq2_k_scelto.rds"))
cat("\nGrafici salvati in RQ2/output/\n")
