if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

for (pkg in c("cluster", "fpc", "mclust")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(cluster)
library(fpc)

# Carico le variabili standardizzate e il numero di cluster, senza MTRANS
dir_output_rq2 <- here::here("RQ2", "output")
dir_data_rq2 <- here::here("RQ2", "data")

vars_scaled_full <- tryCatch(
  as.matrix(load_processed("rq2_variabili_standardizzate", dir = dir_data_rq2)),
  error = function(e) stop("Esegui prima preparazione_encoding.R")
)
df <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)

# Stesse 10 colonne comportamentali di clustering_senza_mtrans.R
colonne_mtrans <- grep("^MTRANS_", colnames(vars_scaled_full))
vars_no_mtrans <- vars_scaled_full[, -colonne_mtrans]

# k ereditato dalla scelta già fatta in clustering_senza_mtrans.R, stessa logica di silhouette e filtro cluster degeneri
scelta_k_no_mtrans <- tryCatch(
  load_processed("rq2_scelta_k_senza_mtrans", dir = dir_data_rq2),
  error = function(e) stop("Esegui prima clustering_senza_mtrans.R")
)
soglia_cluster_minimo <- round(0.01 * nrow(vars_no_mtrans))
validi_no_mtrans <- scelta_k_no_mtrans$cluster_minimo >= soglia_cluster_minimo
k_scelto <- scelta_k_no_mtrans$k[validi_no_mtrans][which.max(scelta_k_no_mtrans$silhouette[validi_no_mtrans])]
cat("\nRQ2 gerarchico senza MTRANS, k ereditato dal clustering k-means:", k_scelto, "\n")

# Calcolo la partizione gerarchica con metodo di Ward, senza MTRANS, e confronto con il k-means già calcolato in clustering_senza_mtrans.R
d_eucl <- dist(vars_no_mtrans, method = "euclidean")
hc <- hclust(d_eucl, method = "ward.D2")

# Taglio dell'albero gerarchico a k scelto, calcolo silhouette e dimensioni dei cluster
df$cluster_hc <- factor(cutree(hc, k = k_scelto))
sil_hc <- mean(silhouette(as.integer(df$cluster_hc), d_eucl)[, 3])
cat("\nRQ2 gerarchico senza MTRANS, dimensioni dei cluster a k =", k_scelto, "\n")
print(table(df$cluster_hc))
cat("Silhouette media della partizione gerarchica a k =", k_scelto, "è", round(sil_hc, 3), "\n")

# Stesse variabili di profilazione usate in clustering_senza_mtrans.R, per confronto diretto
profilo <- df %>%
  dplyr::group_by(cluster_hc) %>%
  dplyr::summarise(
    n = dplyr::n(),
    FAF_media = round(mean(FAF), 2),
    CH2O_media = round(mean(CH2O), 2),
    TUE_media = round(mean(TUE), 2),
    FCVC_media = round(mean(FCVC), 2),
    NCP_media = round(mean(NCP), 2),
    pct_FAVC_yes = round(mean(FAVC == "yes") * 100, 1),
    pct_SMOKE_yes = round(mean(SMOKE == "yes") * 100, 1),
    pct_MTRANS_auto = round(mean(MTRANS == "Automobile") * 100, 1)
  )
cat("\nRQ2 gerarchico senza MTRANS, profilazione dei cluster\n")
print(as.data.frame(profilo))

tab_ct <- table(df$cluster_hc, df$target)
cat("\nRQ2 gerarchico senza MTRANS, tabella di contingenza cluster e target\n")
print(tab_ct)
chi_ct <- chisq.test(tab_ct)
cat("\nRQ2 gerarchico senza MTRANS, test chi-quadro cluster e target\n")
print(chi_ct)
prevalenza_target <- round(prop.table(tab_ct, margin = 1)[, "Yes"] * 100, 1)
cat("\nPrevalenza di target Yes per cluster gerarchico, percentuale\n")
print(prevalenza_target)

# Ricalcolo il k-means con lo stesso seed e parametri usati prima, per confrontare le due partizioni a parità di k
set.seed(123)
km_confronto <- kmeans(vars_no_mtrans, centers = k_scelto, nstart = 100)
ari <- mclust::adjustedRandIndex(df$cluster_hc, km_confronto$cluster)
# L'Adjusted Rand Index vale 1 se le partizioni sono identiche, 0 se l'accordo è pari al caso
cat("\nRQ2, concordanza gerarchico e k-means, Adjusted Rand Index a k =", k_scelto, "è", round(ari, 3), "\n")
cat("\nTabella di contingenza gerarchico e k-means\n")
print(table(gerarchico = df$cluster_hc, kmeans = km_confronto$cluster))

# Stabilità bootstrap della partizione gerarchica senza MTRANS, con metodo di Jaccard, B = 100
cat("\nRQ2 gerarchico senza MTRANS, stabilità bootstrap Jaccard a k =", k_scelto, ", B = 100\n")
set.seed(123)
cb <- fpc::clusterboot(vars_no_mtrans, B = 100,
                        clustermethod = fpc::hclustCBI,
                        k = k_scelto, method = "ward.D2", scaling = FALSE,
                        count = FALSE)
stabilita <- data.frame(cluster = seq_along(cb$bootmean), jaccard_medio = round(cb$bootmean, 3))
print(stabilita)

# Dendrogramma con taglio a k scelto, con etichette dei cluster e dimensioni, senza MTRANS
salva_png("dendrogramma_senza_mtrans.png", dir = dir_output_rq2,
          larghezza = 1000, altezza = 600, disegna = function() {
  palette_cluster <- c("#2a78d6", "#eb6834", "#34a853", "#f9ab00", "#8e44ad", "#16a085")
  dend <- as.dendrogram(hc)

  n_oss <- length(hc$height) + 1
  altezza_taglio <- mean(hc$height[(n_oss - k_scelto):(n_oss - k_scelto + 1)])
  altezza_min_visibile <- 15

  ordine <- hc$order
  cluster_leaf <- as.integer(df$cluster_hc)[ordine]
  posizioni <- tapply(seq_along(ordine), cluster_leaf, mean)
  dimensioni <- table(df$cluster_hc)

  par(mar = c(4, 5, 4, 2))
  plot(dend, leaflab = "none",
       xlim = c(0, 2400),
       ylim = c(altezza_min_visibile, max(hc$height) * 1.08),
       ylab = "Altezza di fusione, metodo Ward",
       xlab = "Osservazioni ordinate dal dendrogramma",
       main = "RQ2 senza MTRANS, dendrogramma con linkage di Ward")
  axis(1, at = seq(0, 2500, by = 400))
  abline(h = altezza_taglio, col = "#e34948", lty = 2, lwd = 2)
  text(x = length(ordine) * 0.985, y = altezza_taglio,
       labels = paste0("taglio a k=", k_scelto), col = "#e34948",
       pos = 3, cex = 0.9, xpd = TRUE)
  for (cl in names(posizioni)) {
    text(x = posizioni[cl], y = altezza_taglio - 4,
         labels = paste0("n=", dimensioni[cl]),
         col = palette_cluster[as.integer(cl)], font = 2, cex = 1.3)
  }
})

save_processed(as.data.frame(profilo), "rq2_gerarchico_profilazione", dir = dir_data_rq2)
save_processed(stabilita, "rq2_gerarchico_stabilita", dir = dir_data_rq2)
cat("\nGrafico e tabelle salvati in RQ2/output/ e RQ2/data/\n")
