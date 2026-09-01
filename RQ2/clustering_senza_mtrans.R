if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

for (pkg in c("cluster", "fpc", "scatterplot3d")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}
library(cluster)
library(fpc)
library(scatterplot3d)

dir_output_rq2 <- here::here("RQ2", "output")
dir_data_rq2 <- here::here("RQ2", "data")
# Carico le variabili standardizzate e il numero di cluster
vars_scaled_full <- tryCatch(
  as.matrix(load_processed("rq2_variabili_standardizzate", dir = dir_data_rq2)),
  error = function(e) stop("Esegui prima preparazione_encoding.R")
)
df <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)

# Le colonne MTRANS sono standardizzate per colonna, quindi rimuoverle non altera la scala delle altre
colonne_mtrans <- grep("^MTRANS_", colnames(vars_scaled_full))
vars_no_mtrans <- vars_scaled_full[, -colonne_mtrans]
cat("\nRQ2 senza MTRANS, colonne usate per il clustering\n")
print(colnames(vars_no_mtrans))

# Stessa logica di scelta di k 
d_eucl <- dist(vars_no_mtrans, method = "euclidean")
set.seed(123)
k_range <- 2:10
wcss <- numeric(length(k_range))
sil  <- numeric(length(k_range))
min_cluster <- numeric(length(k_range))
for (i in seq_along(k_range)) {
  km <- kmeans(vars_no_mtrans, centers = k_range[i], nstart = 100)
  wcss[i] <- km$tot.withinss
  sil[i]  <- mean(silhouette(km$cluster, d_eucl)[, 3])
  min_cluster[i] <- min(table(km$cluster))
}
risultati_k <- data.frame(k = k_range, wcss = round(wcss, 0),
                           silhouette = round(sil, 3), cluster_minimo = min_cluster)
cat("\nRQ2 senza MTRANS, scelta di k da 2 a 10\n")
print(risultati_k)

soglia_cluster_minimo <- round(0.01 * nrow(vars_no_mtrans))
validi <- risultati_k$cluster_minimo >= soglia_cluster_minimo
k_scelto <- risultati_k$k[validi][which.max(risultati_k$silhouette[validi])]
cat("\nk scelto senza MTRANS:", k_scelto, "silhouette:", risultati_k$silhouette[risultati_k$k == k_scelto], "\n")
# Seed fissato per rendere riproducibile il clustering
set.seed(123)
km_final <- kmeans(vars_no_mtrans, centers = k_scelto, nstart = 100)
df$cluster_no_mtrans <- factor(km_final$cluster)

cat("\nRQ2 senza MTRANS, k-means finale con k =", k_scelto, "\n")
print(table(df$cluster_no_mtrans))

# profilazione dei cluster senza MTRANS
profilo <- df %>%
  dplyr::group_by(cluster_no_mtrans) %>%
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
cat("\nRQ2 senza MTRANS, profilazione dei cluster\n")
print(as.data.frame(profilo))
# pct_MTRANS_auto è solo una verifica a posteriori, MTRANS non ha preso parte al clustering

tab_ct <- table(df$cluster_no_mtrans, df$target)
cat("\nRQ2 senza MTRANS, tabella di contingenza cluster e target\n")
print(tab_ct)
chi_ct <- chisq.test(tab_ct)
cat("\nRQ2 senza MTRANS, test chi-quadro cluster e target\n")
print(chi_ct)
prevalenza_target <- round(prop.table(tab_ct, margin = 1)[, "Yes"] * 100, 1)
prevalenza_media <- round(mean(df$target == "Yes") * 100, 1)
cat("\nPrevalenza di target Yes per cluster senza MTRANS, percentuale\n")
print(prevalenza_target)
cat("Prevalenza media campionaria:", prevalenza_media, "%\n")

cat("\nRQ2 senza MTRANS, stabilità bootstrap Jaccard, k =", k_scelto, ", B = 100\n")
set.seed(123)
cb <- fpc::clusterboot(vars_no_mtrans, B = 100, clustermethod = fpc::kmeansCBI,
                        k = k_scelto, count = FALSE)
stabilita <- data.frame(cluster = seq_along(cb$bootmean), jaccard_medio = round(cb$bootmean, 3))
print(stabilita)

salva_png("scelta_k_senza_mtrans.png", dir = dir_output_rq2,
          larghezza = 650, altezza = 550, disegna = function() {
  colori_punti <- ifelse(validi, "#2a78d6", "#b0b0b0")
  par(mar = c(5, 5, 4, 2))
  plot(risultati_k$k, risultati_k$silhouette, type = "b", pch = 19, col = "#2a78d6", lwd = 2,
       xlab = "Numero di cluster", ylab = "Silhouette media",
       main = "RQ2 senza MTRANS, silhouette media")
  points(risultati_k$k, risultati_k$silhouette, pch = 19, col = colori_punti, cex = 1.6)
  abline(v = k_scelto, col = "#e34948", lty = 2, lwd = 2)
  legend("bottomright", legend = c("cluster minimo sostanziale", "cluster minimo degenere, sotto 1%"),
         col = c("#2a78d6", "#b0b0b0"), pch = 19, bty = "n", cex = 0.8)
})

pca <- prcomp(vars_no_mtrans)
var_spiegata <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
coord_2d <- as.data.frame(pca$x[, 1:2])
colnames(coord_2d) <- c("PC1", "PC2")
coord_3d <- as.data.frame(pca$x[, 1:3])
colnames(coord_3d) <- c("PC1", "PC2", "PC3")
cat("\nRQ2 senza MTRANS, varianza spiegata dalle prime tre componenti principali:",
    var_spiegata[1], "% +", var_spiegata[2], "% +", var_spiegata[3], "% =",
    sum(var_spiegata[1:3]), "%\n")

salva_png("cluster_2d_senza_mtrans.png", dir = dir_output_rq2,
          larghezza = 750, altezza = 650, disegna = function() {
  palette_cluster <- c("#2a78d6", "#eb6834", "#34a853", "#f9ab00", "#8e44ad", "#16a085")
  colori <- palette_cluster[as.integer(df$cluster_no_mtrans)]
  centroidi <- aggregate(coord_2d, by = list(cluster = df$cluster_no_mtrans), mean)

  par(mar = c(5, 5, 4, 2))
  plot(coord_2d$PC1, coord_2d$PC2, col = colori, pch = 19, cex = 0.6,
       xlab = paste0("PC1, ", var_spiegata[1], "% varianza spiegata"),
       ylab = paste0("PC2, ", var_spiegata[2], "% varianza spiegata"),
       main = "RQ2 senza MTRANS, cluster sulle prime due componenti principali")
  points(centroidi$PC1, centroidi$PC2,
         col = palette_cluster[seq_len(k_scelto)], pch = 8, cex = 2.5, lwd = 3)
  legend("topright", legend = paste("Cluster", levels(df$cluster_no_mtrans)),
         col = palette_cluster[seq_len(k_scelto)], pch = 19, bty = "n", cex = 0.9)
})

# angle = 65, stessa vista usata nel clustering principale di clustering_profilazione.R, scelta come la più separata tra quelle confrontate
salva_png("cluster_3d_senza_mtrans.png", dir = dir_output_rq2,
          larghezza = 800, altezza = 700, disegna = function() {
  palette_cluster <- c("#2a78d6", "#eb6834", "#34a853", "#f9ab00", "#8e44ad", "#16a085")
  colori <- palette_cluster[as.integer(df$cluster_no_mtrans)]
  centroidi_3d <- aggregate(coord_3d, by = list(cluster = df$cluster_no_mtrans), mean)

  par(mar = c(3, 3, 4, 2))
  s3d <- scatterplot3d(
    coord_3d$PC1, coord_3d$PC2, coord_3d$PC3,
    color = colori, pch = 19, cex.symbols = 0.6, angle = 65,
    xlab = paste0("PC1, ", var_spiegata[1], "%"),
    ylab = paste0("PC2, ", var_spiegata[2], "%"),
    zlab = paste0("PC3, ", var_spiegata[3], "%"),
    main = "RQ2 senza MTRANS, cluster sulle prime tre componenti principali")
  s3d$points3d(centroidi_3d$PC1, centroidi_3d$PC2, centroidi_3d$PC3,
               col = palette_cluster[seq_len(k_scelto)], pch = 8, cex = 2.5, lwd = 3)
  legend("topright", legend = paste("Cluster", levels(df$cluster_no_mtrans)),
         col = palette_cluster[seq_len(k_scelto)], pch = 19, bty = "n", cex = 0.9)
})

# Verifica del possibile confondimento con Gender: se il cluster più a rischio fosse anche sproporzionatamente maschile, la sua prevalenza alta si spiegherebbe con il sesso e non con le abitudini
quota_fumatori <- tapply(as.integer(df$SMOKE == "yes"), df$cluster_no_mtrans, mean)
cl_fumatori <- names(which.max(quota_fumatori))
tab_gender_cluster <- table(df$cluster_no_mtrans == cl_fumatori, df$Gender)
rownames(tab_gender_cluster) <- c("Altri cluster", paste("Cluster", cl_fumatori))

cat("\nRQ2, composizione per sesso del cluster più a rischio\n")
print(tab_gender_cluster)
cat("Percentuale di maschi nel cluster:",
    round(100 * tab_gender_cluster[2, "Male"] / sum(tab_gender_cluster[2, ]), 1),
    "% contro", round(100 * mean(df$Gender == "Male"), 1), "% nel campione\n")
test_gender_cluster <- chisq.test(tab_gender_cluster)
print(test_gender_cluster)

confondimento <- data.frame(
  cluster = cl_fumatori,
  n = sum(df$cluster_no_mtrans == cl_fumatori),
  maschi = tab_gender_cluster[2, "Male"],
  perc_maschi = round(100 * tab_gender_cluster[2, "Male"] / sum(tab_gender_cluster[2, ]), 1),
  perc_maschi_campione = round(100 * mean(df$Gender == "Male"), 1),
  chi_quadro = round(unname(test_gender_cluster$statistic), 3),
  p_value = round(test_gender_cluster$p.value, 4)
)
save_processed(confondimento, "rq2_confondimento_gender", dir = dir_data_rq2)

save_processed(risultati_k, "rq2_scelta_k_senza_mtrans", dir = dir_data_rq2)
save_processed(as.data.frame(profilo), "rq2_profilazione_senza_mtrans", dir = dir_data_rq2)
save_processed(stabilita, "rq2_stabilita_senza_mtrans", dir = dir_data_rq2)
cat("\nGrafici e tabelle salvati in RQ2/output/ e RQ2/data/\n")
