if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here", repos = "https://cloud.r-project.org")
}
source(here::here("analisi_descrittiva", "setup.R"))

if (!requireNamespace("scatterplot3d", quietly = TRUE)) {
  install.packages("scatterplot3d", repos = "https://cloud.r-project.org")
}
library(scatterplot3d)

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
df <- tryCatch(
  load_processed("df_features"),
  error = function(e) engineer_features(load_raw_data())
)
# seed fissato per rendere riproducibile il clustering
set.seed(123)
km_final <- kmeans(vars_scaled, centers = k_scelto, nstart = 25)
df$cluster <- factor(km_final$cluster)

cat("\nRQ2, k-means finale con k =", k_scelto, "\n")
print(table(df$cluster))

# Si proiettano le 15 colonne standardizzate sulle prime componenti principali solo per il grafico
pca <- prcomp(vars_scaled)
var_spiegata <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)
coord_2d <- as.data.frame(pca$x[, 1:2])
colnames(coord_2d) <- c("PC1", "PC2")
coord_3d <- as.data.frame(pca$x[, 1:3])
colnames(coord_3d) <- c("PC1", "PC2", "PC3")
cat("\nRQ2, varianza spiegata dalle prime due componenti principali:",
    var_spiegata[1], "% +", var_spiegata[2], "% =", sum(var_spiegata[1:2]), "%\n")
cat("RQ2, varianza spiegata dalle prime tre componenti principali:",
    var_spiegata[1], "% +", var_spiegata[2], "% +", var_spiegata[3], "% =",
    sum(var_spiegata[1:3]), "%\n")

# La percentuale è bassa perchè 15 colonne in gran parte dummy si comprimono male in poche componenti
profilo <- df %>%
  dplyr::group_by(cluster) %>%
  dplyr::summarise(
    n = dplyr::n(),
    FAF_media = round(mean(FAF), 2),
    CH2O_media = round(mean(CH2O), 2),
    TUE_media = round(mean(TUE), 2),
    pct_FAVC_yes = round(mean(FAVC == "yes") * 100, 1),
    pct_SMOKE_yes = round(mean(SMOKE == "yes") * 100, 1),
    pct_MTRANS_auto = round(mean(MTRANS == "Automobile") * 100, 1),
    pct_MTRANS_pubblico = round(mean(MTRANS == "Public_Transportation") * 100, 1)
  )
cat("\nRQ2, profilazione dei cluster\n")
print(as.data.frame(profilo))

tab_ct <- table(df$cluster, df$target)
cat("\nRQ2, tabella di contingenza cluster e target\n")
print(tab_ct)

chi_ct <- chisq.test(tab_ct)
cat("\nRQ2, test chi-quadro cluster e target\n")
print(chi_ct)

prevalenza_target <- round(prop.table(tab_ct, margin = 1)[, "Yes"] * 100, 1)
prevalenza_media <- round(mean(df$target == "Yes") * 100, 1)
cat("\nPrevalenza di target Yes per cluster, percentuale\n")
print(prevalenza_target)
cat("Prevalenza media campionaria:", prevalenza_media, "%\n")

salva_png("profilazione_cluster.png", dir = dir_output_rq2,
          larghezza = 1000, altezza = 650, disegna = function() {
  vars_plot <- c("FAF_media", "CH2O_media", "TUE_media")
  colori_var <- c("#2a78d6", "#eb6834", "#34a853")
  mat <- t(as.matrix(profilo[, vars_plot]))
  colnames(mat) <- paste("Cluster", profilo$cluster)

  par(mar = c(5, 5, 4, 2))
  barplot(mat, beside = TRUE, col = colori_var, border = "white",
          ylab = "Valore medio", ylim = c(0, max(mat) * 1.25),
          main = "RQ2, profilazione dei cluster su variabili comportamentali continue")
  legend("topleft", legend = vars_plot, fill = colori_var,
         bty = "n", cex = 0.85)
})

salva_png("cluster_2d_pca.png", dir = dir_output_rq2,
          larghezza = 750, altezza = 650, disegna = function() {
  palette_cluster <- c("#2a78d6", "#eb6834", "#34a853", "#f9ab00", "#8e44ad", "#16a085")
  colori <- palette_cluster[as.integer(df$cluster)]
  centroidi <- aggregate(coord_2d, by = list(cluster = df$cluster), mean)

  par(mar = c(5, 5, 4, 2))
  plot(coord_2d$PC1, coord_2d$PC2, col = colori, pch = 19, cex = 0.6,
       xlab = paste0("PC1, ", var_spiegata[1], "% varianza spiegata"),
       ylab = paste0("PC2, ", var_spiegata[2], "% varianza spiegata"),
       main = "RQ2, cluster proiettati sulle prime due componenti principali")
  points(centroidi$PC1, centroidi$PC2,
         col = palette_cluster[seq_len(k_scelto)], pch = 8, cex = 2.5, lwd = 3)
  legend("topright", legend = paste("Cluster", levels(df$cluster)),
         col = palette_cluster[seq_len(k_scelto)], pch = 19, bty = "n", cex = 0.9)
})

# angle = 65 è la vista che separa più chiaramente i cluster tra quelle confrontate
salva_png("cluster_3d_pca.png", dir = dir_output_rq2,
          larghezza = 800, altezza = 700, disegna = function() {
  palette_cluster <- c("#2a78d6", "#eb6834", "#34a853", "#f9ab00", "#8e44ad", "#16a085")
  colori <- palette_cluster[as.integer(df$cluster)]
  centroidi_3d <- aggregate(coord_3d, by = list(cluster = df$cluster), mean)

  par(mar = c(3, 3, 4, 2))
  s3d <- scatterplot3d(
    coord_3d$PC1, coord_3d$PC2, coord_3d$PC3,
    color = colori, pch = 19, cex.symbols = 0.6, angle = 65,
    xlab = paste0("PC1, ", var_spiegata[1], "%"),
    ylab = paste0("PC2, ", var_spiegata[2], "%"),
    zlab = paste0("PC3, ", var_spiegata[3], "%"),
    main = "RQ2, cluster proiettati sulle prime tre componenti principali")
  # i centroidi vanno proiettati con la stessa trasformazione prospettica del grafico
  s3d$points3d(centroidi_3d$PC1, centroidi_3d$PC2, centroidi_3d$PC3,
               col = palette_cluster[seq_len(k_scelto)], pch = 8, cex = 2.5, lwd = 3)
  legend("topright", legend = paste("Cluster", levels(df$cluster)),
         col = palette_cluster[seq_len(k_scelto)], pch = 19, bty = "n", cex = 0.9)
})

salva_png("prevalenza_target_per_cluster.png", dir = dir_output_rq2,
          larghezza = 700, altezza = 600, disegna = function() {
  par(mar = c(5, 5, 4, 2))
  bp <- barplot(prevalenza_target, names.arg = paste("Cluster", names(prevalenza_target)),
                col = "#2a78d6", border = "white", ylim = c(0, max(prevalenza_target) * 1.3),
                ylab = "Target Yes, percentuale", main = "RQ2, prevalenza di target Yes per cluster")
  abline(h = prevalenza_media, col = "#e34948", lty = 2, lwd = 2)
  text(x = max(bp), y = prevalenza_media, labels = paste0("media campionaria ", prevalenza_media, "%"),
       col = "#e34948", pos = 3, cex = 0.85)
  text(bp, prevalenza_target, labels = paste0(prevalenza_target, "%"), pos = 3, cex = 1)
})

save_processed(as.data.frame(profilo), "rq2_profilazione", dir = dir_data_rq2)
save_processed(data.frame(cluster = df$cluster, target = df$target), "rq2_cluster_target", dir = dir_data_rq2)
cat("\nGrafici salvati in RQ2/output/\n")
