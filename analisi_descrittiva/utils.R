# Apre un file PNG, disegna dentro con la funzione "disegna" e lo chiude, così non serve ripetere png()/dev.off() in ogni grafico.
salva_png <- function(nomefile, disegna, larghezza = 800, altezza = 700,
                       dir = here::here("output", "figures")) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  png(file.path(dir, nomefile), width = larghezza, height = altezza)
  disegna()
  invisible(dev.off())
}

# Legge il CSV originale del dataset (dati grezzi, non ancora puliti)
load_raw_data <- function(path = here::here("dataset", "Obesity_onevsrest_5.csv")) {
  readr::read_csv(path, show_col_types = FALSE)
}

# Nomi delle variabili continue (numeriche) del dataset
get_continuous_vars <- function() {
  c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE")
}

# Nomi delle variabili categoriche, target incluso
get_categorical_vars <- function() {
  c("Gender", "family_history_with_overweight", "FAVC", "CAEC",
    "SMOKE", "SCC", "CALC", "MTRANS", "target")
}

# Nomi delle variabili categoriche escluso il target (solo i predittori)
get_categorical_predictors <- function() {
  setdiff(get_categorical_vars(), "target")
}

# Conta quante osservazioni di x sono outlier secondo la regola IQR, fuori dall'intervallo [Q1 - 1.5*IQR, Q3 + 1.5*IQR]
check_outliers_iqr <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  sum(x < q1 - 1.5 * iqr | x > q3 + 1.5 * iqr, na.rm = TRUE)
}

# Calcola l'indice di asimmetria (skewness) di un campione a mano, senza dipendere da pacchetti esterni
skewness_manual <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  (sum((x - m)^3) / n) / s^3
}

# Percentuale di valori di x che non sono numeri interi
pct_non_integer <- function(x) {
  x <- x[!is.na(x)]
  round(mean(x != round(x)) * 100, 1)
}

# Costruisce una tabella riassuntiva (media, mediana, sd, min, max, asimmetria, ecc.) per ciascuna variabile continua indicata
describe_continuous <- function(df, vars = get_continuous_vars()) {
  righe <- lapply(vars, function(v) {
    x <- df[[v]]
    data.frame(
      variabile = v,
      n = sum(!is.na(x)),
      mancanti = sum(is.na(x)),
      media = mean(x, na.rm = TRUE),
      mediana = median(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      q1 = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
      q3 = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
      max = max(x, na.rm = TRUE),
      asimmetria = skewness_manual(x),
      pct_non_intero = pct_non_integer(x)
    )
  })
  out <- do.call(rbind, righe)
  out[, -1] <- round(out[, -1], 2)
  out
}

# Restituisce le categorie di x la cui frequenza percentuale è sotto soglia (default < 1%), utile per individuare categorie rare
rare_categories <- function(x, soglia = 1) {
  p <- round(prop.table(table(x)) * 100, 2)
  p[p < soglia]
}

# Confronta una variabile continua x tra i due gruppi No/Yes del target: normalità (Shapiro), omogeneità delle varianze (var.test), test di Welch e di Wilcoxon
compare_continuous_by_group <- function(x, group) {
  group <- factor(group)
  gruppi <- split(x, group)
  shapiro_p <- vapply(gruppi, function(g) {
    g <- g[!is.na(g)]
    if (length(g) >= 3 && length(g) <= 5000) shapiro.test(g)$p.value else NA_real_
  }, numeric(1))

  data.frame(
    media_per_gruppo = paste(round(vapply(gruppi, mean, numeric(1), na.rm = TRUE), 2), collapse = " vs "),
    shapiro_p_min = round(min(shapiro_p, na.rm = TRUE), 4),
    var_test_p = round(var.test(x ~ group)$p.value, 4),
    welch_p = round(t.test(x ~ group)$p.value, 4),
    wilcoxon_p = round(wilcox.test(x ~ group)$p.value, 4)
  )
}

# Testa l'associazione tra una variabile categorica x e il target y: chi-quadro se le frequenze attese sono tutte >= 5, altrimenti test esatto di Fisher
screen_categorical <- function(x, y) {
  tab <- table(x, y)
  atteso <- suppressWarnings(chisq.test(tab, correct = FALSE)$expected)
  if (any(atteso < 5)) {
    test <- tryCatch(
      fisher.test(tab),
      error = function(e) fisher.test(tab, simulate.p.value = TRUE, B = 10000)
    )
    metodo <- "fisher"
  } else {
    test <- chisq.test(tab)
    metodo <- "chi-quadro"
  }
  data.frame(metodo = metodo, p_value = unname(test$p.value), cramer_v = cramer_v(tab))
}

# Calcola il V di Cramer, forza dell'associazione tra due variabili categoriche, a partire da una tabella di contingenza
cramer_v <- function(tab) {
  chi2 <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)
  n <- sum(tab)
  k <- min(nrow(tab), ncol(tab))
  as.numeric(sqrt(chi2 / (n * (k - 1))))
}

# Disegna un pie chart se la variabile ha meno di 4 categorie, altrimenti un barplot, piu' leggibile quando le etichette sono numerose
plot_categorical <- function(x, main = "") {
  tab <- sort(table(x), decreasing = TRUE)
  if (length(tab) >= 4) {
    barplot(tab, main = main, col = "#2a78d6", border = "white",
            las = 2, cex.names = 0.9)
  } else {
    palette_pie <- c("#2a78d6", "#eb6834", "#34a853", "#f9ab00")
    pie(tab, main = main, col = palette_pie[seq_along(tab)],
        labels = paste0(names(tab), ": ", round(100 * tab / sum(tab), 1), "%"))
  }
}

# Applica le trasformazioni di feature engineering al dataset grezzo: BMI, arrotondamento delle variabili quasi-Likert, factor per le categoriche accorpando CALC=Always, troppo rara, con Frequently, e target come fattore No/Yes
engineer_features <- function(df) {
  df %>%
    dplyr::mutate(
      BMI = Weight / (Height^2),
      Age = round(Age),
      FCVC = round(FCVC),
      NCP = round(NCP),
      CH2O = round(CH2O),
      FAF = round(FAF),
      TUE = round(TUE),
      Gender = factor(Gender),
      family_history_with_overweight = factor(family_history_with_overweight, levels = c("no", "yes")),
      FAVC = factor(FAVC, levels = c("no", "yes")),
      SMOKE  = factor(SMOKE, levels = c("no", "yes")),
      SCC = factor(SCC, levels = c("no", "yes")),
      MTRANS = factor(MTRANS),
      CAEC = factor(CAEC, levels = c("no", "Sometimes", "Frequently", "Always")),
      CALC = dplyr::case_when(
        CALC %in% c("Frequently", "Always") ~ "Frequently_o_Always",
        TRUE ~ CALC
      ),
      CALC = factor(CALC, levels = c("no", "Sometimes", "Frequently_o_Always")),
      target = factor(target, levels = c(0, 1), labels = c("No", "Yes"))
    )
}

# Salva un data frame come CSV dentro data/processed/, così che gli script successivi possano ricaricarlo senza rifare i calcoli
save_processed <- function(df, name, dir = here::here("data", "processed")) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  readr::write_csv(df, file.path(dir, paste0(name, ".csv")))
}

# Ricarica un data frame precedentemente salvato con save_processed()
load_processed <- function(name, dir = here::here("data", "processed")) {
  readr::read_csv(file.path(dir, paste0(name, ".csv")), show_col_types = FALSE)
}
