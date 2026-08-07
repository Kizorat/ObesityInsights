# Mirror CRAN da usare per installare i pacchetti
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Pacchetti necessari a tutti gli script del progetto
pacchetti <- c("here", "readr", "dplyr")

# Installo solo i pacchetti che non sono gia' presenti
nuovi <- pacchetti[!pacchetti %in% installed.packages()[, "Package"]]
if (length(nuovi) > 0) install.packages(nuovi)

# Carico i pacchetti senza stampare i messaggi di avvio
invisible(suppressPackageStartupMessages(
  lapply(pacchetti, library, character.only = TRUE)
))

# Seed fisso per rendere riproducibili le procedure stocastiche
set.seed(123)

# Carico le funzioni di utilità condivise
source(here::here("analisi_descrittiva", "utils.R"))
