# --- KOMPLETNY WORKFLOW PROJEKTU ---

required_packages <- c(
  "dplyr",
  "readr",
  "tidyr",
  "lubridate",
  "cli",
  "rlang",
  "ggplot2",
  "zoo",
  "tsibble",
  "fable",
  "feasts",
  "prophet",
  "xgboost"
)

installed_packages <- rownames(installed.packages())

to_install <- setdiff(required_packages, installed_packages)

if (length(to_install) > 0) {
  install.packages(to_install)
}

# 1. Deklaracja używanych pakietów
usethis::use_package("dplyr")
usethis::use_package("readr")
usethis::use_package("tidyr")
usethis::use_package("lubridate")
usethis::use_package("cli")
usethis::use_package("rlang")
usethis::use_package("ggplot2")
usethis::use_package("zoo")
usethis::use_package("tsibble")
usethis::use_package("fable")
usethis::use_package("feasts")
usethis::use_package("prophet")
usethis::use_package("xgboost")

# 2. Ręczne ładowanie wszystkich funkcji ze skryptów
source("./R/load_sales_data.R")
source("./R/validate_sales_ts.R")
source("./R/clean_sales_ts.R")
source("./R/compute_sales_metric.R")
source("./R/plot_sales_trends.R")
source("./R/management_summary.R")
source("./R/create_prognosis.R")
source("./R/sales_ts_logic.R")

# 3. Wczytywanie danych z Twojej ścieżki
datas <- load_sales_data(file.path(getwd(), "data"))

# 4. Walidacja i czyszczenie
validate_sales_ts(datas$train)
clean_train <- clean_sales_ts(datas$train)

# --- ANALIZA GLOBALNA (Poziom 1 i 2) ---
metrics <- compute_sales_metrics(clean_train)
print(metrics$metrics)

plot <- plot_sales_trends(clean_train)
print(plot)

report <- create_management_summary(clean_train)

# --- ZAAWANSOWANA LOGIKA FILTROWANIA (Poziom 3) ---

# Przykład użycia nowej funkcji: metryki tylko dla sklepów typu "A" w mieście "Quito"
metryki_quito <- sales_ts_logic(
  datasets = datas, 
  target_city = "Quito", 
  target_type = "A", 
  FUN = compute_sales_metrics
)
print(metryki_quito$metrics)

# --- PROGNOZOWANIE (Poziom 3) ---

# XGBoost
wykres_xgb <- create_prognosis(clean_train, horizon = 30, method = "xgboost")
print(wykres_xgb)

# Prophet (z kalendarzem świąt)
wykres_prophet <- create_prognosis(clean_train, holidays_data = datas$holidays, horizon = 30, method = "prophet")
print(wykres_prophet)

# ARIMA
wykres_arima <- create_prognosis(clean_train, horizon = 30, method = "arima")
print(wykres_arima)
