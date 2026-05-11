#' Główna logika analizy czasowej (Funkcja wyższego rzędu)
#'
#' Funkcja łączy dane sprzedażowe z metadanymi sklepów, filtruje je według
#' wybranych kryteriów (miasto, stan, typ), a następnie dynamicznie aplikuje
#' przekazaną funkcję analityczną (np. wyliczanie metryk lub rysowanie wykresu).
#'
#' @param datasets Lista z wczytanymi zbiorami danych (wymaga 'train' i 'stores').
#' @param target_city Opcjonalnie: nazwa miasta do analizy (np. "Quito").
#' @param target_state Opcjonalnie: nazwa stanu do analizy.
#' @param target_type Opcjonalnie: typ sklepu (np. "A", "B").
#' @param date_from Opcjonalnie: data początkowa w formacie "YYYY-MM-DD".
#' @param date_to Opcjonalnie: data końcowa w formacie "YYYY-MM-DD".
#' @param FUN Funkcja, która ma zostać wykonana na wyselekcjonowanych danych.
#' Domyślnie \code{compute_sales_metrics}, ale może to być \code{plot_sales_trends}.
#'
#' @return Wynik działania przekazanej funkcji \code{FUN}.
#'
#' @importFrom dplyr left_join filter mutate
#' @importFrom cli cli_alert_info cli_alert_success
#' @export
sales_ts_logic <- function(datasets,
                           target_city = NULL,
                           target_state = NULL,
                           target_type = NULL,
                           date_from = NULL,
                           date_to = NULL,
                           FUN = compute_sales_metrics) {

  # 1. Połączenie (JOIN) głównej tabeli ze słownikiem sklepów
  if (!all(c("train", "stores") %in% names(datasets))) {
    stop("Błąd: Przekazana lista datasets musi zawierać tabele 'train' i 'stores'.")
  }

  cli::cli_alert_info("Łączenie danych o sprzedaży z lokalizacjami sklepów...")
  merged_data <- datasets$train %>%
    dplyr::left_join(datasets$stores, by = "store_nbr")

  # 2. Dynamiczne filtrowanie
  if (!is.null(target_city)) {
    merged_data <- merged_data %>% dplyr::filter(city == target_city)
  }
  if (!is.null(target_state)) {
    merged_data <- merged_data %>% dplyr::filter(state == target_state)
  }
  if (!is.null(target_type)) {
    merged_data <- merged_data %>% dplyr::filter(type == target_type)
  }
  if (!is.null(date_from)) {
    merged_data <- merged_data %>% dplyr::filter(date >= as.Date(date_from))
  }
  if (!is.null(date_to)) {
    merged_data <- merged_data %>% dplyr::filter(date <= as.Date(date_to))
  }

  if (nrow(merged_data) == 0) {
    stop("Błąd: Filtrowanie usunęło wszystkie dane! Sprawdź, czy miasto/stan są poprawne.")
  }

  cli::cli_alert_success("Dane przefiltrowane. Pozostało {nrow(merged_data)} wierszy.")

  # 3. Czyszczenie wyselekcjonowanego wycinka danych (w locie)
  clean_subset <- clean_sales_ts(merged_data)

  # 4. Zastosowanie funkcji wyższego rzędu (FUN)
  cli::cli_alert_info("Aplikowanie wybranej funkcji analitycznej...")
  result <- FUN(clean_subset)

  return(result)
}
