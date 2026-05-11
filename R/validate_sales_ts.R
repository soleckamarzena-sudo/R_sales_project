#' Walidacja jakości danych sprzedażowych
#'
#' Sprawdza poprawność danych: identyfikuje braki (NA), zduplikowane daty
#' oraz wartości ujemne.
#'
#' @param data Ramka danych (tibble) z danymi sprzedażowymi.
#' @param date_col Nazwa kolumny z datą (domyślnie "date").
#' @param sales_col Nazwa kolumny z wielkością sprzedaży (domyślnie "sales").
#'
#' @return Zwraca `TRUE` jeśli dane są poprawne, lub `FALSE` i ostrzeżenia.
#'
#' @importFrom dplyr filter group_by summarise n across all_of
#' @importFrom cli cli_h1 cli_alert_success cli_alert_danger cli_alert_warning
#' @export
validate_sales_ts <- function(data, date_col = "date", sales_col = "sales") {

  cli::cli_h1("Raport walidacji danych")
  is_valid <- TRUE

  if (!(date_col %in% names(data))) {
    cli::cli_alert_danger("Błąd: Brak kolumny z datą '{date_col}'.")
    return(FALSE)
  }

  if (sales_col %in% names(data)) {
    if (sum(is.na(data[[sales_col]])) > 0) {
      cli::cli_alert_warning("Znaleziono braki danych (NA) w sprzedaży.")
      is_valid <- FALSE
    }
    if (sum(data[[sales_col]] < 0, na.rm = TRUE) > 0) {
      cli::cli_alert_warning("Znaleziono ujemne wartości sprzedaży.")
      is_valid <- FALSE
    }
  }

  grouping_cols <- c(date_col)
  if ("store_nbr" %in% names(data)) grouping_cols <- c(grouping_cols, "store_nbr")
  if ("family" %in% names(data)) grouping_cols <- c(grouping_cols, "family")

  duplicates <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(count > 1)

  if (nrow(duplicates) > 0) {
    cli::cli_alert_danger("Znaleziono zduplikowane daty dla sklepów/kategorii!")
    is_valid <- FALSE
  }

  if (is_valid) {
    cli::cli_alert_success("Dane są czyste i gotowe do analizy!")
  } else {
    cli::cli_alert_warning("Zalecane czyszczenie funkcją clean_sales_ts().")
  }

  return(invisible(is_valid))
}
