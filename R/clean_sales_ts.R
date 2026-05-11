#' Czyszczenie i przygotowanie szeregów czasowych
#'
#' Sortuje dane, rozwiązuje problem zduplikowanych wpisów agregując je
#' wybraną funkcją, zachowuje dane o promocjach i obsługuje braki danych (NA).
#'
#' @param data Ramka danych (tibble).
#' @param date_col Nazwa kolumny z datą.
#' @param sales_col Nazwa kolumny ze sprzedażą.
#' @param promo_col Nazwa kolumny z promocjami (domyślnie "onpromotion").
#' @param handle_na Sposób obsługi NA: "zero" lub "drop".
#' @param aggregate_time Opcjonalna agregacja czasu.
#' @param agg_func Funkcja agregacji duplikatów (domyślnie sum).
#'
#' @return Oczyszczona ramka danych.
#'
#' @importFrom dplyr arrange group_by summarise across all_of filter mutate
#' @importFrom lubridate floor_date
#' @importFrom tidyr replace_na drop_na
#' @importFrom cli cli_alert_success cli_alert_info
#' @importFrom rlang sym :=
#' @export
clean_sales_ts <- function(data,
                           date_col = "date",
                           sales_col = "sales",
                           promo_col = "onpromotion",
                           handle_na = "zero",
                           aggregate_time = NULL,
                           agg_func = sum) {

  data <- data %>% dplyr::arrange(!!rlang::sym(date_col))

  if (!is.null(aggregate_time)) {
    data <- data %>% dplyr::mutate(!!date_col := lubridate::floor_date(!!rlang::sym(date_col), unit = aggregate_time))
  }

  grouping_cols <- intersect(names(data), c(date_col, "store_nbr", "family"))

  if (length(grouping_cols) > 0 && sales_col %in% names(data)) {

    # NOWOŚĆ: Jeśli w surowych danych jest kolumna z promocjami, zsumujmy ją podczas grupowania!
    if (promo_col %in% names(data)) {
      data <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
        dplyr::summarise(
          !!sales_col := agg_func(!!rlang::sym(sales_col), na.rm = TRUE),
          !!promo_col := sum(!!rlang::sym(promo_col), na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      data <- data %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(grouping_cols))) %>%
        dplyr::summarise(!!sales_col := agg_func(!!rlang::sym(sales_col), na.rm = TRUE), .groups = "drop")
    }
  }

  if (sales_col %in% names(data)) {
    if (handle_na == "zero") {
      data <- data %>%
        dplyr::mutate(!!sales_col := tidyr::replace_na(!!rlang::sym(sales_col), 0)) %>%
        dplyr::mutate(!!sales_col := ifelse(!!rlang::sym(sales_col) < 0, 0, !!rlang::sym(sales_col)))

      # Zabezpieczenie braków (NA) również w promocjach
      if (promo_col %in% names(data)) {
        data <- data %>% dplyr::mutate(!!promo_col := tidyr::replace_na(!!rlang::sym(promo_col), 0))
      }

    } else if (handle_na == "drop") {
      data <- data %>%
        tidyr::drop_na(dplyr::all_of(sales_col)) %>%
        dplyr::filter(!!rlang::sym(sales_col) >= 0)
    }
  }

  cli::cli_alert_success("Czyszczenie zakończone (zachowano dane promocyjne)!")
  return(data)
}
