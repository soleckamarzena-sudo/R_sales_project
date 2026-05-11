#' Obliczanie kluczowych metryk biznesowych
#'
#' Agreguje dane do poziomu dziennego, dodaje średnią kroczącą, a następnie
#' wylicza podsumowanie: sprzedaż całkowitą, średnią, zmienność (współczynnik zmienności),
#' udział dni promocyjnych oraz średnią długość pomiędzy szczytami sprzedaży.
#'
#' @param data Ramka danych z danymi sprzedażowymi (wynik clean_sales_ts).
#' @param date_col Nazwa kolumny z datą (domyślnie "date").
#' @param sales_col Nazwa kolumny ze sprzedażą (domyślnie "sales").
#' @param promo_col Nazwa kolumny z promocjami (domyślnie "onpromotion").
#' @param ma_window Okno dla średniej kroczącej w dniach (domyślnie 7).
#'
#' @return Zwraca listę z dwoma elementami:
#' \itemize{
#'   \item \code{daily_data} - dane zagregowane do poziomu dnia ze średnią kroczącą,
#'   \item \code{metrics} - jednowierszowa ramka danych (tibble) z wyliczonymi metrykami.
#' }
#'
#' @importFrom dplyr group_by summarise arrange mutate filter pull
#' @importFrom zoo rollmean
#' @importFrom stats sd quantile
#' @importFrom rlang sym :=
#' @importFrom cli cli_alert_success
#' @importFrom tibble tibble
#' @export
compute_sales_metrics <- function(data,
                                  date_col = "date",
                                  sales_col = "sales",
                                  promo_col = "onpromotion",
                                  ma_window = 7) {

  # 1. Agregacja do poziomu dziennego (globalnie dla całej firmy)
  daily_data <- data %>%
    dplyr::group_by(!!rlang::sym(date_col)) %>%
    dplyr::summarise(
      !!sales_col := sum(!!rlang::sym(sales_col), na.rm = TRUE),
      promo = if(promo_col %in% names(data)) sum(!!rlang::sym(promo_col), na.rm = TRUE) else 0,
      .groups = "drop"
    ) %>%
    dplyr::arrange(!!rlang::sym(date_col))

  # 2. Obliczenie średniej kroczącej (Rolling Average) z pakietu zoo
  daily_data <- daily_data %>%
    dplyr::mutate(
      moving_avg = zoo::rollmean(!!rlang::sym(sales_col), k = ma_window, fill = NA, align = "right")
    )

  # 3. Wyliczenie statystyk biznesowych
  total_sales <- sum(daily_data[[sales_col]], na.rm = TRUE)
  avg_sales   <- mean(daily_data[[sales_col]], na.rm = TRUE)
  sales_sd    <- stats::sd(daily_data[[sales_col]], na.rm = TRUE)

  # Współczynnik zmienności (odchylenie standardowe podzielone przez średnią)
  volatility <- ifelse(avg_sales > 0, sales_sd / avg_sales, NA)

  # Udział dni z jakąkolwiek promocją
  promo_share <- if(sum(daily_data$promo, na.rm = TRUE) > 0) {
    sum(daily_data$promo > 0) / nrow(daily_data)
  } else {
    0
  }

  # 4. Długość między szczytami sprzedaży (szczyt definiujemy jako dzień > 90 percentyla)
  threshold <- stats::quantile(daily_data[[sales_col]], 0.90, na.rm = TRUE)
  peaks <- daily_data %>%
    dplyr::filter(!!rlang::sym(sales_col) > threshold)

  avg_days_between_peaks <- NA
  if (nrow(peaks) > 1) {
    avg_days_between_peaks <- mean(as.numeric(diff(peaks[[date_col]])))
  }

  # 5. Budowanie tabeli wynikowej
  metrics_summary <- tibble::tibble(
    total_sales = total_sales,
    avg_daily_sales = avg_sales,
    sales_volatility = volatility,
    promo_days_share = promo_share,
    avg_days_between_peaks = avg_days_between_peaks
  )

  cli::cli_alert_success("Wyliczono metryki biznesowe! (Średnia krocząca: {ma_window} dni)")

  return(list(
    daily_data = daily_data,
    metrics = metrics_summary
  ))
}
