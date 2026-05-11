#' Wizualizacja trendów sprzedażowych
#'
#' Generuje wykres liniowy dla szeregu czasowego sprzedaży. Aby wykres był czytelny,
#' funkcja domyślnie agreguje dane do poziomu tygodniowego. Dodaje również wygładzoną
#' linię trendu.
#'
#' @param data Oczyszczona ramka danych ze sprzedażą.
#' @param date_col Nazwa kolumny z datą (domyślnie "date").
#' @param sales_col Nazwa kolumny ze sprzedażą (domyślnie "sales").
#'
#' @return Obiekt wykresu typu ggplot.
#'
#' @importFrom dplyr mutate group_by summarise
#' @importFrom lubridate floor_date
#' @importFrom ggplot2 ggplot aes geom_line geom_smooth labs theme_minimal scale_y_continuous theme
#' @importFrom rlang sym :=
#' @importFrom cli cli_alert_success
#' @export
plot_sales_trends <- function(data, date_col = "date", sales_col = "sales") {

  # 1. Agregacja do poziomu tygodnia (dane dzienne na wykresie z kilku lat to "szum")
  plot_data <- data %>%
    dplyr::mutate(week_date = lubridate::floor_date(!!rlang::sym(date_col), "week")) %>%
    dplyr::group_by(week_date) %>%
    dplyr::summarise(!!sales_col := sum(!!rlang::sym(sales_col), na.rm = TRUE), .groups = "drop")

  # 2. Tworzenie wykresu ggplot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = week_date, y = !!rlang::sym(sales_col))) +
    ggplot2::geom_line(color = "steelblue", alpha = 0.8, linewidth = 0.8) +
    # Dodajemy linię trendu (metoda LOESS)
    ggplot2::geom_smooth(method = "loess", color = "darkred", se = FALSE, linetype = "dashed") +
    ggplot2::scale_y_continuous(labels = scales::comma) + # Formatowanie osi Y na liczby z przecinkami
    ggplot2::labs(
      title = "Tygodniowy trend sprzedaży całkowitej",
      subtitle = "Czerwona przerywana linia wyznacza ogólny trend",
      x = "Data",
      y = "Suma sprzedaży"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

  cli::cli_alert_success("Wykres został pomyślnie wygenerowany!")

  return(p)
}
