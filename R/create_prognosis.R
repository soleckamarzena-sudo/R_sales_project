#' Generowanie prognoz sprzedaży
#'
#' Funkcja tworzy prognozy sprzedaży całkowitej przy użyciu modelu ARIMA,
#' algorytmu Prophet lub uczenia maszynowego XGBoost.
#'
#' @param data Oczyszczona ramka danych ze sprzedażą.
#' @param holidays_data Ramka danych ze świętami (domyślnie NULL).
#' @param horizon Horyzont prognozy w dniach (domyślnie 30).
#' @param method Wybór modelu: "arima", "prophet" lub "xgboost".
#'
#' @return Wykres z wygenerowaną prognozą.
#'
#' @importFrom dplyr group_by summarise rename select filter arrange mutate bind_rows slice
#' @importFrom tsibble as_tsibble fill_gaps
#' @importFrom fabletools model forecast autoplot
#' @importFrom fable ARIMA
#' @importFrom prophet prophet fit.prophet make_future_dataframe predict
#' @importFrom xgboost xgboost
#' @importFrom ggplot2 ggplot geom_line labs theme_minimal aes
#' @importFrom stats predict
#' @importFrom cli cli_alert_success cli_alert_info
#' @export
create_prognosis <- function(data, holidays_data = NULL, horizon = 30, method = "prophet") {

  cli::cli_alert_info(paste("Przygotowywanie danych do prognozy globalnej modelem:", toupper(method)))

  if (!("date" %in% names(data)) || !("sales" %in% names(data))) {
    stop("Dane muszą zawierać kolumny 'date' i 'sales'.")
  }

  daily_data <- data %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop")

  # ==============================
  # METODA 1: ARIMA
  # ==============================
  if (method == "arima") {
    daily_ts <- tsibble::as_tsibble(daily_data, index = date) %>%
      tsibble::fill_gaps(sales = 0)

    fit <- daily_ts %>% fabletools::model(arima = fable::ARIMA(sales))
    fc <- fit %>% fabletools::forecast(h = horizon)

    p <- fabletools::autoplot(fc, daily_ts) +
      ggplot2::labs(title = paste("Prognoza ARIMA na", horizon, "dni"), x = "Data", y = "Całkowita sprzedaż") +
      ggplot2::theme_minimal()

    cli::cli_alert_success("Model ARIMA wygenerował prognozę!")
    return(p)

    # ==============================
    # METODA 2: PROPHET
    # ==============================
  } else if (method == "prophet") {
    df_prophet <- daily_data %>% dplyr::rename(ds = date, y = sales)

    hols <- NULL
    if (!is.null(holidays_data)) {
      hols <- holidays_data %>%
        dplyr::filter(transferred == FALSE) %>%
        dplyr::rename(ds = date, holiday = description) %>%
        dplyr::select(ds, holiday)
    }

    m <- suppressMessages(prophet::prophet(holidays = hols, daily.seasonality = FALSE, yearly.seasonality = TRUE))
    m <- suppressMessages(prophet::fit.prophet(m, df_prophet))

    future <- prophet::make_future_dataframe(m, periods = horizon)
    forecast <- stats::predict(m, future)

    p <- plot(m, forecast) +
      ggplot2::labs(title = paste("Prognoza PROPHET na", horizon, "dni"), x = "Data", y = "Sprzedaż") +
      ggplot2::theme_minimal()

    cli::cli_alert_success("Model PROPHET wygenerował prognozę!")
    return(p)

    # ==============================
    # METODA 3: XGBOOST (Machine Learning)
    # ==============================
  } else if (method == "xgboost") {

    # 1. Inżynieria Cech (Feature Engineering)
    df_xgb <- daily_data %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(
        month = as.numeric(format(date, "%m")),
        wday = as.numeric(format(date, "%u")), # Dzień tygodnia 1-7
        lag1 = dplyr::lag(sales, 1),           # Wczoraj
        lag7 = dplyr::lag(sales, 7)            # Tydzień temu
      ) %>%
      tidyr::drop_na()

    features <- c("lag1", "lag7", "month", "wday")
    X_train <- as.matrix(df_xgb[, features])
    y_train <- df_xgb$sales

    # 2. Trenowanie Modelu
    model_xgb <- xgboost::xgboost(
      data = X_train, label = y_train, nrounds = 100,
      objective = "reg:squarederror", verbose = 0
    )

    # 3. Predykcja Rekurencyjna (Krok po Kroku) w przyszłość
    cli::cli_alert_info("Generowanie prognozy rekurencyjnej (krok po kroku)...")
    full_data <- daily_data %>% dplyr::arrange(date)

    for (i in 1:horizon) {
      last_idx <- nrow(full_data)
      next_date <- full_data$date[last_idx] + 1

      X_next <- matrix(c(
        full_data$sales[last_idx],      # lag1
        full_data$sales[last_idx - 6],  # lag7
        as.numeric(format(next_date, "%m")),
        as.numeric(format(next_date, "%u"))
      ), nrow = 1)
      colnames(X_next) <- features

      next_pred <- stats::predict(model_xgb, X_next)
      next_pred <- max(0, next_pred) # XGBoost czasem przewiduje ujemnie, zabezpieczamy!

      full_data <- dplyr::bind_rows(full_data, data.frame(date = next_date, sales = next_pred))
    }

    # 4. Wykres z uwzględnieniem XGBoost (Zgodnie ze standardem z zajęć)
    history_data <- head(full_data, nrow(daily_data))
    forecast_data <- tail(full_data, horizon)

    p <- ggplot2::ggplot() +
      ggplot2::geom_line(data = history_data, ggplot2::aes(x = date, y = sales), color = "black", alpha = 0.6) +
      ggplot2::geom_line(data = forecast_data, ggplot2::aes(x = date, y = sales), color = "green", linewidth = 1.2) +
      ggplot2::labs(title = paste("Prognoza XGBOOST na", horizon, "dni"),
                    subtitle = "Czarna linia to historia, Zielona linia to predykcja",
                    x = "Data", y = "Całkowita sprzedaż") +
      ggplot2::theme_minimal()

    cli::cli_alert_success("Model XGBOOST wygenerował prognozę!")
    return(p)

  } else {
    stop("Nieznana metoda prognozowania. Wybierz 'arima', 'prophet' lub 'xgboost'.")
  }
}
