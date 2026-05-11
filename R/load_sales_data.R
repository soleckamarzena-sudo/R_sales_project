library(magrittr)

#' Wczytywanie zbiorów danych sprzedażowych
#'
#' Funkcja ładuje z podanego folderu trzy kluczowe pliki:
#' train.csv, stores.csv oraz holidays_events.csv. Od razu odpowiednio formatuje
#' ich kolumny
#'
#' @param data_dir Ścieżka do folderu, w którym znajdują się pobrane pliki CSV.
#'
#' @return Zwraca listę (list) zawierającą trzy obiekty typu \code{tibble}:
#' \itemize{
#'   \item \code{train} - główne dane sprzedażowe,
#'   \item \code{stores} - metadane sklepów,
#'   \item \code{holidays} - informacje o świętach.
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr mutate across
#' @importFrom cli cli_alert_success cli_alert_danger cli_alert_warning cli_bullets
#' @export
load_sales_data <- function(data_dir) {

  # Przygotowujemy pustą listę na zbiory
  datasets <- list()

  # 1. Wczytywanie TRAIN
  path_train <- file.path(data_dir, "train.csv")
  if (file.exists(path_train)) {
    datasets$train <- readr::read_csv(path_train, show_col_types = FALSE) %>%
      dplyr::mutate(
        date = as.Date(date),
        store_nbr = as.factor(store_nbr),
        family = as.factor(family)
      )
  } else {
    cli::cli_alert_danger("Nie znaleziono pliku train.csv w folderze {data_dir}.")
  }

  # 2. Wczytywanie STORES
  path_stores <- file.path(data_dir, "stores.csv")
  if (file.exists(path_stores)) {
    datasets$stores <- readr::read_csv(path_stores, show_col_types = FALSE) %>%
      dplyr::mutate(
        dplyr::across(c(store_nbr, city, state, type, cluster), as.factor)
      )
  } else {
    cli::cli_alert_warning("Nie znaleziono pliku stores.csv.")
  }

  # 3. Wczytywanie HOLIDAYS_EVENTS
  path_holidays <- file.path(data_dir, "holidays_events.csv")
  if (file.exists(path_holidays)) {
    datasets$holidays <- readr::read_csv(path_holidays, show_col_types = FALSE) %>%
      dplyr::mutate(
        date = as.Date(date),
        type = as.factor(type),
        locale = as.factor(locale),
        transferred = as.logical(transferred)
      )
  } else {
    cli::cli_alert_warning("Nie znaleziono pliku holidays_events.csv.")
  }

  # Podsumowanie i zwrot danych
  if (length(datasets) > 0) {
    cli::cli_alert_success("Pomyślnie wczytano zbiory danych z folderu: {data_dir}")
    cli::cli_bullets(c("*" = "Wczytane tabele: {paste(names(datasets), collapse = ', ')}"))
  } else {
    stop("Błąd: Nie wczytano żadnego pliku. Upewnij się, że podałeś poprawną ścieżkę do folderu z danymi.")
  }

  return(datasets)
}

