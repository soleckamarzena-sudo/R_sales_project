#' Profesjonalny Raport Menedżerski (Executive Summary)
#'
#' Generuje zaawansowany raport biznesowy w standardzie rynkowym.
#' Opiera się na analizie Year-over-Year (YoY) - porównuje ostatnie 365 dni (L12M)
#' do poprzednich 365 dni (P12M). Obejmuje główne KPI, analizę sklepów,
#' dynamikę kategorii oraz skuteczność akcji promocyjnych.
#'
#' @param clean_data Oczyszczona ramka danych (np. wyjście z clean_sales_ts).
#' @param date_col Nazwa kolumny z datą.
#' @param sales_col Nazwa kolumny ze sprzedażą.
#' @param store_col Nazwa kolumny z ID sklepu.
#' @param category_col Nazwa kolumny z kategorią produktu.
#' @param promo_col Nazwa kolumny z informacją o promocjach.
#'
#' @return Zwraca listę z wnioskami (niewidoczną), drukując raport w konsoli.
#'
#' @importFrom dplyr filter group_by summarise arrange mutate if_else
#' @importFrom tidyr pivot_wider drop_na
#' @importFrom cli cli_h1 cli_h2 cli_bullets cli_text
#' @importFrom rlang sym :=
#' @export
create_management_summary <- function(clean_data,
                                      date_col = "date",
                                      sales_col = "sales",
                                      store_col = "store_nbr",
                                      category_col = "family",
                                      promo_col = "onpromotion") {

  # --- 1. DEFINICJA OKRESÓW BIZNESOWYCH (L12M vs P12M) ---
  max_date <- max(clean_data[[date_col]], na.rm = TRUE)
  l12m_start <- max_date - 365
  p12m_start <- l12m_start - 365

  # Odfiltrowanie tylko ostatnich 2 lat i otagowanie okresów
  biz_data <- clean_data %>%
    dplyr::filter(!!rlang::sym(date_col) > p12m_start) %>%
    dplyr::mutate(period = dplyr::if_else(!!rlang::sym(date_col) > l12m_start, "L12M", "P12M"))

  # Funkcja pomocnicza do ładnego formatowania waluty/liczb
  fmt <- function(x) formatC(x, format = "f", big.mark = " ", digits = 0)

  # --- 2. GLOBALNE KPI ---
  kpi <- biz_data %>%
    dplyr::group_by(period) %>%
    dplyr::summarise(total_sales = sum(!!rlang::sym(sales_col), na.rm = TRUE), .groups = "drop")

  sales_l12m <- kpi$total_sales[kpi$period == "L12M"]
  sales_p12m <- kpi$total_sales[kpi$period == "P12M"]

  if (length(sales_l12m) == 0) sales_l12m <- 0
  if (length(sales_p12m) == 0) sales_p12m <- 0

  yoy_growth <- ifelse(sales_p12m > 0, (sales_l12m - sales_p12m) / sales_p12m * 100, NA)

  # --- 3. ANALIZA SKLEPÓW (Top/Bottom Performers w L12M) ---
  store_perf <- biz_data %>%
    dplyr::filter(period == "L12M") %>%
    dplyr::group_by(!!rlang::sym(store_col)) %>%
    dplyr::summarise(total = sum(!!rlang::sym(sales_col), na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(total))

  top_store <- as.character(store_perf[[store_col]][1])
  bottom_store <- as.character(store_perf[[store_col]][nrow(store_perf)])

  # --- 4. DYNAMIKA KATEGORII (YoY) ---
  cat_perf <- biz_data %>%
    dplyr::group_by(!!rlang::sym(category_col), period) %>%
    dplyr::summarise(total = sum(!!rlang::sym(sales_col), na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = period, values_from = total, values_fill = list(total = 0)) %>%
    # Filtrujemy mikrokategorie (np. baza poniżej 1000 sprzedaży), aby uniknąć wzrostów rzędu 600000%
    dplyr::mutate(
      growth_pct = dplyr::if_else(P12M > 1000, (L12M - P12M) / P12M * 100, NA_real_)
    ) %>%
    tidyr::drop_na(growth_pct) %>%
    dplyr::arrange(dplyr::desc(growth_pct))

  top_cat <- as.character(cat_perf[[category_col]][1])
  top_cat_growth <- round(cat_perf$growth_pct[1], 1)

  worst_cat <- as.character(cat_perf[[category_col]][nrow(cat_perf)])
  worst_cat_drop <- round(cat_perf$growth_pct[nrow(cat_perf)], 1)

  # --- 5. SKUTECZNOŚĆ PROMOCJI (Promo Lift w L12M) ---
  promo_lift_pct <- NA
  promo_msg <- "Brak danych"

  if (promo_col %in% names(biz_data)) {
    promo_stats <- biz_data %>%
      dplyr::filter(period == "L12M") %>%
      dplyr::mutate(has_promo = dplyr::if_else(!!rlang::sym(promo_col) > 0, "Promo", "Regular")) %>%
      dplyr::group_by(has_promo) %>%
      dplyr::summarise(avg_daily = mean(!!rlang::sym(sales_col), na.rm = TRUE), .groups = "drop")

    sales_promo <- promo_stats$avg_daily[promo_stats$has_promo == "Promo"]
    sales_reg <- promo_stats$avg_daily[promo_stats$has_promo == "Regular"]

    if (length(sales_promo) > 0 && length(sales_reg) > 0 && sales_reg > 0) {
      promo_lift_pct <- round(((sales_promo - sales_reg) / sales_reg) * 100, 1)
      promo_msg <- paste0("Sprzedaż z promocją jest o ", promo_lift_pct, "% wyższa niż bez promocji.")
    }
  }

  # --- WIZUALIZACJA RAPORTU W KONSOLI ---
  cli::cli_h1("EXECUTIVE SUMMARY: {max_date - 365} do {max_date}")

  cli::cli_h2("1. KLUCZOWE WSKAŹNIKI (YoY)")
  cli::cli_bullets(c(
    "*" = "Przychód (L12M): {.val {fmt(sales_l12m)}} USD",
    "*" = "Przychód (P12M): {.val {fmt(sales_p12m)}} USD",
    "v" = "Dynamika YoY: {.strong {round(yoy_growth, 1)}%}"
  ))

  cli::cli_h2("2. WYDAJNOŚĆ LOKALIZACJI")
  cli::cli_bullets(c(
    "*" = "Lider obrotu: Sklep nr {.val {top_store}}",
    "*" = "Najsłabszy punkt: Sklep nr {.val {bottom_store}}"
  ))

  cli::cli_h2("3. DYNAMIKA ASORTYMENTU (YoY)")
  cli::cli_bullets(c(
    "^" = "Top Mover (wzrost): {.val {top_cat}} (+{top_cat_growth}%)",
    "x" = "Underperformer (spadek): {.val {worst_cat}} ({worst_cat_drop}%)"
  ))

  cli::cli_h2("4. RENTOWNOŚĆ AKCJI PROMOCYJNYCH")
  cli::cli_bullets(c(
    "!" = "Promo Lift: {promo_msg}"
  ))

  cat("\n") # Pusta linia na koniec dla czytelności

  return(invisible(list(
    yoy_growth = yoy_growth,
    top_store = top_store,
    top_cat = top_cat,
    promo_lift = promo_lift_pct
  )))
}
