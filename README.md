# Projekt: Analiza i Prognozowanie Sprzedaży

Pakiet R stworzony w ramach zaliczenia II semestru. System służy do kompleksowej analizy szeregów czasowych sprzedaży detalicznej, wykorzystując zaawansowane metody statystyczne i Machine Learning.

## 🚀 Główne Funkcjonalności
- **ETL & Walidacja**: Automatyczne wczytywanie i sprawdzanie jakości danych (`load_sales_data`, `validate_sales_ts`).
- **Analiza Biznesowa**: Wyliczanie metryk (zmienność, średnie kroczące) oraz generowanie raportów menedżerskich YoY (Year-over-Year).
- **Logika Filtrowania**: Funkcja wyższego rzędu pozwalająca na dynamiczną analizę konkretnych miast lub typów sklepów.
- **Prognozowanie (Etap 3)**: Trzy niezależne silniki predykcyjne:
  - **ARIMA**: Klasyczny model statystyczny.
  - **Prophet**: Model addytywny z uwzględnieniem kalendarza świąt.
  - **XGBoost**: Model uczenia maszynowego z inżynierią cech (lagi, sezonowość).

## 🛠️ Instalacja i Uruchomienie
1. Otwórz projekt w RStudio (plik `.Rproj`).
2. Otwórz skrypt `uruchamianie.R`.
3. Uruchom skrypt sekcja po sekcji, aby zobaczyć pełny workflow: od czyszczenia danych po wykresy prognoz.

## 📦 Wykorzystane technologie
Projekt wykorzystuje ekosystem `tidyverse` (`dplyr`, `ggplot2`), pakiety do analizy szeregów czasowych (`tsibble`, `fable`) oraz biblioteki ML (`xgboost`, `prophet`).
