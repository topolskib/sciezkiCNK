#' @title Filtrowanie danych 
#' 
#' @description 
#' Funkcja służy do przetwarzania wybranych w opcjach aplikacji dni do zbioru
#' dni rozumianych przez funkcję rysującą rysujBoxploty
#' 
#' @param days wektor zawierający skrajne wartości zakresu dni, które chcemy uwzględnić
#' 
#' @return zwraca wektor z datami typu dzień_miesiac (np. 21_02)
#' 
#' @export

wybraneDni_filter_data_byday <- function(days){
   
   dates <- seq.Date(from = as.Date(days[1]), to = as.Date(days[2]), 
                     by = 1)
   
   dates <- format(dates, format = "%d_%m")
   
   dates <- dates[dates %in% names(dane_all)]
   
}