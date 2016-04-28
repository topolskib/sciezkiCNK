#' @title Filtrowanie danych 
#' 
#' @description 
#' Funkcja służy do przetwarzania wybranych w opcjach aplikacji miesięcy i dni tygodnia do zbioru
#' dni rozumianych przez funkcję rysującą rysujBoxploty
#' 
#' @param weekdays wektor zawierający numeryczne wartości dni tygodnia,
#' które chcemy uwzględnić
#' 
#' @param months wektor zawierający numeryczne wartości miesięcy,
#' które chcemy uwzględnić
#' 
#' @return zwraca wektor z datami typu dzień_miesiac (np. 21_02)
#' 
#' @export

wybraneDni_filter_data <- function(weekdays = c(1, 7), months = c(1, 12)){
   
   daty <- seq.Date(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), 
                    by = 1)
   
   dni <- format(daty[as.numeric(format.Date(daty, format = "%u")) %in% 
                         weekdays], format = "%d_%m")
   
   dni <- dni[(as.numeric(stri_sub(dni, 4, 5)) %in% months)]
   
}