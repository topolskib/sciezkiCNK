#' Funkcja służy do filtrowania danych po miesiącach, dniach tygodnia i godzinach.
#' 
#' @param days Wektor zawierający skrajne wartości zakresu dni, które chcemy uwzględnić
#' 
#' @param hours Wektor dwuelementowy zawierający informacje o przedziale godzin, który chcemy uwzględnić
#' 
#' 
#' @export

filter_data_byday <- function(days, hours=c(9,20)){
  dates <- seq.Date(from = as.Date(days[1]), to = as.Date(days[2]), by = 1)
  dates <- format(dates, format = "%d_%m")
  dates <- dates[dates %in% names(dane_all)]
  filtered <- dane_all[dates]
  suma <- 0
  print(length((filtered)))
  if(hours[1] == hours[2]) {
    for(i in 1:length(filtered)){
      suma <- suma + filtered[[i]][,(hours[1]-5)]
    }
    
  }
  else {
    for(i in 1:length(filtered)){
      suma <- suma + rowSums(filtered[[i]][,(hours[1]-5):(hours[2]-5)])
    }
  }
  wynik <- cbind(dane_all[[1]][,2:3],suma)
  names(wynik)[3] <- "total"
  wynik
}