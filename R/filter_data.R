#' Funkcja służy do filtrowania danych
#' @export

filter_data <- function(weekdays = c(1,7), months = c(1,12), hours=c(9,20)) {  
  require(dplyr)
  require(stringi)
  daty <- seq.Date(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), by = 1)
  days <- format(daty[as.numeric(format.Date(daty, format ="%u")) %in% weekdays], format = "%d_%m")
  filtered <- dane_all[days]
  filtered <- filtered[(as.numeric(stri_sub(names(filtered), 4,5)) %in% months)]
  suma <- 0
  print(length((filtered)))
  if(hours[1] == hours[2]) {
    for(i in 1:length(filtered)){
      suma <- suma + dane_all[[i]][,(hours[1]-5)]
    }
    
  }
  else {
    for(i in 1:length(filtered)){
      suma <- suma + rowSums(dane_all[[i]][,(hours[1]-5):(hours[2]-5)])
    }
  }
  wynik <- cbind(dane_all[[1]][,2:3],suma)
  names(wynik)[3] <- "total"
  wynik
}