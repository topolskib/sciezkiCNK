#' Funkcja wybiera sciezki dla zwiedzajacego (usuwam powroty do tego samego eksponatu) i
#' zwraca ramke danych z informacjami o przejsciach miedzy eksponatami z ich godzina
#' 
#' @param data ramka danych z informacjami o wszystkich zalogowaniach do eksponatow
#' danego dnia
#' @param visitorId identyfikator odwiedzajacego
#' 
#' @export

visitor_path <- function(data, visitorId){  
  tmp <- data[data$visitor_id == visitorId,]
  from <- c("---", as.character(tmp$exhibit))
  to <- c(as.character(tmp$exhibit), "---")
  hour <- c(stri_sub(tmp$begin_time[1], from = 1, to = 2), stri_sub(tmp$end_time, from = 1, to = 2)) 
  new_path <- data.frame(from = from, to = to, hour = hour)[from != to,]
  new_path
}



