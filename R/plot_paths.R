#' Funkcja służy do rysowania scieżek z danych
#' 
#' @param data ramka danych z kolumnami tekstowymi from i to określającymi urządzenia początkowe i końcowe oraz numeryczną 
#' kolumną total 
#' @param slownik ramka danych z kolumnami x i y określającymi współrzedne urządzeń i kolumną nr określającą numer urządzenia
#' 
#' @export


plot_paths <- function(data, slownik){
   rownames(slownik) <- slownik$nr
   data %>% filter(from != "---" & to != "---") -> data
   pocz <- slownik[data$from, 4:5]
   kon <- slownik[data$to, 4:5]
   print(head(pocz))
   plot_polaczenia(pocz, kon, wartosci = data$total)
}
