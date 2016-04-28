#' @title Rysowanie ścieżek (igraph)
#' 
#' @description
#' Funkcja służy do rysowania scieżek z danych za pomocą pekietu `igraph`.
#' 
#' @param data ramka danych z kolumnami tekstowymi from i to określającymi urządzenia początkowe i końcowe oraz numeryczną 
#' kolumną total 
#' @param slownik ramka danych z kolumnami x i y określającymi współrzędne urządzeń i kolumną nr określającą numer urządzenia
#' 
#' @export

plot_paths_graph <- function(data, slownik){
   data %>% filter(from != "---" & to != "---") -> data
   plot_polaczenia_graph(data, alpha = data$total, slownik = slownik, etykLinii = data$total)
}