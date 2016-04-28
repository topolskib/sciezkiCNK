#' @title Nanoszenie urządzeń
#'
#' @description
#' Funkcja służy do nanoszenia na mapę punktów odpowiadających urządzeniom. 
#' Funkcja wymaga uprzedniego wywołania plot.new (np. przez wywołanie funkcji plot_mapa)
#' 
#' @param dane_urz ramka danych z kolumnami x i y odpowiadającymi współrzędnym urządzeń
#' @param pch rodzaj znacznika urządzeń
#' @param rozmiary rozmiary planu, na który nanosimy punkty
#' @param col kolor znacznika
#' @param cex wielkość znacznika
#' 
#' @export
plot_urzadz <- function(dane_urz,  pch=19, cex=2.5, col="firebrick2", rozmiary = c(1018, 886)){

   points( x= dane_urz$x, y= rozmiary[2] - dane_urz$y, pch=pch, cex=cex, col = col ) 
   
}