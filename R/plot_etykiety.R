#' @title Rysowanie etykiet z nazwami
#' 
#' @description 
#' Funkcja służy do rysowania etykiet z pełnymi nazwami eksponatów.
#' Funkcja wymaga uprzedniego wywołania plot.new (np. przez wywołanie funkcji plot_mapa)
#' 
#' @param dane_urz ramka danych z kolumnami x i y odpowiadającymi współrzędnym urządzeń oraz kolumną nazwa - opisem urządzenia
#' @param przes 2 - el. wektor przesunięcia etykiet, dodatnie wartości oznaczają odpowiednio: w lewo, w dół, ujemne prawo, góra
#' domyślnie etykiety są ustawione na środku linii łączącej punkty
#' @param rozmiary rozmiary planu, na który nanosimy punkty
#' @param col kolor tekstu etykiet
#' @param cex wielkość tekstu etykiet
#' 
#' @return 
#' Funkcja nanosi na istniejący wykres etykiety z nazwami eksponatów.
#' 
#' @export
plot_etykiety <- function(dane_urz, cex=0.7, col="black", rozmiary = c(1018, 886), przes = c(-40,40)){
   
   text( x = dane_urz$x - przes[1], y= rozmiary[2] - dane_urz$y - przes[2], labels = dane_urz$nazwa,  cex=cex, col = col ) 

}
