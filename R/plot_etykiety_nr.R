#' @title Rysowanie etykiet z numerami
#' 
#' @description 
#' Funkcja służy do rysowania etykiet z numerami urządzeń.
#' Funkcja wymaga uprzedniego wywołania plot.new (np. przez wywołanie funkcji plot_mapa).
#' 
#' @param dane_urz ramka danych z kolumnami x i y odpowiadającymi współrzędnym urządzeń oraz kolumną nr - numerem urządzenia
#' @param przes 2 - el. wektor przesunięcia etykiet, dodatnie wartości oznaczają odpowiednio: w lewo, w dół, ujemne prawo, góra
#' domyślnie etykiety są ustawione na środku linii łączącej punkty
#' @param rozmiary rozmiary planu, na który nanosimy punkty
#' @param col kolor tekstu etykiet
#' @param cex wielkość tekstu etykiet
#' 
#' @return 
#' Funkcja nanosi na istniejący wykres etykiety z numerami eksponatów.
#' 
#' @export

plot_etykiety_nr <- function(dane_urz, przes= c(0,0), cex=0.7, col="white", rozmiary = c(1018, 886)){
   
   text( x = dane_urz$x - przes[1], y= rozmiary[2] - dane_urz$y - przes[2], labels = dane_urz$nr,  cex=cex, col = col ) 
   
}
