#' Funkcja służy do rysowania etykiet z liczbami nad liniami łączącymi urządzenia
#' 
#' Funkcja wymaga uprzedniego wywołania plot.new (np. przez wywołanie funkcji plot_mapa)
#' 
#' @param dane_pocz ramka danych z kolumnami x i y odpowiadającymi współrzędnym początkowych urządzeń
#' @param dane_kon ramka danych z kolumnami x i y odpowiadającymi współrzędnym końcowych urządzeń
#' @param wartosci wektor o długości równej długości powyższych ramek danych z etykietami wyświetlanymi nad liniami
#' @param przes 2 - el. wektor przesunięcia etykiet, dodatnie wartości oznaczają odpowiednio: w lewo, w dół, ujemne prawo, góra
#' domyślnie etykiety są ustawione na środku linii łączącej punkty
#' @param rozmiary rozmiary planu, na który nanosimy punkty
#' @param col kolor tekstu etykiet
#' @param cex wielkość tekstu etykiet
#' 
#' @export

plot_liczby <- function(dane_pocz, dane_kon, wartosci, przes=c(-2,-5), cex=0.9, col="dodgerblue4", rozmiary = c(1018, 886)){
   
   diff_x <- (dane_kon$x + dane_pocz$x)/2
   diff_y <- (dane_kon$y + dane_pocz$y)/2
   text( x = diff_x - przes[1], y= rozmiary[2] - diff_y - przes[2], labels = wartosci,  cex=cex, col = col )
   
}

