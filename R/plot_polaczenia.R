#' Funkcja służy do rysowania połączeń między punktami
#' 
#' Funkcja wymaga uprzedniego wywołania plot.new (np. przez wywołanie funkcji plot_mapa)
#' 
#' @param dane_pocz ramka danych z kolumnami x i y odpowiadającymi współrzędnym początkowych urządzeń
#' @param dane_kon ramka danych z kolumnami x i y odpowiadającymi współrzędnym końcowych urządzeń
#' @param wartosci wektor o długości równej długości powyższych ramek danych z liczbami połączeń
#' @param rozmiary rozmiary planu, na który nanosimy punkty
#' @param col kolor linii
#' @param lwd grubość linii
#' 
#' @export



plot_polaczenia <- function(dane_pocz, dane_kon, wartosci, col="dodgerblue3",lwd = 3.5 ,rozmiary = c(1018, 886)){
   
   doRGB <- as.numeric(col2rgb(col))/255
   alfy <- wartosci/max(wartosci)
   segments(x0= dane_pocz$x, y0= rozmiary[2] - dane_pocz$y, x1= dane_kon$x, y1= rozmiary[2] - dane_kon$y, 
            lwd=lwd, col = rgb(doRGB[1],doRGB[2],doRGB[3],alpha=alfy))
   
}