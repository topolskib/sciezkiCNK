#' Funkcja służy do rysowania polaczen
#' @export

plot_polaczenia <- function(dane_pocz, dane_kon, wartosci=0, col="black", alpha = 0.2 ,lwd = 2 ,rozmiary = c(1018, 886)){
  
  
  doRGB <- as.numeric(col2rgb(col))
  segments(x0= dane_pocz$x, y0= rozmiary[2] - dane_pocz$y, x1= dane_kon$x, y1= rozmiary[2] - dane_kon$y, 
           lwd=lwd, col = rgb(doRGB[1],doRGB[2],doRGB[3],alpha=alpha))
  
}