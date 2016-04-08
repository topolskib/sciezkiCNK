#' Funkcja służy do rysowania etykiet
#' @export

plot_etykiety <- function(dane_urz, przes= c(0,0), cex=0.5, col="black", rozmiary = c(1018, 886)){
  
  # lista_urz taka sama dlugosc jak
  text( x = dane_urz$x - przes[1], y= rozmiary[2] - dane_urz$y - przes[2], labels = dane_urz$nazwa,  cex=cex, col = col ) 
  # gimp liczy wysokosc od gory, a R od dolu, wiec trzeba zrobic dla y poprawke
  
}