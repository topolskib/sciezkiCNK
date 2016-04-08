#' Funkcja służy do rysowania urzadzen 
#' @export

plot_urzadz <- function(dane_urz, pch=19, cex=0.5, col="black", rozmiary = c(1018, 886)){
  
  # lista_urz taka sama dlugosc jak
  
  
  points( x= dane_urz$x, y= rozmiary[2] - dane_urz$y, pch=pch, cex=cex, col = col ) 
  # gimp liczy wysokosc od gory, a R od dolu, wiec trzeba zrobic dla y poprawke
  
}