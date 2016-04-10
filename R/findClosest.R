#' Funkcja zwraca informacje o stacji znajdującej się najbliżej kursora
#' 
#' 
#' @param coords Współrzędne kursora
#' 
#' @param slownik Słownik urządzeń
#' 
#' @export
#' 
findClosest <- function(coords, slownik){
  dists <- as.matrix(dist(rbind(coords,slownik[,4:5])))[-1,1]
  stacja <- which.min(dists)
  if(dists[stacja]<20) {return(c(paste0("Numer stacji: ", as.character(slownik$nr[stacja])),paste0( "Nazwa stacji: ",as.character(slownik$nazwa[stacja])  )))}
  else return(c("Numer stacji: ---", "Nazwa stacji: ---" ))
}


findClosest(c(150,150), slownik_urz)
