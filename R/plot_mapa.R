#' Funkcja służy do rysowania planu wystawy Regeneracja
#' 
#' @param sciezka ścieżka dostępu do pliku .png z planem wystawy
#' @param obram  czy narysować prostokąt wokół planu (domyślnie FALSE)
#' @param tytul tytuł mapy (domyślnie "Plan wystawy REGENERACJA")
#' 
#' @return zwraca 2 - elem. wektor liczbowy z rozmiarami obrazu, którego użyliśmy
#' 
#' @export

plot_mapa <- function(sciezka, obram = FALSE, tytul = "Plan wystawy REGENERACJA" )
{
   require('png')
   png = readPNG(sciezka, native=T) 
   res = c(dim(png)[2] , dim(png)[1])
   plot(1,1,xlim=c(0,res[1]),ylim=c(0,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n',
        main=tytul)
   rasterImage(png,0,0,res[1],res[2] )
   
   if (obram){
      rect(0,0,res[1],res[2])
   }
   
   return(res)
}