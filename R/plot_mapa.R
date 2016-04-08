#' Funkcja służy do rysowania mapy
#' @export

plot_mapa <- function(png, obram = FALSE )
{
  require('png')
  
  res = c(dim(png)[2] , dim(png)[1])
  plot(1,1,xlim=c(0,res[1]),ylim=c(0,res[2]),asp=1,type='n',xaxs='i',yaxs='i',xaxt='n',yaxt='n',xlab='',ylab='',bty='n')
  rasterImage(png,0,0,res[1],res[2])
  
  if (obram){
    rect(0,0,res[1],res[2])
  }
  
  return(res)
}