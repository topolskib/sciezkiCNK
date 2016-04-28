#' @title Różnica między czasami
#'  
#' @description 
#' Funkcja wyliczająca róznicę między czasami.
#' 
#' @param czas_potem wektor czasów późniejszych
#' 
#' @param czas_przedtem wektor czasów wcześniejszych
#' 
#' @param units jednostki, w jakich ma być zwracana różnica czasów
#' 
#' @param format format w jakim podane są czasy
#' 
#' @return wektor z różnicami czasów w jednostkach określonych parametrem units.
#' 
#' @export


roznica_czasu <- function(czas_potem, czas_przedtem, units="secs", format="%H:%M:%S"){
   
   # jesli roznica czasu po a czasu przed jest ujemna, to minelismy polnoc po drodze
   roznica <- as.numeric(difftime(strptime(czas_potem,format = format ), 
                                  strptime(czas_przedtem,format = format ), 
                                  units=units))
   roznica[roznica <0] <- as.numeric(difftime(strptime(paste0("2013-01-02 ",czas_potem[roznica <0]),
                                                       format = paste0("%Y-%m-%d ",format) ), 
                                              strptime(paste0("2013-01-01 ",czas_przedtem[roznica <0]),
                                                       format = paste0("%Y-%m-%d ",format) ), 
                                              units=units))
   
   roznica
}