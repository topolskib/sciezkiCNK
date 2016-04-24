#' Funkcja służy do wyznaczani najbardziej popularnej scieżki
#' 
#' @param filtered_data Ramka danych powstała w wyniku użycia funkcji filter_data
#' 
#' @param n Długość ścieżki
#' 
#' 
#' 
#' @export

sciezka <- function(filtered_data, n, first)  {
  
  wynik <- character(n)
  wynik[1] <- first
  for(i in 1:(n-1)) {
    tpm <- filtered_data %>% filter(from == wynik[i], !(to %in% wynik[1:n]), (!to == "---"))
    wynik[i+1] <- as.character(tpm[which.max(tpm$total),2])
    if(wynik[i+1]=="---") break
  }
  wynik <-  wynik[(wynik!= "") & (wynik != "---")]
  data.frame(from =  wynik[-length(wynik)], to = wynik[-1], total = 1 )
}