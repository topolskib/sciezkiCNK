#' Funkcja służy do rysowania scieżek z danych
#' 
#' @param data ramka danych z kolumnami tekstowymi from i to określającymi urządzenia początkowe i końcowe oraz numeryczną
#' kolumnętotal 
#' @param slownik ramka danych z kolumnami x i y określającymi współrzędne urządzenia i  kolumnę nr określającą 
#' numer urządzenia
#' @param col kolor linii
#' @param lwd szerekość linii
#' @param rozmiary  rozmiary mapy
#' @export


plot_paths2 <- function (data, slownik, col= "#160773", lwd = 3.5, rozmiary = c(1018, 886)) 
{
   require(dplyr)
   
   doRGB <- as.numeric(col2rgb(col))/255
   
   data <- data %>% filter(from != "---" & to != "---" & total > 0.002) %>%
      mutate(x_pocz = slownik$x[match(from, slownik$nr)],
             y_pocz = rozmiary[2] - slownik$y[match(from, slownik$nr)],
             x_kon = slownik$x[match(to, slownik$nr)],
             y_kon = rozmiary[2] - slownik$y[match(to, slownik$nr)],
             kolor = rgb(doRGB[1], doRGB[2], doRGB[3], alpha = total)) %>%
      filter(!is.na(x_pocz) & !is.na(x_kon))
   
   segments(x0 = data$x_pocz, y0 = data$y_pocz, 
            x1 = data$x_kon, y1 = data$y_kon, lwd = lwd, col = data$kolor)
}