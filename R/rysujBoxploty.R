#' @title Rysowanie boxplotów
#' 
#' @description 
#' Funkcja służy do rysowania 3-częściowego wykresu (boxplot z rozkładem 
#' długości pojedynczej ścieżki, boxplot z rozkładem czasu pojedynczej ścieżki
#' i pusty wykres z informacjami o średniej dla wybranych wielkości
#' 
#' @param dni wektor dni, których rozkład ma być wyrysowany
#' 
#' @param summary_dl wektor z charakterystykami rozkładu dla długości scieżek
#' dla okresu, z którym będziemy porównujemy (min, Ikwartyl, mediana, średnia, IIIkwartyl, max)
#' 
#' @param summary_czas wektor z charakterystykami rozkładu dla czasu scieżek
#' dla okresu, z którym będziemy porównujemy (min, Ikwartyl, mediana, średnia, IIIkwartyl, max)
#' 
#' @export

rysujBoxploty <- function(dni, summary_dl = c(min =1, low=3, med=7, mean=8.304, upper=12,max=53),
                          summary_czas = c(min=0, low=578, med=1582, mean=2353, upper=3076,max=36220)){
   
   
   require(ggplot2)

   dane <- doStatystyk[dni]
   
   if(length(dni) == 1){
      dane_dlug <- summary(dane[[1]]$dlugosci_sciezek)
      dane_czasy <- summary(dane[[1]]$czasy_sciezek)
   }else{
      dane_dlug <- summary(unlist(sapply(dane, "[[", 1)))
      dane_czasy <- summary(unlist(sapply(dane, "[[", 2)))
   }
   
   DF_dlug <- data.frame(x=factor(c("Wybrany okres","2013"),
                                  levels= c("Wybrany okres","2013")), 
                         min=c(0,summary_dl["min"]), low=c(0,summary_dl["low"]), mid=c(0,summary_dl["med"]), 
                         top=c(0,summary_dl["upper"]), max=c(0,summary_dl["max"]))
   
   DF_dlug[1,2:6] <-c(dane_dlug[1], dane_dlug[2],dane_dlug[3],dane_dlug[5],dane_dlug[6])
   
   DF_czas <- data.frame(x=factor(c("Wybrany okres","2013"),
                                  levels= c("Wybrany okres","2013")), 
                         min=c(0,summary_czas["min"]), low=c(0,summary_czas["low"]), mid=c(0,summary_czas["med"]), 
                         top=c(0,summary_czas["upper"]), max=c(0,summary_czas["max"]))
   
   DF_czas[1,2:6] <-c(dane_czasy[1], dane_czasy[2],dane_czasy[3],dane_czasy[5],dane_czasy[6])
   
   p_dl <- ggplot(DF_dlug, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
      geom_boxplot(stat = "identity", colour="#556270",fill="#4ECDC4") + ggtitle("Rozkład długości pojedynczej ścieżki") +
      xlab("") + ylab("Ilość urządzeń")+ coord_cartesian(ylim = c(0, 30)) +
      theme(axis.ticks=element_blank(), axis.text=element_text(size=12), axis.title = element_text(size=14))+
      annotate("text", label = paste0("max: ", dane_dlug[6]), x = 1, y = 30, size = 4, colour = "black")   +
      annotate("text", label = paste0("max: ",summary_dl["max"]), x = 2, y = 30, size = 4, colour = "black")
   
   
   p_czas <- ggplot(DF_czas, aes(x=x, ymin = min, lower = low, middle = mid, upper = top, ymax = max)) +
      geom_boxplot(stat = "identity",colour="#DB0A5B",fill="#EBC2C2")+ ggtitle("Rozkład czasu pojedynczej ścieżki") +
      xlab("") + ylab("Czas w sek.")+ coord_cartesian(ylim = c(0, 5000)) +
      theme(axis.ticks=element_blank(), axis.text=element_text(size=12), axis.title = element_text(size=14)) +
      annotate("text", label = paste0("max: ", dane_czasy[6]), x = 1, y = 5000, size = 4, colour = "black")   +
      annotate("text", label = paste0("max: ",summary_czas["max"]), x = 2, y = 5000, size = 4, colour = "black")
   
   # pusty wykres
   df <- data.frame()
   
   dod <-ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 5000) +
      theme( panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             panel.background = element_blank(),
             panel.border=element_blank(),
             axis.text.y=element_blank(),axis.ticks=element_blank(),
             axis.text.x=element_blank()) + xlab("") + ylab("")+
      annotate("text", label = paste0("liczba wybranych dni: ",length(dni), " z 324"), x = 5, y = 5000, size = 5, colour = "black") +
      annotate("text", label = paste0("średnia długość ścieżki w 2013: ", round(summary_dl["mean"],2)), x = 5, y = 4500, size = 5,
               colour = "black") +
      annotate("text", label = paste0("średnia długość ścieżki w wybranym okresie: ", round(dane_dlug[4],2)), x = 5, y = 4000, 
               size = 5, colour = "black") +
      annotate("text", label = paste0("średni czas ścieżki w 2013: ", round(summary_czas["mean"]/60,2)," min"), x = 5, y = 3500, 
               size = 5, colour = "black") +
      annotate("text", label = paste0("średni czas ścieżki w wybranym okresie: ", round(dane_czasy[4]/60,2)," min"), x = 5, 
               y = 3000, size = 5, colour = "black")
   
   
   multiplot(p_dl,p_czas,dod , cols=3)
}