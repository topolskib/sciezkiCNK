#' @title Rysowanie połączeń (igraph) 
#' 
#' Funkcja służy do rysowania połączeń między danymi za pomocą dostosowanych strzałek z pakietu `igraph`.
#' 
#' @param data ramka danych o trzech kolumnach (from, to i total)
#' @param rozmiary = rozmiary mapy, na której rsyujemy 
#' @param alpha = wartosci, które posłużą do dobru przeźroczystości połączeń
#' @param slownik słownik ze współrzędnymi urządzeń i ich nazwami
#' @param czyStrzalki czy rysować kierunek połączenia
#' @param kolLinii kolor linii 
#' @param szerLinii szerokość linii 
#' @param krzywLinii krzywizna linii [0,1] 
#' @param rozmiarStrzalek rozmiar strzałek
#' @param szerStrzalek szerokość strzałek
#' @param typLinii typ linii (0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”,
#' 6 or “twodash”
#' @param etykLinii etykiety rysowane nad linią
#' @param etykLiniiRozmiar rozmiar tych etykiet
#' 
#' @export

plot_polaczenia_graph <- function(data, rozmiary = c(1018, 886), alpha, slownik, czyStrzalki = F,
                            kolLinii = "cyan3", 
                            szerLinii = 4, 
                            krzywLinii = 0.2, 
                            rozmiarStrzalek = 0.5,
                            szerStrzalek = 1, 
                            typLinii = "solid",
                            etykLinii = "", 
                            etykLiniiRozmiar = 1){
   require(igraph)
   
   # dbam o to by miec charactery zamiast factorow
   data[,"from"] <- as.character(data[,"from"])
   data[,"to"] <- as.character(data[,"to"])
   
   #wybieram wspolrzedne tylko tych urzadzen ze slownika, ktore sa w danych
   meta <- slownik[slownik$nr %in% c(data$from, data$to),c("nr","x", "y")]
   
   #poprawiam wspolrzedne y-owe
   meta$y <- rozmiary[2] - meta$y

   meta$nr <- as.character(meta$nr)
   
   #tworze strukture grafu
   g <- graph.data.frame(data, directed=czyStrzalki, vertices=meta)
   
   #definiuje sobie jak maja rysowane byc linie (edges)
   alpha <-(alpha/max(alpha))
   doRGB <- as.numeric(col2rgb(kolLinii))/255
   E(g)$color <- rgb(doRGB[1],doRGB[2],doRGB[3],alpha=alpha)
   E(g)$width <- szerLinii
   
   # rysuje graf, nakladajac go na juz istniejace mape
   lo <- as.matrix(meta[,c("x","y")])
   plot(g, layout = lo, rescale=F, add=T, 
        edge.curved=krzywLinii, 
        edge.arrow.size = rozmiarStrzalek,
        edge.arrow.width = szerStrzalek, 
        edge.lty = typLinii, 
        edge.label= etykLinii, 
        edge.label.cex = etykLiniiRozmiar,
        vertex.shape="none",
        vertex.label = "")
}
