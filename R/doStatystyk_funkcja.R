#' @title Tworzenie danych do statystyk
#' 
#' @description
#' Funkcja służy do tworzenia danych z długościami i czasami ścieżek dla każdego dnia
#' 
#' @param sciezka ścieżka dostępu do katalogu ze ścieżkami
#' 
#' @param out ścieżka do katalogu, w którym mają zostać zapisane dane (jako plik
#' doStatystyk.Rda)
#' 
#' @export

doStatystyk_funkcja <- function(sciezka, out){
   
   pliki <- list.files(sciezka,full.names = F)
   
   require(parallel)
   require(sciezkiCNK) # roznica_czasu potrzebna
   
   ile_rdzeni <- detectCores() - 1 
   klaster <-makeCluster(ile_rdzeni)
   
   clusterExport(klaster, c("sciezka", "pliki"))
   clusterEvalQ(klaster, library(sciezkiCNK))
   
   doStatystyk <- parLapply(klaster, pliki, function(i){
      
      dlugosci_sciezek <- numeric()
      czasy_sciezek <- numeric()
      najDlugosc <- character()
      najCzas <- character()
      
      plik <- file.path(sciezka, i)
      
      dane <- read.table(plik,header=T, sep=",", stringsAsFactors = F)
      
      bezPowtorzen <- dane[!duplicated(dane[,c("visitor_id","exhibit")]),]
      
      a <- table(bezPowtorzen$visitor_id)
      
      dlugosci_sciezek <- as.numeric(a)
      names(dlugosci_sciezek) <- names(a)
      czasy <- numeric(length(a))
      
      for(j in 1:length(a)){
         pocz <- min(format(dane[dane$visitor_id == names(a)[j],"begin_time"],format = "%H:%M:%S"))
         kon <- max(format(dane[dane$visitor_id == names(a)[j],"end_time"],format = "%H:%M:%S"))
         czasy[j] <- roznica_czasu(kon,pocz)
      }
      czasy_sciezek <- czasy
      names(czasy_sciezek) <- names(a)
      
      ktory <- names(which.max(dlugosci_sciezek))
      najDlugosc <- bezPowtorzen[bezPowtorzen$visitor_id == ktory, "exhibit"]
      ktoryCzas <- names(which.max(czasy_sciezek))
      najCzas <-  bezPowtorzen[bezPowtorzen$visitor_id == ktoryCzas, "exhibit"]
      
      wynik <- list(dlugosci_sciezek = dlugosci_sciezek, czasy_sciezek = czasy_sciezek,  
                    najDlugosc = najDlugosc, najCzas = najCzas)
      
   })
   
   stopCluster(klaster)
   
   nazwy <- stri_replace_first_fixed(pliki, pattern = ".txt", replacement = "")
   names(doStatystyk) <- nazwy
   
   
   save(doStatystyk, file=file.path(out, "doStatystyk.Rda"))
   
}


