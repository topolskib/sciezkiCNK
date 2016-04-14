#' Funkcja przetwarza pliki z informacjami o zalogowaniach danego dnia na ramki danych ze
#' zliczonymi sciezkami i wynikowe pliki zapisuje w podanym folderze
#' 
#' @param rawDataFolder sciezka dostepu do folderu zawierajacego pliki do obrobki 
#' @param returnFolder sciezka dostepu do folderu, w ktorym maja byc zapisane pliki wynikowe
#' @param allExhibits wektor character numerow wszystkich urzadzen
#' 
#' @export

create_dataFiles <- function(allExhibits, rawDataFolder, returnFolder){
  filenames <- list.files(rawDataFolder)
  emptyPaths <- create_emptyPaths(allExhibits)

  for(filename in filenames){
    data <- read.table(file.path(rawDataFolder, filename), sep = ",", header = TRUE)[,-1]
    visitors <- unique(data$visitor_id)
    paths <- emptyPaths
  
    for(visitorId in visitors){    
      newPath <- visitor_path(data, visitorId)
      paths <- add_path(newPath, paths)
    }
    
    write.csv(paths, file = file.path(returnFolder, filename))
  } 
}