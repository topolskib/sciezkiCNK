#' Funkcja tworzy ramke danych ze wszystkimi mozliwymi przejsciami miedzy eksponatami
#' 
#' @param allExhibits wektor character numerow eksponatow
#' 
#' @export

create_emptyPaths <- function(allExhibits){
  emptyPaths <- data.frame(from = "---", to = "---")
  for(i in 1:length(allExhibits)){
    paths <- data.frame(from = c("---", rep(allExhibits[i], 58)), to = c(allExhibits[i], allExhibits[-i], "---"))
    emptyPaths <- rbind(emptyPaths, paths)
  }
  emptyPaths <- emptyPaths[-1,]
  hours <- c(8:18, "after 19")
  emptyPaths[,hours] <- 0
  emptyPaths
}



