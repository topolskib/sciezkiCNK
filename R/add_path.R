#' Funkcja zwraca zbiorcza ramke danych ~paths uaktualniona o sciezke z ~newPath
#' 
#' @param newPath ramka danych z kolumnami $from, $to, $hour opisujaca poruszanie sie
#' konkretnego zwiedzajacego po galerii
#' @param paths ramka danych z informacjami o wszystkich przejsciach w galerii
#' 
#' @export

add_path <- function(newPath, paths){
  for(j in 1:nrow(newPath)){
    from = as.character(newPath[j, "from"])
    to = as.character(newPath[j, "to"])
    hour = as.character(newPath[j, "hour"])
    path_row <- which(paths$from == from & paths$to == to)
    if(hour %in% as.character(8:18)){
      paths[path_row, hour] <- paths[path_row, hour] + 1 
    }else{
      paths[path_row, "after 19"] <- paths[path_row, "after 19"] + 1      
    }
  }
  paths
}