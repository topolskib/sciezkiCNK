#' Funkcja służy do rysowania scieżek z danych
#' @export

plot_paths <- function(data, slownik, alpha_mod = 1){
  rownames(slownik) <- slownik$nr
  data %>% filter(from != "---" & to != "---") -> data
  alpha <- data$total / max(data$total)
  pocz <- slownik[data$from, 4:5]
  kon <- slownik[data$to, 4:5]
  print(head(pocz))
  plot_polaczenia(pocz, kon, alpha = alpha^alpha_mod)
}
