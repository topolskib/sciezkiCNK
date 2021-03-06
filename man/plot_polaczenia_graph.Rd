% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_polaczenia_graph.R
\name{plot_polaczenia_graph}
\alias{plot_polaczenia_graph}
\title{Rysowanie połączeń (igraph) 

Funkcja służy do rysowania połączeń między danymi za pomocą dostosowanych strzałek z pakietu `igraph`.}
\usage{
plot_polaczenia_graph(data, rozmiary = c(1018, 886), alpha, slownik,
  czyStrzalki = F, kolLinii = "cyan3", szerLinii = 4, krzywLinii = 0.2,
  rozmiarStrzalek = 0.5, szerStrzalek = 1, typLinii = "solid",
  etykLinii = "", etykLiniiRozmiar = 1)
}
\arguments{
\item{data}{ramka danych o trzech kolumnach (from, to i total)}

\item{rozmiary}{= rozmiary mapy, na której rsyujemy}

\item{alpha}{= wartosci, które posłużą do dobru przeźroczystości połączeń}

\item{slownik}{słownik ze współrzędnymi urządzeń i ich nazwami}

\item{czyStrzalki}{czy rysować kierunek połączenia}

\item{kolLinii}{kolor linii}

\item{szerLinii}{szerokość linii}

\item{krzywLinii}{krzywizna linii [0,1]}

\item{rozmiarStrzalek}{rozmiar strzałek}

\item{szerStrzalek}{szerokość strzałek}

\item{typLinii}{typ linii (0 or “blank”, 1 or “solid”, 2 or “dashed”, 3 or “dotted”, 4 or “dotdash”, 5 or “longdash”,
6 or “twodash”}

\item{etykLinii}{etykiety rysowane nad linią}

\item{etykLiniiRozmiar}{rozmiar tych etykiet}
}

