% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_data_byday.R
\name{filter_data_byday}
\alias{filter_data_byday}
\title{Selekcja danych}
\usage{
filter_data_byday(days, hours = c(9, 20))
}
\arguments{
\item{days}{Wektor zawierający skrajne wartości zakresu dni, które chcemy uwzględnić.}

\item{hours}{Wektor dwuelementowy zawierający informacje o przedziale godzin, który chcemy uwzględnić. Składa się z krańców żądanego zakresu.}
}
\value{
Funkcja zwraca ramkę danych powstałą po agregacji danych według argumentów wejściowych.
}
\description{
Funkcja służy do filtrowania danych po miesiącach, dniach tygodnia i godzinach. Operuje ona bezpośrednio na ramce dane_all znajdującej
się w pakiecie.
}

