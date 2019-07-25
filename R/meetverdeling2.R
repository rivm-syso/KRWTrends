#' Toets Meetverdeling2
#'
#' Deze functie bepaalt de meetverdeling door de jaren voor de
#' verschillende KRW planperiodes op putfilterniveau
#'
#'
#' @param d tijdreeks van waarnemingen voor 1 parameter en 1 diepte
#'
#'
#' @return een data.frame met het aantal metingen per KRW planperiode
#' om te toetsen aan de verdeling van het aantal metingen, de volgende
#' kolommen worden gegeven:
#'  \itemize{
#'    \item aantal.metingen aantal metingen per planperiode
#'    \item totaal totaal aantal metingen per put
#' }
#'
#' @export
#'


meetverdeling2 <- function(d, param, diep) {

  d <- d %>%
    filter(parameter == param, diepte == diep) %>%
    droplevels()
  
  freq <- as.data.frame(table(d$putfilter, d$meetjaar)) %>%
    spread(key = Var2, value = Freq) 
  #%>%
  #  mutate(totaal = rowSums(freq[, c(2:23)]))
}  
