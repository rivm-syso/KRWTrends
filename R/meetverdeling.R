#' Toets Meetverdeling
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


meetverdeling <- function(d) {
  
  d <- d[order(d$meetjaar),] 
  n.tot <- nrow(d)
  d %>% mutate(tijdsperiode = ifelse(meetjaar > 2003 & meetjaar < 2010, "1e planperiode KRW (2004-2009)",
                                     ifelse(meetjaar > 2009 & meetjaar < 2016, "2e planperiode KRW (2010-2015)",
                                            ifelse(meetjaar > 2015, "3e planperiode KRW (2016-2021)", "Eerdere meting")))) %>%
    group_by(tijdsperiode) %>%
    summarise(aantal.metingen = n(),
              totaal = n.tot)
}
    
    
