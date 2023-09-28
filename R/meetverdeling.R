#' Toets Meetverdeling
#'
#' Deze functie bepaalt de meetverdeling door de jaren voor de
#' verschillende KRW planperiodes op putfilterniveau
#'
#'
#' @param d tijdreeks van waarnemingen voor 1 parameter en 1 diepte
#' @param jaren_overlap integer voor het aantal jaren overlap. staat standaard
#' op 0
#'
#'
#' @return een data.frame met het aantal metingen per KRW planperiode
#' om te toetsen aan de verdeling van het aantal metingen, de volgende
#' kolommen worden gegeven:
#'  \itemize{
#'    \item aantal_metingen aantal metingen per planperiode
#'    \item totaal totaal aantal metingen per put
#' }
#' 
#' Als blijkt dat veel reeksen afvallen o.b.v. de meetverdeling dan kan ervoor 
#' gekozen worden na te gaan of de gewenste meetverdeling wel wordt gehaald als 
#' een jaar overlap wordt toegestaan tussen de verschillende planperiodes. Een 
#' jaar overlap houd bijvoorbeeld in: planperiode 2004-2009 wordt periode 
#' 2003-2010, planperiode 2010-2015 wordt periode 2009-2016 en zo voort.
#' 
#' in het geval dat gekozen is voor een jaren overlap > 0 wordt in plaast van
#' aantal metingen per plan periode en totaal aantal metingen een kolom gegeven
#' met voldoende metingen TRUE of FALSE omdat het overzicht met de indeling
#' anders niet eenvoudig duidelijk te formuleren is.
#'
#' @export
#'


meetverdeling <- function(d, jaren_overlap = 0) {

  if(jaren_overlap == 0) {
    
    d <- d[order(d$meetjaar),] 
    n.tot <- nrow(d)
    res <- d %>% mutate(tijdsperiode = case_when(meetjaar > 2003 & meetjaar < 2010 ~ "1e planperiode KRW (2004-2009)",
                                                 meetjaar > 2009 & meetjaar < 2016 ~ "2e planperiode KRW (2010-2015)",
                                                 meetjaar > 2015 & meetjaar < 2025 ~ "3e planperiode KRW (2016-2024)",
                                                 meetjaar > 2024 ~ "Meting na 2024",
                                                 TRUE ~ "Eerdere meting")) %>%
      group_by(tijdsperiode) %>%
      summarise(aantal_metingen = n(),
                totaal = n.tot)
    
  }  
  
  if(jaren_overlap > 0) {
    
    jo <- as.integer(jaren_overlap)
    
    periode1 <- (2004-jo):(2009+jo)
    periode2 <- (2010-jo):(2015+jo)
    periode3 <- (2016-jo):(2024+jo)
    
    d <- d %>% filter(meetjaar >= min(periode1))
    
    min_p1 <- d %>% slice_min(order_by = meetjaar, n = 1)
    d <- d %>% slice_max(order_by = meetjaar, n = nrow(d)-nrow(min_p1))
    
    max_p3 <- d %>% slice_max(order_by = meetjaar, n = 2)
    d <- d %>% slice_min(order_by = meetjaar, n = nrow(d)-nrow(max_p3))
    
    res <- data.frame(tijdsperiode = c("1e planperiode KRW (2004-2009)",
                                       "2e planperiode KRW (2010-2015)",
                                       "3e planperiode KRW (2016-2024)"),
                      voldoende_metingen = c(all(min_p1$meetjaar %in% periode1),
                                             sum(d$meetjaar %in% periode2) >= 2,
                                             all(max_p3$meetjaar %in% periode3)))
    
  }
  
  if(jaren_overlap < 0) {
    warning("Negatieve overlap niet ondersteund")
  } 
  
  return(res)
  
}
    
    
