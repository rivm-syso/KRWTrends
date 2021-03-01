#' Maakt een overzicht van de bemonsteringsfrequentie
#' 
#' Per grondwaterlichaam en filterdiepte wordt een overzicht gemaakt
#' van het aantal filters per aantal bemonsteringen. Dit gebeurt op 
#' het niveau van één vaak bemonsterde stof, bijvoorbeeld no3.
#' 
#' 
#' @param x grondwaterdataset.
#'
#' @return data.frame met bemonsteringsfrequenties
#' 
#' @export


monsterFreq <- function(x, param) {
  
    d <- x %>% filter(parameter == param) %>%
    group_by(grondwaterlichaam, diepte) %>%
    count(putfilter) %>%
    group_by(grondwaterlichaam, diepte, aantal.metingen = n) %>%
    dplyr::summarise(aantal.putten = length(unique(putfilter))) %>%
    spread(key = aantal.metingen, value = aantal.putten, fill = 0) 
    
    bereik <- 1:rev(names(d))[1]
    x <- bereik[!bereik %in% names(d)]
    
    d[, as.character(x)] <- 0
    d <- d %>% dplyr::select(grondwaterlichaam, diepte, as.character(1:max(bereik)))

    return(d)
}
  
  
  
  
  
  
  
