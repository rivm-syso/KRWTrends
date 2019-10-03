#' Vervang waardes onder de detectielimiet. 
#' 
#' Help functie voor \code{lmgsubset} om waardes onder de 
#' detectielimiet te vervangen.
#' 
#' Deze functie vervangt waardes onder de detectielimiet. Deze 
#' waardes zijn gecodeerd als in de kolom detectielimiet door 1.
#' Om de detectielimet te vervangen voor de helft van de
#' oorspronkelijke waarde moet de vermenigvuldigingsfactor 0,5 zijn.
#' 
#' @param x vector met waardes
#' @param replaceval vermenigvuldigingsfactor om rapportagegrens waardes
#' te vervangen
#' @return een vector met waardes waar de detectielimiet is vervangen
#' 
#' @examples
#' data(lmg_reeks)
#' d <- replaceDL(lmg_reeks, 0.5)
#'
#' @export

replaceDL <- function(d, replaceval = 0.5) {
  
  # Check of er maar één reeks wordt ingevoerd om de DG aan te passen
  if(length(unique(d$putfilter)) > 1){
    stop("de reeks bevat meerdere putfilters,
         maar mag er maar 1 bevatten")
  }

  if(length(unique(d$parameter)) > 1){
    stop("de reeks bevat meerdere parameters, 
         maar mag er maar 1 bevatten")
  }
  
  laagste_waarneming <- d$waarde[d$detectielimiet == 0] %>% min(na.rm = TRUE) 
  hoogste_RG_onder_waarneming <- d$waarde[d$detectielimiet == 1 & d$waarde < laagste_waarneming] %>% max(na.rm = TRUE)
  
  d <- d %>% mutate(waarde = ifelse(waarde < laagste_waarneming & detectielimiet == 1, replaceval * hoogste_RG_onder_waarneming, waarde))

  return(d)
}


