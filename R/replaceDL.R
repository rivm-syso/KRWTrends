#' Vervang waardes onder de detectielimiet. 
#' 
#' Help functie voor \code{lmgsubset} om waardes onder de 
#' detectielimiet te vervangen.
#' 
#' Deze functie vervangt waardes onder de detectielimiet. Deze 
#' waardes zijn gecodeerd door 1 in de detectielimiet kolom.
#' Om de detectielimet te vervangen voor de helft van de
#' oorspronkelijke waarde moet de vermenigvuldigingsfactor 0,5 zijn.
#' 
#' @param d grondwaterdata
#' @param replaceval vermenigvuldigingsfactor om rapportagegrens waardes
#' te vervangen
#' @return grondwaterdata
#' 
#' @examples
#' data(grondwaterdata)
#' d <- replaceDL(grondwaterdata, 0.5)
#'
#' @export

replaceDL <- function(d, replaceval = 0.5) {
  
  d <- d %>% mutate(waarde = ifelse(detectielimiet == 1, replaceval * waarde, waarde))

  return(d)
}


