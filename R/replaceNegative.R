#' Vervang waardes onder de detectielimiet. 
#' 
#' Help functie voor \code{lmgsubset} om waardes onder de 
#' detectielimiet te vervangen.
#' 
#' Deze functie vervangt waardes onder de detectielimiet. Deze 
#' waardes zijn gecodeerd als negatieve concentraties in de LMG
#' dataset. Om de detectielimet te vervangen voor de helft van de
#' oorspronkelijke waarde moet de vermenigvuldigingsfactor 0,5 zijn.
#' 
#' @param x vector met waardes
#' @param replaceval vermenigvuldigingsfactor om negatieve waardes
#' te vervangen
#' @return een vector met waardes waar de detectielimiet is vervangen
#' 
#' @examples
#' data(lmg)
#' x<-replaceNegative(lmg$no3,-0.5)
#'
#' @export

replaceNegative <- function(x,replaceval=NA) {

    d <- ifelse(x<0,-1*x*replaceval,x)
    return(d)
}


