#'Help functie voor een lijst van de grondwaterlichamen 
#'@return een lijst van grondwaterlichaamnamen uit de LMG dataset
#'
#' @export

getgwl <- function() {
    x <- as.character(unique(drempelwaarden$gwbident))
    x <- na.omit(x)
    x <- sort(x)
    attributes(x) <- NULL
    return(x)
}

