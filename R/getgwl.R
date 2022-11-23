#'Help functie voor een lijst van de grondwaterlichamen 
#'
#' @param data dataset met drempelwaardes. default is drempelwaardes dataset die
#' bij het pakket zit

#'@return een lijst van grondwaterlichaamnamen uit de LMG dataset
#'
#' @export

getgwl <- function(data=drempelwaarden) {
    x <- as.character(unique(data$gwbident))
    x <- na.omit(x)
    x <- sort(x)
    attributes(x) <- NULL
    return(x)
}

