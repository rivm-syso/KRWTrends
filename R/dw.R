#' Selecteer drempelwaardes (DW)
#'
#' Selecteer drempelwaardes (limit values)
#'
#' @param param parameter naam (als string)
#' @param gwl grondwaterlichaam (groundwaterbody)

#' @return drempelwaarde per stof per grondwaterlichaam 
#'
#' @examples
#' get list of groundwater bodies
#' getgwl()
#' dw("no3","NLGW0012")
#'
#' @export



dw <- function(param,gwl) {
    data(drempelwaarden)
    x <- subset(drempelwaarden,select=param,gwbident==gwl)[[param]]
    if(length(x)==0) {
        warning("dw: no value found")
        x <- NA
    }

    return(x)
}


