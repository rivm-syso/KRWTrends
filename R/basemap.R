#' Maakt ggplot2 object met LMG basemap
#' 
#' Deze functie maakt een ggplot2 object met de basemap met
#' de grondwaterlichamen in Nederland. Ruimtelijke data kan
#' worden toegevoegd aan deze plot met de ggplot2 syntax.
#'
#' @return een ggplot2 object
#' 
#' @examples
#' 
#' p <- basemap()
#' p <- p + geom_sf(data=pts) 
#' 
#' @importFrom ggspatial annotation_scale annotation_north_arrow 
#' @importFrom ggplot2 geom_sf
#' @export

basemap <- function(){    

    map <- ggplot(data=provinciegrenzen) +
        geom_sf(data=grondwaterlichamen,aes(fill=naam),col=NA) +
        geom_sf(col="gray10",fill=NA) +
        theme(legend.position="none") +
        annotation_scale(location = "bl", width_hint = 0.4) +
        annotation_north_arrow(location = "tl", which_north = "true",
                               style = north_arrow_fancy_orienteering)

        return(map)
}

