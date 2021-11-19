#' Toets Statistiek
#'
#' Deze functie geeft een overzicht van de statistieken in een 
#' grondwaterdataset. Het 50ste en 95ste percentiel worden
#' berekend, waarbij zowel metingen <RG als erboven zijn meegenomen.
#' Daarnaast wordt het percentage metingen onder de rapportagegrens 
#' bepaald, met de laagste en hoogste rapportagegrens binnen de dataset.  
#'
#' @param d tijdreeks van waarnemingen voor 1 parameter en 1 diepte
#'
#'
#' @return een data.frame met de toets statistieken, de volgende
#' kolommen worden gegeven:
#'  \itemize{
#'    \item ntot totaal aantal gegeven meetwaarden zonder NA's
#'    \item P50 mediaan van meetwaarden
#'    \item P95 95ste percentiel van meetwaarden
#'    \item perc.RG percentage meetwaarden onder de rapportagegrens (RG)
#'    \item RG.min laagste rapportagegrens
#'    \item RG.max hoogste rapportagegrens
#'
#' }
#'
#' @export
#'


toetsStatistiek <- function(d) {
  
    testSerie(d)

    param <- d$parameter[1]
    nrm <- d$norm[1]
    if(is.na(nrm)){
        d %>% mutate(dwratio = 1) %>%
        drop_na(waarde) %>%
        summarise(ntot = n(),
                P50 = median(waarde),
                P95 = quantile(waarde, 0.95),
                perc.RG = round(nrow(d[d$detectielimiet > 0,]) / n() * 100, digits = 1),
                RG.min = min(waarde[detectielimiet > 0]),
                RG.max = max(waarde[detectielimiet > 0]),
                norm = nrm) 
    }else{
      d %>% mutate(dwratio = waarde / norm) %>%
            drop_na %>%
            summarise(ntot = n(),
                    P50 = median(waarde),
                    P95 = quantile(waarde, 0.95),
                    perc.RG = round(nrow(d[d$detectielimiet > 0,]) / n() * 100, digits = 1),
                    RG.min = min(waarde[detectielimiet > 0]),
                    RG.max = max(waarde[detectielimiet > 0]),
                    norm = nrm) 
     }
}




