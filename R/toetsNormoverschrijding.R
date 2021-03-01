#' Toets Normoverschrijding
#'
#' Deze functie berekent het percentage van de  waarnemingen met
#' normoverschrijdingen. Hierbij worden alleen waardes >RG meegenomen.
#' Het berekent ook de gemiddelde concentratie van de putten met 
#' normoverschrijdingen en de gemiddelde ratio van 
#' concentratie (waarde) gedeeld door de norm.
#'
#' @param d tijdreeks van waarnemingen voor 1 parameter en 1 diepte
#' @param toetsnorm de relatieve norm waar tegen getoetst wordt.
#'
#' De normen zijn afhankelijk van het grondwaterlichaam waarvoor
#' getoetst wordt. De drempelwaarden per grondwaterlichaam zijn
#' gegeven in `data(drempelwaarden)`.
#'
#' De toetstnorm is de relatieve norm. Deze is standaard 0.75 om te
#' toetsen aan 75% van de norm. Als getoetst moet worden t.o.v. de 
#' actuele drempelwaarde, dan moet de toetsnorm op 1 gezet worden. 
#'
#' @return een data.frame met de toets statistieken, de volgende
#' kolommen worden gegeven:
#'  \itemize{
#'    \item n totaal aantal gegeven meetwaarden (>RG) boven de toetsnorm
#'    \item percentage percentage boven de toetsnorm
#'    \item gem.waarde gemiddelden van waarden boven de toetsnorm
#'    \item gem.ratio gemiddelde ratio van waarde/norm
#' }
#'
#' @export
#'



toetsNormoverschrijding <- function(d, toetsnorm = 0.75) {

    testSerie(d)
    param <- d$parameter[1]

    n.tot <- nrow(d)
    d %>% filter(detectielimiet == 0) %>% 
        mutate(dwratio = waarde / norm) %>%
        filter(dwratio > toetsnorm) %>%
        summarise(n = n(),
                  percentage = round(n() / n.tot * 100, digits = 1),
                  gem.waarde = mean(waarde),
                  gem.ratio = mean(dwratio)
                  ) 

}


