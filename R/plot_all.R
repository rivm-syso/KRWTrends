#' Maakt tijdreeksgrafieken van de gegevens
#' 
#' De data van de \code{lmgsubset} wordt omgezet in een tijdreeks,
#' uitbijters worden verwijderd met \code{rmoutlier}. Elke reeks wordt geplot,
#' ongeacht het aantal metingen.
#' 
#' @param i put/filter naam waarvoor de statistiek berekend wordt
#' @param x data.frame van lmgsubset
#' @param trim als TRUE, dan worden uitbijters verwijderd (trimmed)
#' @param trimFactor factor van \code{rmoutlier}
#' @param dw.plot geeft aan om de relevante drempelwaarde in de grafiek wordt geplot
#' @param rpDL of de rapportagegrenzen aangepast moeten worden, standaard op TRUE.
#'
#' @return een plot object
#' 
#' @export
#' 

plot_all  <-  function (i, x, trim = FALSE, trimfactor = 1.5, dw.plot = TRUE, 
                        rpDL = TRUE, replacefactor = 0.5) {

    dw <- x$norm[1]
    param <- x$parameter[1]
    d <- x %>% select(putfilter, meetjaar, waarde, detectielimiet, eenheid) %>% 
        mutate(jr = meetjaar - min(meetjaar)) %>% 
        arrange(jr) %>% 
        filter(putfilter == i)

    d <- na.omit(d)

    if (rpDL) {
        d <- d %>% replaceDL(replaceval = replacefactor)
    }
    if (trim) {
        d <- mutate(d, waarde = rmoutlier(d[["Waarde"]], 
                                          factor = trimfactor, na.rm = TRUE))
    }

    d <- na.omit(d)
    n <- nrow(d)
    d <- d %>% mutate(detectielimiet = ifelse(detectielimiet == 
                                              1, "< RG", "waarneming"))

    p <- ggplot(d, aes(jr, waarde, colour = detectielimiet))
    p <- p + geom_line(colour = "grey")
    #p <- p + geom_point(aes(shape = instantie)) + scale_shape_manual(values=c(16, 17))
    if (dw.plot) {
        p <- p + geom_hline(aes(yintercept = dw, linetype = "drempelwaarde"), 
                            colour = "red")
        p <- p + geom_hline(aes(yintercept = 0.75 * dw, linetype = "75% drempelwaarde"), 
                            colour = "orange")
    }
    p <- p + theme(legend.position = "none", axis.text.x = element_text(angle = 90, 
                                                                        hjust = 1))
    p <- p + scale_x_continuous(breaks = d$jr, labels = d$meetjaar)
    p <- p + labs(x = "Jaar", y = paste("Concentratie", 
                                        param, strsplit(x$eenheid[1], " ")[[1]][1]), 
                  title = paste("Metingen in filter", i))
    p <- p + theme(plot.title = element_text(hjust = 0.5), 
                   legend.position = "bottom")
    p <- p + scale_color_manual(name = "", values = c(`< RG` = "red", 
                                                      waarneming = "black"))
    p <- p + scale_linetype_manual(name = "", values = c(2, 
                                                         2), guide = guide_legend(override.aes = list(color = c("orange", 
                                                                                                                "red"))))
    return(p)
}
