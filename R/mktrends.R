#' Berekent Mann-Kendall statistiek voor enkel LMG filter 
#' 
#' Voor een enkele put/filter in de data van \code{lmgsubset}
#' wordt de Mann-Kendall statistiek berekend voor een parameter.
#' De data van de \code{lmgsubset} wordt omgezet in een tijdreeks,
#' uitbijters worden verwijderd met \code{rmoutlier}
#' 
#' @param i put/filter naam waarvoor de statistiek berekend wordt
#' @param x data.frame van lmgsubset
#' @param dw drempelwaarde van relevante stof per relevant grondwaterlichaam
#' @param trim als TRUE, dan worden uitbijters verwijderd (trimmed)
#' @param trimFactor factor van \code{rmoutlier}
#' @param psig significanctie niveau
#' @param alter alternatieve hypothese van \code{kendatTrendTest}
#' @param rpDL of de rapportagegrenzen aangepast moeten worden, standaard op TRUE.
#' @param make.plot geeft een plot output in plaats van een data.frame
#'
#' @return data.frame met statistiek of een plot object (als #' make.plot=TRUE)
#' 
#' @importFrom EnvStats kendallTrendTest
#'
#' @export


mktrends <- function(i, x , dw, trim = FALSE, trimfactor = 1.5,
                     psig = 0.05, alter = "two.sided", 
                     rpDL = TRUE, make.plot = FALSE ) {
  
    param <- x$parameter[1]
    # subset d, only interested in time serie, i.e. jr and
    # concentration
    d <- x %>% select(putfilter, meetjaar, waarde, detectielimiet, eenheid) %>%
        mutate(jr = meetjaar - min(meetjaar)) %>%
        arrange(jr) %>%
        filter(putfilter == i)
    
    d <- na.omit(d)
    
    if(rpDL) {
    d <- d %>% replaceDL() 
    }
    
    # wijs de reeks af als er minder dan 4 waarnemingen (waarde > RG) zijn 
    if(nrow(d[d$detectielimiet < 1, ]) < 4) {
        return(NA)
    }

    # remove outliers
    if(trim) {
        d <- mutate(d,
                    waarde=rmoutlier(d[["Waarde"]], factor = trimfactor,
                                     na.rm = TRUE))
    }

    # remove NA values, order by year and create time series
    d <- na.omit(d)
    #d <- d[order(d$jr),]
    #d.ts <- ts(select(d,waarde))

    # do Mann-Kendall test
    n <- nrow(d)
    d.mk <- EnvStats::kendallTrendTest(waarde~jr, data = d, alternative = alter)

    # create data.frame with results, also add description of result
    # (significant trend / no trend and direction of trend)

    res <- data.frame(putfilter = i,
                      S = d.mk$S,
                      var = d.mk$var.S,
                      n = n,
                      p = d.mk$p.value,
                      tau = d.mk$estimate[1],
                      slope = d.mk$estimate[2],
                      intercept = d.mk$estimate[3],
                      slopeLCL = d.mk$interval$limits[1],
                      slopeUCL = d.mk$interval$limits[2],
                      trend = (ifelse(d.mk$p.value <= psig, "trend", "geen trend")),
                      direction = (ifelse(d.mk$S <= 0, "neerwaarts", "opwaarts")),
                      stringsAsFactors = FALSE, row.names = i)

    if(make.plot) {
        # make plot
        
        d <- d %>% mutate(detectielimiet = ifelse(detectielimiet == 1, "< RG", "waarneming"))
      
        p <- ggplot(d, aes(jr, waarde, colour = detectielimiet))
        p <- p + geom_line(colour = "grey")
        p <- p + geom_point()
        p <- p + geom_hline(aes(yintercept = dw, linetype = "drempelwaarde"), colour = "red") 
       p <- p + geom_hline(aes(yintercept = 0.75 * dw, linetype = "75% drempelwaarde"), colour = "orange")
        p <- p + theme(legend.position = "none", 
                       axis.text.x = element_text(angle = 90, hjust = 1))
        p <- p + scale_x_continuous(breaks = d$jr, labels = d$meetjaar)
        p <- p + labs(x = "Jaar", y = paste("Concentratie", param, strsplit(x$eenheid[1], " ")[[1]][1])),
                      title = paste("Trend in filter ", i))
        p <- p + theme(plot.title = element_text(hjust = 0.5),
                       legend.position = "bottom")
        p <- p + scale_color_manual(name = "", values = c(`< RG` = "red", waarneming = "black"))
        p <- p + scale_linetype_manual(name = "", values = c(2, 2),
                                       guide = guide_legend(override.aes = list(color = c("orange", "red"))))

        if(res$p <= psig) {
            p <- p + geom_abline(intercept = res$intercept,
                                 slope = res$slope,
                                 color = "red")
        }
        return(p)

    } else {
        return(res)
    }

}


