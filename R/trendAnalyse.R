#' Bereken gegevens trendanalyse
#'
#' Deze functie berekent de statistieken voor de trendanalyse. Het
#' bepaalt of trends aanwezig zijn en de richting van trends.
#' De trends worden gecorrigeerd voor vals positieven.
#'
#' @param d data.frame met grondwaterdata
#' @param n.reeks totaal aantal reeksen van selectie
#'
#' @return tabel met trendstatistieken
#'  \itemize{
#'    \item aantal op- en neergaande trends
#'    \item percentage op- en neergaande trends t.o.v. totaal aantal reeksen en 
#'          t.o.v. aantal goedgekeurde reeksen.
#'    \item gemiddelde helling van trends
#'    \item voor benoemde als niet benoemde trends
#' }
#'
#' @export


trendAnalyse <- function(d, n.reeks) {

  if(n.reeks < 1) {
    return("totaal aantal reeksen onbekend om % trends te berekenen")
  }
  
  if(nrow(d) > 0) {  
    i <- as.character(unique(d$putfilter))
    res2 <- lapply(i, FUN = mktrends, d) %>%
        do.call("rbind", args =.) %>%
        na.omit()
    
    if(nrow(res2) > 0) {
      fdr <- res2 %>% select(p) %>% bhfdr()
      n.mk <- nrow(res2)
      
      # samenvatting benoemde trends en richting
      d.trend1 <- res2 %>%
          filter(trend == "trend", p <= fdr$threshold) %>%
          mutate(trend = "trend- benoemd") %>%
          group_by(trend, direction) %>%
          summarise(n = n(),
                    percentage = paste(round(n() / n.reeks, digits = 2), round(n() / n.mk, digits = 2), sep = " / " ),
                    gemiddeld.helling = mean((slope)),
                    putfilters = paste(sort(unique(putfilter)), collapse = ", ")) %>%
          ungroup
      
      # samenvatting benoemde trends, totaal op en neerwaartse richting
      d.trend2  <- res2 %>% 
          filter(trend == "trend", p <= fdr$threshold) %>%
          mutate(trend = "trend- benoemd") %>%
          group_by(trend) %>%
          summarise(n = n(),
                    percentage = paste(round(n() / n.reeks, digits = 2), round(n() / n.mk, digits = 2), sep = " / " ),
                    gemiddeld.helling = mean((slope)),
                    putfilters = paste(sort(unique(putfilter)), collapse = ", ")) %>%
          ungroup 

      # samenvatting niet benoemde trends (p>fdr threshold)
      d.trend3  <- res2 %>% 
          filter(trend == "trend") %>%
          mutate(trend = "trend- niet benoemd") %>%
          group_by(trend) %>%
          summarise(n = n(),
                    percentage = paste(round(n() / n.reeks, digits = 2), round(n() / n.mk, digits = 2), sep = " / " ),
                    gemiddeld.helling = mean((slope)),
                    putfilters = paste(sort(unique(putfilter)), collapse = ", ")) %>%
          ungroup 
      
      d.trend <- bind_rows(d.trend1, d.trend2, d.trend3) %>%
          mutate(direction = ifelse(is.na(direction), "op/neer" ,direction)) %>%
          mutate(statistiek = paste(trend, direction)) %>%
          select(statistiek, n, percentage, gemiddeld.helling, putfilters)
      
      } else {
        d.trend <- NA
      }
  }
  return(d.trend)
}


