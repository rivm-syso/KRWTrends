#' Bereken gegevens trendomkering
#'
#' Deze functie berekent de statistieken voor de trendomkering. Het
#' bepaald of trendomkeringen aanwezig zijn en het jaar waarin de
#' omkering plaatsvindt. 
#'
#' De trends worden gecorrigeerd voor vals positieven.
#'
#' @param x data.frame met grondwaterdata
#' @param sig alpha waarde (significantie) van trendomkeringen
#'
#' @return tabel met trendstatistieken
#'  \itemize{
#'    \item aantal trendomkeringen
#'    \item percentage trendomkeringen
#'    \item gemiddelde hellingen van trendtrendomkeringen
#'    \item gemiddeld jaar en standaardafwijking van keerpunt
#'    \item voor benoemde als niet benoemde trends
#' }
#'
#' @export

trendomkeringAnalyse  <- function(d,sig=0.05) {

    if(nrow(d) > 0) { 
  
    if(length(unique(d$parameter))!=1) {
        stop("Er mag slechts 1 parameter in dataset aanwezig zijn")
    }

#     resultaat=data.frame()


    i <- as.character(unique(d$putfilter))
    resultaat  <- lapply(i,FUN=trendReversal,d) %>%
        do.call("rbind",args=.) %>%
        na.omit()

#     for (i in unique(d$putfilter)) {
#         d.serie  <- d %>% filter(putfilter==i) %>%
#             select(meetjaar,waarde)
# 
#         res <- trendReversal(d.serie,make.plot=FALSE)
#         resultaat <- resultaat %>%
#             bind_rows(data.frame(putfilter=i,
#                                  res$output,
#                                  stringsAsFactors=FALSE))
#     }

    fdr  <-  resultaat %>%
        select(p) %>%
        bhfdr()

    trend1  <- resultaat %>%
        filter(p<=fdr$threshold) %>%
        mutate(trend="trendomkering - benoemd")
    trend2  <- resultaat %>%
        filter(p<=sig,p>fdr$threshold)  %>% 
        mutate(trend="trendomkering - niet benoemd")
    trend3 <- resultaat %>%
        filter(p>sig) %>%
        mutate(trend="geen trendomkering")
    trend <- trend1 %>%
        bind_rows(trend2) %>%
        bind_rows(trend3)

    n.total <- nrow(trend)
    trend.sum <- trend %>%
        group_by(trend) %>%
        summarise(n = n(),
                  percentage = n() / n.total,
                  gem.richting1 = mean(slope.1),
                  gem.richting2 = mean(slope.2),
                  jaar = mean(turning.point),
                  jaar.sd = sd(turning.point),
                  putfilters = paste(sort(unique(putfilter)), collapse = ", ")
                  )
        


    return(trend.sum)

    }
}

