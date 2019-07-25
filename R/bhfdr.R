#' Berekening False Discovery Rate (FDR)
#'
#' Deze functie berekent de Benjamini-Hochberg False Discovery
#' Rate (FDR). 
#'
#' @param p vector van p-waardes
#' @param alpha alpha waarde voor FDR procedure, standaard is 0.05
#'
#' @return lijst met de resultaten van de FDR procedure
#'  \itemize{
#'    \item rejects Hoeveelheid afgewezen H0's
#'    \item threshold Max p-waarde van de procedure
#'    \item gamma.hat p-waarde van de minst siginificante p-waarde 
#'    gebaseerd op dichtheid
#'    \item df data frame met alle data, gebruikt voor de plot functies
#' }
#'
#' @export



bhfdr <- function(p,alpha=0.05) {


    # check if we have a data.frame with p values
    stopifnot(names(p)=="p")

    # order and unlist p values
    p <- p %>% 
        arrange(p) %>%
        unlist

    n <- length(p)

    p.fdr <- (alpha*1:n)/n
    fdr.df <- data.frame(p=p,p.fdr=p.fdr,const=p<=p.fdr)

    rejects <-  max(c(0,which(fdr.df$const)))

    threshold <- ifelse(rejects==0,0,p[rejects])

    aux.histogram <- hist(p,plot=FALSE)
    gamma.hat <- min(1,aux.histogram$density[length(aux.histogram$density)])


    result <- list(rejects=rejects,threshold=threshold,gamma.hat=gamma.hat,
                   df=fdr.df)

}


