#' Gebruik functie voor het vaststellen van de robuuste
#' Theil Sen helling.
#'
#' @importFrom mblm mblm

apply.Theil.Sen <- function(y,Series){

    aux.Series <- subset(Series,Jaar<=y)

    Theil.Sen <- mblm::mblm(Waarde~Jaar,dataframe=aux.Series,repeated=FALSE)$coefficients
    slope.1 <- Theil.Sen[2]
    intercept.1 <- Theil.Sen[1]
    aux.Series <- transform(aux.Series,residual=NA)
    aux.Series$residual <- aux.Series$Waarde-(intercept.1+slope.1*aux.Series$Jaar)
    RSS.1 <- sum(aux.Series$residual^2)

    aux.Series <- subset(Series,Jaar>=y)
    Theil.Sen <- mblm::mblm(Waarde~Jaar,dataframe=aux.Series,repeated=FALSE)$coefficients
    slope.2 <- Theil.Sen[2]
    intercept.2 <- Theil.Sen[1]
    aux.Series <- transform(aux.Series,residual=NA)
    aux.Series$residual <- aux.Series$Waarde-(intercept.2+slope.2*aux.Series$Jaar)
    RSS.2 <- sum(aux.Series$residual^2)

    RSS <- RSS.1+RSS.2

    return(c(y,slope.1,slope.2,intercept.1,intercept.2,RSS))

}

