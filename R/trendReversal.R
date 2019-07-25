#' Trend reversal test
#'
#' De trend reversal test is een methode om te toetsen of er
#' trendomkering in de data aanwezig is
#'
#' @param i put/filter naam waar statistiek voor berekend wordt
#' @param x data.frame van lgmsubset
#' @param trim als TRUE, dan worden uitbijters verwijderd (trimmed)
#' @param trimFactor factor van \code{rmoutlier}
#' @param make.plot als TRUE dan wordt een plot object aangemaakt
#'
#' @return een trendReversal object met daarin de resultaten van de
#' trend reversal test en, als make.plot is TRUE, een ggplot object
#' met een grafische weergave van de trendomkering
#'
#' Een trendomkering ontstaat als de concentratie (van een stof in een
#' put) vanaf een bepaalde moment/jaar - het keerpunt genoemd -
#' toeneemt (of afneemt, of constant is) terwijl de concentratie
#' daarvoor afnam (of toenam, of constant was). Het detecteren van een
#' trendomkering is meer complex dan het detecteren van een trend
#' alleen, aangezien dat er meerdere patronen zijn waarop een reeks
#' geen duidelijk toenemende or afnemende trend aantoont en waardoor
#' een trendomkering onterecht gesignaleerd kan worden. 
#'
#' Om zulke
#' patronen uit te sluiten wordt eerst gecontroleerd of de reeks
#' compatibel is met twee achtervolgende trends in verschillende
#' richtingen. Voor elke keuze van het potentiele keerpunt wordt de
#' reeks in twee stukken verdeeld, op elk stuk wordt een rechte lijn
#' aangepast door de robuuste methode van Theil-Sen, en een algemeen
#' mate van discrepantie tussen de lijnen en de werkelijke
#' concentraties berekent (namelijk de som van de kwadraten van de
#' ‘residuen’, ofwel de verschillen tussen de aangepaste lijnen en de
#' concentraties). Het keerpunt met de kleinste discrepantie wordt
#' gekozen, met de bijbehorende lijnen. Alleen als de hellingen van de
#' lijnen van teken verschillen (een is <0 en de andere >0) gaat men
#' verder om een p-waarde te berekenen (welke p-waarde de aanwijzing
#' geeft voor het ontstaan van een trendomkering eerder dan een
#' simpele trend); als dit niet het geval is dan wordt zonder meer de
#' p-waarde gedefinieerd als 1 (wat geen aanwijzing voor trendomkering
#' geeft). 
#' Voor het berekenen van de p-waarde wordt een regressie
#' model met een kwadratische term aan de data gepast (m.a.w.: men
#' past een parabool i.p.v. een rechte lijn op de reeks
#' concentraties). Daarna wordt er verifieerd of het hoogste/laagste
#' punt van de aangepaste kromme binnen het tijdsvenster van de reeks
#' valt (zoals het hoort bij een trendomkering). In het geval dat het
#' niet zo is wordt de p-waarde gedefinieerd als 1. Anders wordt er
#' getoetst of het kwadratische model de data niet ‘aanzienlijk beter
#' beschrijft’ dan het simpeler recht lijnig model. 
#'
#' @export


#trendReversal <- function(series,make.plot){
trendReversal <- function(i,x,trim=FALSE,trimfactor=1.5,
                          make.plot=FALSE) {


    param <- x$parameter[1]
    d <- x %>% filter(putfilter==i) %>%
        select(Jaar=meetjaar,Waarde=waarde)

    if(trim) {
        d <- mutate(d,
                    waarde=rmoutlier(d[["Waarde"]],factor=trimfactor,
                                     na.rm=TRUE))
    }

    series <- na.omit(d)

# 
#     names(series) <- c("Jaar","Waarde")
#     series <- na.omit(series)
# 
    min.no.years <- 5
    Years <- sort(unique(series$Jaar))
        
    output <- data.frame(turning.point=NA,slope.1=NA,slope.2=NA,intercept.1=NA,intercept.2=NA,p=1)
    p <- NA

    if(length(Years)>=min.no.years*2) {

        permissible.range <- Years[(min.no.years):(length(Years)-min.no.years)]

        results.Theil.Sen <- as.data.frame(t(sapply(permissible.range,apply.Theil.Sen,series)))
        names(results.Theil.Sen) <- c("Jaar","slope.1","slope.2","intercept.1","intercept.2","RSS")
        results.Theil.Sen <- transform(results.Theil.Sen,discordant.slopes=sign(slope.1*slope.2))
        results.Theil.Sen 

        best.Theil.Sen <- subset(results.Theil.Sen,discordant.slopes<0)
        minimum.RSS <- suppressWarnings(min(best.Theil.Sen$RSS))
        best.Theil.Sen <- subset(best.Theil.Sen,RSS<=minimum.RSS)[1,]; rownames(best.Theil.Sen) <- NULL
        best.Theil.Sen  
        turning.point <- as.numeric(best.Theil.Sen[1])

        quadratic.model <- lm(Waarde~Jaar+I(Jaar^2),data=series)
        fitted.values <- fitted(quadratic.model)
        quadratic.model <- summary(quadratic.model)     
        estimates.of.quadratic.model <- coef(quadratic.model)[,1]  
        other.turning.point <- as.numeric(-0.5*estimates.of.quadratic.model[2]/estimates.of.quadratic.model[3])

        P.value.reversal <- quadratic.model$coefficients[3,4]
        range.of.years <- range(series$Jaar)

        if(any(results.Theil.Sen$discordant.slopes<0) & other.turning.point>=min(permissible.range) & 
           other.turning.point<=max(permissible.range)){

            if(!make.plot){
                output <- transform(best.Theil.Sen[1:5],p=P.value.reversal)

                names(output)[1] <- "turning.point"
            } else {


                p <- ggplot(data=series,aes(x=Jaar,y=Waarde)) +
                    geom_line(color = "grey") +
                    geom_point() +
                    geom_line(aes(x=Jaar, 
                                  y=estimates.of.quadratic.model[1]+
                                      estimates.of.quadratic.model[2]*series$Jaar+
                                      estimates.of.quadratic.model[3]*(series$Jaar^2)),color="black")
                    aux.x.axis <- series$Jaar[series$Jaar<=turning.point]
                    aux.y.axis <- best.Theil.Sen$intercept.1+best.Theil.Sen$slope.1*aux.x.axis
                    d <- data.frame(x=aux.x.axis,y=aux.y.axis)
                    p <- p + geom_line(aes(x,y),data=d,
                                      color=ifelse(best.Theil.Sen$slope.1<0,"blue","red"))
                    aux.x.axis <- series$Jaar[series$Jaar>=turning.point]
                    aux.y.axis <- best.Theil.Sen$intercept.2+best.Theil.Sen$slope.2*aux.x.axis
                    d <- data.frame(x=aux.x.axis,y=aux.y.axis)
                    p <- p + geom_line(aes(x,y),data=d,
                                      color=ifelse(best.Theil.Sen$slope.2<0,"blue","red"))
                    p <- p + geom_vline(aes(xintercept=turning.point),color="grey")
                    p <- p + labs(x = "Jaar", y = paste("Concentratie", param, " [mg/]"))
                    output <- p
            } 
        }

    } 

    return(output)
}	




