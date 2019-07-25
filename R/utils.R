# Hulp functies voor KRWTrends package

valideParameters <- function() {
    # returns valide parameters
    return(c("cl","ni","as","cd","pb","ptot","no3"))
}


testKolommen <- function(d) {
    # test of verplichte kolommen aanwezig zijn
    
    kolommen_numeriek <- c("xcoord","ycoord","meetjaar","waarde")
    kolommen <- c("putfilter","diepte","parameter",kolommen_numeriek)
    if(length(setdiff(kolommen,names(d)))>0) {
        stop("kolommen ontbreken of worden niet herkend")
    }

    for(i in kolommen_numeriek) {
        if(!is.numeric(d[[i]])) {
               stop(paste("kolom",i,"is niet numeriek"))
        }
    }
}

testCoordinaten <- function(d){
    # test of coordinate binnen bounding-box van Nederland vallen,
    # coordinaten in meters

    if(any(is.na(c(d$xcoord,d$ycoord)))){
        stop("enkele coordinaten ontbreken")
    }

    if(min(d$xcoord)<9000||max(d$xcoord)>280000) {
        stop("x-coordinaten buiten bereik")
    }

    if(min(d$ycoord)<300000||max(d$ycoord)>624000) {
        stop("y-coordinaten buiten bereik")
    }
    
}
testRanges <- function(d) {
    # controleer inhoud van de velden

    if(any(is.na(d$meetjaar))){
        stop("enkele meetjaren ontbreken")
    }

    if(min(d$meetjaar)<1990||max(d$meetjaar)>lubridate::year(lubridate::now()))
        stop("meetjaar buiten bereik")

    if(length(setdiff(unique(d$parameter),valideParameters()))>0) {
        stop("onbekende parameters aangetroffen")
    }
}

testData <- function(d) {
    # voer verschillende testen uit
    testKolommen(d)
    testCoordinaten(d)
    testRanges(d)
}



testSerie <- function(d) {
    # check tijd serie, 1 parameter, 1 diepte
    testData(d)
    if(length(unique(d$parameter))>1) {
        stop("Meer dan 1 parameter aanwezig")
    }
    if(length(unique(d$diepte))>1) {
        stop("Meer dan 1 diepte aanwezig")
    }
    if(any(na.omit(d$waarde<0))) {
        stop("Negatieve concentratie aanwezig")
    }

}

