#' Tukey's methode om uitbijters te verwijderen.
#'
#' Gebruik Tukey's methode om uitbijters te markeren als NA. Uitbijters zijn
#' waardes welke groter of kleiner zijn dan 1,5 (factor) keer de 
#' interkwartielafstand (IQR) plus of min kwartiel 2 of 4. NA waardes worden
#' niet automatisch verwijderd en resulteren in een error. 
#'
#' @param x vector van waardes
#' @param factor factor om de interkwartielafstand (IQR) uit te breiden
#' @param na.rm verwijder NA waardes
#' 
#' @return vector met uitbijters vervangen door NA
#' 
#' @export




rmoutlier<-function (x,factor=1.5,na.rm=FALSE) {
	# options:
	# x: vector with numeric values
	# factor: IQR factor
	# na.rm: remove NA values
	nna <- is.na(x);
	if (any(nna)&&!na.rm) {
		stop("NA's found but na.rm=FALSE");
	}
	stats <- stats::fivenum(x, na.rm = na.rm);
	iqr <- diff(stats[c(2, 4)]);
	if (factor <= 0) {
		stop("'factor' must not be <= 0");
	}else {
		out <- x < (stats[2] - factor * iqr) |
		x > (stats[4] +  factor * iqr);
		if (any(out[nna],na.rm=TRUE)){
			stats[c(1, 5)] <- range(x[!out], na.rm = TRUE);
		}
		x[out]<-NA;
	}
	return(x);
}

