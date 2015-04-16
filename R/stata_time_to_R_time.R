#' Convert Stata time to R time after using read.dta
#'
#' @param times A time vector to convert.
#'


stata_time_to_R_time<-function(times) {
	time<-as.POSIXct(times/1000,origin="1960-01-01",tz="GMT")
	return(time)
}
