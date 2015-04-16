#' Removes columns with only missing values.
#'
#' @param tmpfile A dataframe.
#' @examples
#' a=data.frame(rnorm(10),rep(NA,10),rpois(10,2),c(rep(NA,4),0,0,rep(NA,4)),c(rep(NA,4),1,2,rep(NA,4)),rep(0,10))
#' print(a)
#' print(omit_NA_cols(a))
omit_NA_cols=function(tmpfile){

  NA_cols=apply(tmpfile,2,function(x){sum(is.na(x))==length(x)})
  tmpfile=tmpfile[,!NA_cols]
  return(tmpfile)
} #end function omit_NA_cols
