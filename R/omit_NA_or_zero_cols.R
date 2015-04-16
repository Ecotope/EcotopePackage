#' Removes columns with only missing values or zero values.
#'
#' @param tmpfile A dataframe.
#' @examples
#' a=data.frame(rnorm(10),rep(NA,10),rpois(10,2),c(rep(NA,4),0,0,rep(NA,4)),c(rep(NA,4),1,2,rep(NA,4)),rep(0,10))
#' print(a)
#' print(omit_NA_or_zero_cols(a))


omit_NA_or_zero_cols=function(tmpfile){
  n_nums=sapply(tmpfile,is.numeric)
  zero_cols=apply(tmpfile[,n_nums],2,function(x){


    summm=sum(as.numeric(x),na.rm=TRUE)
    if (summm==0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  tmpfile=tmpfile[,-which(n_nums==1)[zero_cols]]

  return(tmpfile)
} #end function omit_NA_or_zero_cols
