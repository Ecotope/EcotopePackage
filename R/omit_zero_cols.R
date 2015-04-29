#' Removes columns with only 0 values.
#'
#' @param tmpfile A dataframe.
#' @examples
#' a=data.frame(rnorm(10),rep(NA,10),rpois(10,2),c(rep(NA,4),0,0,rep(NA,4)),c(rep(NA,4),1,2,rep(NA,4)),rep(0,10))
#' print(a)
#' print(omit_zero_cols(a))

omit_zero_cols=function(tmpfile){
  zero_cols=apply(tmpfile,2,function(x){
    summm=try(sum(as.numeric(x)),silent=TRUE)
    if (is.na(summm)) {
      return(FALSE)
    } else if (summm==0) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  })
  tmpfile=tmpfile[,!zero_cols]
  return(tmpfile)
} #end function omit_zero_cols
