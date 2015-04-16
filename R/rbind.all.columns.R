#' A more robust rbind, allows for different columns.
#'
#' @param x A dataframe being appended to.
#' @param y A dataframe to append.




rbind.all.columns <- function(x, y) {

  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))
  if (!is.null(x)){
    if (length(y.diff)>0 & dim(x)[2]>0) {
      x[, c(as.character(y.diff))] <- NA
    }

    if (length(x.diff)>0 & dim(x)[2]>0) {
      y[, c(as.character(x.diff))] <- NA
    }
  }

  return(rbind(x,y))


} #end of function rbind.all.columns
