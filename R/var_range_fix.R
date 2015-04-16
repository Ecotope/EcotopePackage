#' Omits specific outlier range.
#'
#' @param tmpfile A dataframe to modify.
#' @param var_ranges A special dataframe which has ranges to omit.

#' @examples
#' No examples yet
#'
#'
var_range_fix=function(tmpfile,var_ranges){

  irrelevant_ranges <- apply(var_ranges, 1,function(x){sum(is.na(x))==2})
  var_ranges=var_ranges[!irrelevant_ranges,]

  rel_ind=which(names(tmpfile) %in% var_ranges$variable.true)
  for (ind in rel_ind){
    ind2=which(var_ranges$variable.true==names(tmpfile)[ind])
    low_ind=which(tmpfile[,ind]<var_ranges$min.allowed[ind2])
    if (length(low_ind)>0) tmpfile[low_ind,ind]=NA
    high_ind=which(tmpfile[,ind]>var_ranges$max.allowed[ind2])
    if (length(high_ind)>0) tmpfile[high_ind,ind]=NA
  }
  return(tmpfile)
} #end function var_fix
