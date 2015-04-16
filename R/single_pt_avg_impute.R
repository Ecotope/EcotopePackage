#' Imputes single values with the mean of pior and post values.
#' Must be sorted for by time
#' Does not account for jumps in time, suggest first applying continuous_uniform_time
#' @param tmpfile A data frame to impue
#' @examples
#' a=data.frame(c(1,2,3,NA,5,6,NA,8,9,10),seq(1,10),c(1,2,NA,NA,5,6,NA,50,9,10),rep("hat",10))
#'  print(a)
#' print(single_pt_avg_impute(a))
#'






single_pt_avg_impute=function(tmpfile){
  n_nums=sapply(tmpfile,is.numeric)


  tmpfile[,n_nums]=apply(tmpfile[,n_nums],2,function(x){
    na_ind=which(is.na(x))
    if (length(na_ind)==0) try(next, silent=TRUE)
    for (j in na_ind){
      if (j==1 | j==length(x)) try(next, silent=TRUE)
      if (is.na(x[j-1]) | is.na(x[j+1])) try(next, silent=TRUE)
      hold=try(mean(c(x[j-1],x[j+1])))
      if (class(hold)!="try-error") x[j]=hold

    }
    return(x)
  }
  )
  return(tmpfile)
} #end function single_pt_avg_impute


