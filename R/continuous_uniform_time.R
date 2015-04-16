#' Adds non-present time stamps in median increments.
#'
#' @param xtmpfile A dataframe to modify.
#' @param DateTime The name of the time variable.
#' @examples
#'
#' No examples yet


continuous_uniform_time=function(tmpfile,DateTime){
  ind=which(names(tmpfile)==DateTime)


  na_time=which(is.na(tmpfile[,ind]))
  if(sum(na_time)>0) tmpfile=tmpfile[-na_time,]


  #sort by time
  tmpfile=tmpfile[order(tmpfile[,ind]),]
  #find median time difference
  time_diff=median(diff(tmpfile[,ind]))
  #make all wanted time stamps
#   hold=seq(from=tmpfile[,ind][1],to=tmpfile[,ind][nrow(tmpfile)],by=time_diff)
  hold=seq(from=min(tmpfile[,ind]),to=max(tmpfile[,ind][nrow(tmpfile)]),by=time_diff)
  dum_time=data.frame(hold)
  names(dum_time)=DateTime
  #force all wanted time stamps into dataframe
  this=merge(tmpfile,dum_time,by=DateTime,all=TRUE)
  this=this[order(this[,ind]),]

  return(this)

}
