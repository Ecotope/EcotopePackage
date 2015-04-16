#' Changes variable names.
#'
#' @param tmpfile A dataframe to modify.
#' @param variable_alias_ref A special dataframe which has names to change to.

#' @examples
#' No examples yet


name_fix = function(tmpfile,variable_alias_ref){
  names(tmpfile)=iconv(names(tmpfile),"latin1", "ASCII", sub="")
  #  fix some names
  null_inds=NULL
  rename_ind=which(names(tmpfile) %in% variable_alias_ref$variable.alias)
  NA_ind=which(is.na(variable_alias_ref$variable.true))
  rename_ind=setdiff(rename_ind,NA_ind)
  for (ind in rename_ind){
    rename_to_ind=which(variable_alias_ref$variable.alias==names(tmpfile)[ind])
    if (variable_alias_ref$variable.true[rename_to_ind]=="NULL")  {
      null_inds=c(null_inds,ind)
    } else {
      names(tmpfile)[ind]=as.character(variable_alias_ref$variable.true[rename_to_ind])
    }
  }
  if (length(null_inds))  tmpfile=tmpfile[,-null_inds]



  return(tmpfile)
} #end function name_fix
