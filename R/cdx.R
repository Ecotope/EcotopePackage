#' Changes directory based on an external reference document
#'
#' @param targ A projectory reference id.
#' @examples
#' cdx("NILM")
#' cdx("rbsaSM2")
#'
#'

cdx<-function(targ="list") {

	if(length(grep("^[Ww]in",Sys.info()[["sysname"]])))
	{
	  cpath="E:\\ado\\site\\"
	} else
	{
		cpath="/fileserver/server/ado/site/"
	}

	d=read.csv(file=paste(cpath,"cdxdirs.csv",sep=""),header=TRUE)
	d$targ=as.character(d$targ)
	d$path=as.character(d$path)

	if(targ=="list")
	{
		subset(d,select=c(targ,path))
	} else
	{
		loc=which(d$targ==targ)
		if(length(loc))
		{

			if(length(grep("^[Ww]in",Sys.info()[["sysname"]])))
			{
			  setwd(paste0("/",d$path[loc]))
			} else
			{
			  setwd(d$path[loc])
			}
		} else {
			cat("Could not find",key,"path\n")
		}
	}

}
