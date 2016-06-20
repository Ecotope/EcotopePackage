

eco.dir <- function(str) {
  driveMap <- data.frame("linux" = c("server", "RBSA_secure", "CONSULT", "homes"),
                         "windows" = c("E:", "R:", "F:", "Z:"))

  os <- Sys.info()["sysname"]
  # Two cases: if the OS matches the path, do nothing. Else change the path
  # Check for the deprecated fileserver mount point. Should be storage
  str <- gsub("fileserver", "storage", str)
  if(length(grep("^/storage", str))) {
    pathType <- "Linux"
  } else {
    pathType <- "Windows"
  }

  if(os == "Linux") {
    if(pathType != "Linux") {
      drive <- gsub("^([EFRZ]\\:)/(.+)$", "\\1", str)
      if(drive == str) {
        stop(paste("Couldn't find a drive for", str))
      }
      path <- gsub("^([EFRZ]\\:)/(.+)$", "\\2", str)
      if(path == str) {
        stop(paste("Couldn't parse file path for", str))
      }
      newPath <- paste("/storage", driveMap$linux[driveMap$windows == drive], path, sep = "/")
    } else {
      newPath <- str
    }
  } else {
    if(pathType != "Windows") {
      drive <- gsub("^/storage/([^\\/]+)/(.+)$", "\\1", str)
      if(drive == str) {
        stop(paste("Couldn't find a drive for", str))
      }
      path <- gsub("^/storage/([^\\/]+)/(.+)$", "\\2", str)
      if(path == str) {
        stop(paste("Couldn't parse file path for", str))
      }
      newPath <- paste(driveMap$windows[driveMap$linux == drive], path, sep = "/")
    } else {
      newPath <- str
    }
  }

  newPath
}

# eco.dir("E:/TMYDATA")
# eco.dir("R:/NEEA")
# eco.dir("F:/client")
#
# eco.dir("/storage/server/NEEA/NSH")
# eco.dir("/storage/RBSA_secure/NEEA/RBSAMETERING")
# eco.dir("/storage/CONSULT/client/BPA_E3t")
#
#


