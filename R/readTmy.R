



# USA TMY File
read.tmy <- function(wname, val = "quantiles") {
  if(length(grep("tm2$", wname))) {
    wfile <- paste(eco.dir("/storage/server/TMY2DATA/"), wname, sep = "")
    weather <- read.table(wfile, fill = TRUE)
    weather <- weather[, 1]
    weather <- weather[-1]

    wr <- data.frame("hoy" = 1:8760)
    temp <- sapply(1:length(weather), function(i) {
      tmp <- as.numeric(substring(weather[i],67,70))/10
      tmp * 1.8 + 32
    })
    # temp[temp > 65] <- 65
    tempDist <- as.numeric(quantile(temp, probs = seq(0, 1, 0.01)))

    dp <- sapply(1:length(weather), function(i) {
      tmp <- as.numeric(substring(weather[i], 73, 76)) / 10
    })
    dp <- dp * 1.8 + 32
    # dp[dp > dpThresh] <- dpThresh
    dpDist <- as.numeric(quantile(dp, probs = seq(0, 1, 0.01), na.rm = TRUE))

    rh <- sapply(1:length(weather), function(i) {
      tmp <- as.numeric(substring(weather[i], 79, 81))
    })
  } else if(length(grep("epw$", wname))) {
    # tmp <- readLines(paste0("E:/TMY2DATA/CTMY2/", fname))
    tmp <- readLines(paste0(eco.dir("/storage/server/TMY2DATA/CTMY2/"), wname))
    dset <- read.csv(file = textConnection(tmp[-(1:8)]), header = FALSE)
    temp <- dset$V7 * 1.8 + 32
    # temps[temps > 65] <- 65
    rh <- dset$V9
    dp <- dset$V8 * 1.8 + 32
    # dps[dps > dpThresh] <- dpThresh
    tempDist <- as.numeric(quantile(temp, probs = seq(0, 1, 0.01)))
    # rhDist <- as.numeric(quantile(rhs, probs = seq(0, 1, 0.01), na.rm = TRUE))
    dpDist <- as.numeric(quantile(dp, probs = seq(0, 1, 0.01), na.rm = TRUE))

  } else {
    stop(paste("Unrecognized file format for", wname))
  }
  if(val == "quantiles") {
    c(tempDist, dpDist)
  } else {
    c(temp, rh)
  }
}
