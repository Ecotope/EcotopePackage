


# hex <- "5467D772"
hextime <- function(hex) {
  as.POSIXct(strtoi(sprintf("0x%s", hex)), origin = "1970-01-01")
}
