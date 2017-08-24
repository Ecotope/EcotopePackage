
# product <- "water_temperature"; begin_date <- "20160101"; end_date <- "20160101"; station <- "9447130"
read.tides <- function(product, begin_date, end_date, station = "9447130") {
  urlBase <- "https://tidesandcurrents.noaa.gov/api/datagetter"
  options <- c(sprintf("product=%s", product),
               sprintf("begin_date=%s", begin_date),
               sprintf("end_date=%s", end_date),
               sprintf("station=%s", station),
               "application=web_services",
               "format=csv",
               "time_zone=lst_ldt",
               "units=english",
               "datum=MLLW")
  urlx <- paste(urlBase, paste(options, collapse = "&"), sep = "?")
  print(urlx)
  apiCall <- httr::GET(urlx)
  dset <- read.csv(file = textConnection(httr::content(apiCall, "text")))
  if(nrow(dset) %in% c(0, 1)) {
    warning("No Data Found")
    return(NULL)
  }
  dset$Date.Time <- lubridate::ymd_hm(dset$Date.Time)
  lubridate::minute(dset$Date.Time) <- 0
  dset <- aggregate(. ~ Date.Time, FUN = mean, data = dset)
  dset
}
