library(glue)
library(httr)
date_start = "2017-01-01"
date_end = "2017-01-02"
vfor1 = c('PM2.5','PM10','NO2','NO','NOx')
#PM2.5,PM10,PM1,O3,NOx,NO2,NO
#PM1 n 03 otfound
vfor2 = c("alnabru","Rådhuset")
#t3i <- vector(mode =="list")
datalist2 = list()
datalist = list()

for(i in vfor1) {

  for(ii in vfor2) {
        url <- glue::glue("https://api.nilu.no/obs/historical/{date_start}%2000:00/{date_end}%2023:00/{ii}?components={i}")
            content <- httr::GET(url)
            json <- httr::content(content, as = "parsed")
            d <- lapply(json[[1]]$values, function(x) {
             date <- stringr::str_sub(x$fromTime, 1, 10)
              value <- x$value
              value[value < 0] <- NA
              retval <- data.frame(date, value)
              return(retval)
            })
            d <- rbindlist(d)
            d <- d[, .(
                          kategori = i,
                          municipality = json[[1]][2],
                          stasjon = ii,
                          lat = json[[1]][7],
                          lon = json[[1]][8],
                          min = min(value, na.rm = T),
                          mean = mean(value, na.rm = T),
                          max = max(value, na.rm = T)
                        ), keyby = .(
                        date
                 )]

           datalist[[ii]] <- d
  }
        d2 <- c( datalist)
        datalist2[[i]] <- rbindlist(d2)
  }

  t3 <- c(datalist2)
  d <- rbindlist(t3)

