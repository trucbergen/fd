library(glue)
library(httr)
library(data.table)
#library(stringr)

vurl = "https://api.nilu.no/obs/historical/" # obs =  måleverdiene
#vurl = "https://api.nilu.no/aq/historical/" # aq =  luftkvalitetsindeksen (aqi)
#vurl = "https://api.nilu.no/stats/day/" #  stats/day =  luftkvalitetsindeksen + (aqi) for seven days


vstation = c("Bergen","Oslo","Stavanger","Trondheim","Tromsø")

############ Get selectted stations #########################################################################
lurl <- "https://api.nilu.no/lookup/stations"
content <- httr::GET(lurl)
json <- httr::content(content, as = "parsed")
dlist <- rbindlist(json)
dlist <- subset(dlist, municipality %in% vstation, select = c(municipality,station,latitude,longitude,components,firstMeasurment,
                                                              lastMeasurment))


vfor1 = unique(dlist$station)
datalist2 = list()
datalist = list()
############# Loop through each station ########################################
for(i in vfor1[1:3]) {
  d2 <- dlist[dlist$station == i, c("components","firstMeasurment","lastMeasurment")]

  ##  Richard i dblist finnes oversikt over firstMeasurment og lastMeasurment kan du legge det i update_airquality()
  download_dates <- seq.Date(lubridate::date(d2$firstMeasurment), lubridate::today()-1, 1)
  download_dates <- split(download_dates, ceiling(1:length(download_dates) / 28))

  date_start <-  "2018-01-01"   #lubridate::date(download_dates [[1]][[1]])
  date_end <-    "2018-01-28"   #lubridate::date(download_dates [[1]][[28]])

  vfor2 <- unlist(strsplit(d2$components,","))
  vfor3 <- dput(as.character(vfor2))

  ############# Loop through each components of selected station ########################################
  for(ii in vfor3) {
    tryCatch({


      url <- glue::glue(paste0(vurl,"{date_start}%2000:00/{date_end}%2023:00/{i}?components={ii}"))
      content <- httr::GET(url)
      json <- httr::content(content, as = "parsed")
      d <- rbindlist(json)
      d <- lapply(json[[1]]$values, function(x) {
        #date <- stringr::str_sub(x$dateTime, 1, 10)
        date <- x$fromTime
        #date[date < 0] <- NA
        value <- x$value
        value[value < 0] <- NA
        retval <- data.frame(date,value)
        return(retval)
      })

      d <- rbindlist(d)
      d <- d[, .(
        municipality = json[[1]][2],
        stasjon = i,
        lat = json[[1]][7],
        lon = json[[1]][8],
        value = value,

        components = ii

      ), keyby = .(
        date
      )]

      datalist[[ii]] <- d
    }, error=function(e){})

  }


  d2 <- c( datalist)
  datalist2[[i]] <- rbindlist(d2)
}

t3 <- c(datalist2)
d <- rbindlist(t3)
d <- as.data.frame(lapply(d, unlist))
d <- data.table::dcast(d, date + municipality + stasjon ~ components, value.var = "value",fun = max)
d$date <- ymd_hms(d$date)
################################# Merge two filen sammen ################################################################################





