library(glue)
library(httr)
library(data.table)
#library(stringr)

vurl = "https://api.nilu.no/obs/historical/" # obs =  måleverdiene
date_start = "2017-01-01"
date_end =  "2017-12-31"

#vurl = "https://api.nilu.no/aq/historical/" # aq =  luftkvalitetsindeksen (aqi)
#vurl = "https://api.nilu.no/stats/day/" #  stats/day =  luftkvalitetsindeksen (aqi)
#date_start = lubridate::today() - 7  #"2018-11-17"
#date_end =  lubridate::today() #"2018-11-21"


vstation = c("Bergen","Oslo","Stavanger","Trondheim","Tromsø")

############ Get selectted stations #########################################################################
url <- "https://api.nilu.no/lookup/stations"
content <- httr::GET(url)
json <- httr::content(content, as = "parsed")
d <- rbindlist(json)
dlist <- subset(d, municipality %in% vstation, select = c(municipality,station,latitude,longitude,components))

############# Loop through all variables for each selected station ########################################
vfor1 = unique(dlist$station)
datalist2 = list()
datalist = list()

for(i in vfor1[1]) {
  d2 <- dlist[dlist$station == i,"components"]
  vfor2 <- unlist(strsplit(d2$components,","))
  vfor3 <- dput(as.character(vfor2))

  for(ii in vfor3[1]) {
    tryCatch({
      #url <- glue::glue(paste0(vurl,"{date_start}/{date_end}/{i}?components={ii}"))

      url <- glue::glue(paste0(vurl,"{date_start}%2000:00/{date_end}%2023:00/{i}?components={ii}"))

      content <- httr::GET(url)
      json <- httr::content(content, as = "parsed")
      d <- lapply(json[[1]]$values, function(x) {
        #date <- stringr::str_sub(x$dateTime, 1, 10)
        date <- x$dateTime #stringr::str_sub(x$dateTime, 1, 10)
        date[date < 0] <- NA
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

 # https://api.nilu.no/obs/historical/2017-01-01%2000:00/2018-12-31%2023:00/Danmarks plass?components=PM2.5https://api.nilu.no/obs/historical/2017-01-01%2000:00/2018-12-31%2023:00/Danmarks plass?components=PM2.5

  d2 <- c( datalist)
  datalist2[[i]] <- rbindlist(d2)
}

t3 <- c(datalist2)
d <- rbindlist(t3)


d$date <- ymd_hms(d$date)
d$municipality <- as.factor(d$municipality )
d$lat <- as.factor(d$lat)
d$lon <- as.factor(d$lon)
sub <- selectByDate(d,year = 2019, day = 20:22,hour=10:12)



##################################### Test
cols <- c('municipality', 'lat','lon','components')
d[,cols] <- mapply(as.factor,d[,cols])


d$date <- as.POSIXct(d$date)


#d$WeekDay <- weekdays(as.Date(d$date))

d$Weekday <- format(as.Date(d$date), "%a")
  #format(weekdays(as.Date(d$date)), "%a")
  #fwrite(d, file = "test.csv", sep = ",")

#########################################################
library(ggplot2)
ggplot(subset(d,municipality  == 'Bergen'), aes(x = Weekday, y = value,group = 1)) +
 # geom_step() +
  geom_line() +
  geom_point() +
  facet_grid(stasjon ~ components) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#####################################################################################
sub <- selectByDate(d,year = 2019, month = 6:9, hour = 7:19)



