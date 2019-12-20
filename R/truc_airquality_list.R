library(glue)
library(httr)
library(data.table)
#library(stringr)
    
    vstation = c("Bergen","Oslo","Stavanger","Trondheim","Tromsø")
    
    ############ Get selectted stations #########################################################################
    url <- glue::glue("https://api.nilu.no/lookup/stations")
    content <- httr::GET(url)
    json <- httr::content(content, as = "parsed")
    d <- rbindlist(json)
    dlist <- subset(d, municipality %in% vstation, select = c(municipality,station,latitude,longitude,components))
    
    #vfor1 <- paste(strsplit(d$components[1], "\\,"),sep=",")
    #for(i in vfor1) {
    
    
    #vars <-toString(paste0(vars),sep=" ," )
    
    
    
    ############# Loop through all variables for each selected station ########################################
    vfor1 = unique(dlist$station)
    datalist2 = list()
    datalist = list()
    
    for(i in vfor1) {
      d2 <- dlist[dlist$station == i,"components"]
        #subset(dlist,dlist$station == i,select = components)
      vfor2 <- unlist(strsplit(d2$components,","))
      vfor3 <- dput(as.character(vfor2))
      
      
     # vfor2 <- c("PM2.5","PM10","PM1", "NOx", "NO2", "NO","CO")
    
      for(ii in vfor3) {
        tryCatch({
       
        url <- glue::glue("https://api.nilu.no/stats/day/2018-09-17/2018-09-21/{i}?components={ii}")
        
       
        #"https://api.nilu.no/obs/historical/{date_start}%2000:00/{date_end}%2023:00/{ii}?components={ii}")
        content <- httr::GET(url)
        json <- httr::content(content, as = "parsed")
        d <- lapply(json[[1]]$values, function(x) {
          date <- x$dateTime  #stringr::str_sub(x$fromTime, 1, 10)
          date[date < 0] <- NA
          value <- x$value
          value[value < 0] <- NA
          retval <- data.frame(date, value)
          return(retval)
        })
        
        d <- rbindlist(d)
        d <- d[, .(
          stasjon = i,
          value = value,
          components = ii
         # municipality = json[[1]][2],
         # stasjon = ii,
          #lat = json[[1]][7],
          #lon = json[[1]][8],
         
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
    
    