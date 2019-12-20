library(glue)
library(httr)
library(data.table)
#library(stringr)
    
    #vurl = "https://api.nilu.no/obs/historical/" # obs =  måleverdiene 
    vurl = "https://api.nilu.no/aq/historical/" # aq =  luftkvalitetsindeksen (aqi)
    date_start = lubridate::today() -7   #"2018-11-17"
    date_end =  lubridate::today() #"2018-11-21"
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
    
    for(i in vfor1) {
      d2 <- dlist[dlist$station == i,"components"]
      vfor2 <- unlist(strsplit(d2$components,","))
      vfor3 <- dput(as.character(vfor2))
    
      for(ii in vfor3) {
        tryCatch({
          url <- glue::glue(paste0(vurl,"{date_start}%2000:00/{date_end}%2023:00/{i}?components={ii}"))
          
        content <- httr::GET(url)
        json <- httr::content(content, as = "parsed")
        d <- lapply(json[[1]]$values, function(x) {
          date <- stringr::str_sub(x$fromTime, 1, 10)
          index = x$index
          color = x$color
          date[date < 0] <- NA
          value <- x$value
          value[value < 0] <- NA
          retval <- data.frame(date,value,index,color)
          return(retval)
        })
        
        d <- rbindlist(d)
        d <- d[, .(
          municipality = json[[1]][2],
          stasjon = i,
          lat = json[[1]][7],
          lon = json[[1]][8],
          value = value,
          index = index,
          color = color,
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
    
    #fwrite(d, file = "test.csv", sep = ",")
    
   
    
    