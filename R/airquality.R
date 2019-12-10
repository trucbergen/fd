airquality_info <- function(){

  #vurl = "https://api.nilu.no/aq/historical/" # aq =  luftkvalitetsindeksen (aqi)
  #vurl = "https://api.nilu.no/stats/day/" #  stats/day =  luftkvalitetsindeksen + (aqi) for seven days

  vstation <- c("Bergen","Oslo","Stavanger","Trondheim","Troms\u00F8")

  # Get selected stations
  lurl <- "https://api.nilu.no/lookup/stations"
  content <- httr::GET(lurl)
  json <- httr::content(content, as = "parsed")
  dlist <- rbindlist(json)
  d <- dlist[municipality %in% vstation,.(
    municip_name = municipality,
    station,
    lat = latitude,
    lon = longitude,
    date_first = firstMeasurment,
    date_last = lastMeasurment,
    components = components
  )]
  len <- length(tstrsplit(d$components, ",", fixed=T))
  vars <- paste0("con",1:len)
  d[,(vars):=tstrsplit(components, ",", fixed=T)]
  d[,components:=NULL]
  d <- melt.data.table(d, id.vars=c(
    "municip_name",
    "station",
    "lat",
    "lon",
    "date_first",
    "date_last"
  ))
  d[,value:=stringr::str_remove_all(value, "c\\(")]
  d[,value:=stringr::str_remove_all(value, '\\"')]
  d[,value:=stringr::str_remove_all(value, "\\)")]
  d[,value:=stringr::str_remove_all(value, " ")]
  d <- d[!is.na(value)]
  d[,variable:=NULL]
  d <- unique(d)
  setnames(d,"value","component")

  d[,date_first := as.Date(stringr::str_sub(date_first,1,10))]
  d[,date_last := as.Date(stringr::str_sub(date_last,1,10))]

  setorder(d,municip_name,station)

  d <- d[date_last >= "2019-01-01"]

  return(d)
}

airquality_download_component <- function(
  station="Danmarks plass",
  component="PM2.5",
  date_start="2019-07-01",
  date_end="2019-07-01") {

  retval <- tryCatch({
    vurl <- "https://api.nilu.no/obs/historical/" # obs =  maleverdiene
    url <- glue::glue(paste0(vurl,"{date_start}%2000:00/{as.Date(date_end)+1}%2000:00/{station}?components={component}"))
    content <- httr::GET(url)
    json <- httr::content(content, as = "parsed")
    d <- rbindlist(json)
    d <- lapply(json[[1]]$values, function(x) {
      date <- x$fromTime
      value <- x$value
      value[value < 0] <- NA
      retval <- data.frame(date,value)
      return(retval)
    })

    d <- rbindlist(d)
    d <- d[, .(
      municip_name = json[[1]][2]$municipality,
      station = station,
      lat = json[[1]][7]$latitude,
      lon = json[[1]][8]$longitude,
      value = value,
      component = component

    ), keyby = .(
      date
    )]

    d[, hour:=as.numeric(stringr::str_sub(date, 12, 13))]
    d[, date:=as.Date(stringr::str_sub(date, 1, 10))]
    d[norway_locations(),on="municip_name", municip_code := municip_code]
    d[,municip_name:=NULL]
    d <- d[!is.na(value)]
    d
  }, error=function(e){
    NULL
  })

  return(retval)
}

airquality_download_dates <- function(
  date_start="2000-07-01",
  date_end="2000-07-07",
  plan = airquality_info()
){

  planx <- plan[date_first <= date_start & date_end <= date_last]

  retval <- vector("list", length=nrow(plan))
  for(i in 1:nrow(plan)){
    p <- planx[i]
    retval[[i]] <- airquality_download_component(
      station = p$station,
      component = p$component,
      date_start = date_start,
      date_end = date_end
    )
  }
  retval <- rbindlist(retval)
  if(nrow(retval)==0) return(NULL)

  retval[,location_code:=glue::glue(
    "{municip_code}_{station}",
    municip_code = municip_code,
    station = epitrix::clean_labels(station)
  )]
  retval[,location_code:=stringr::str_replace(location_code,"^municip","station")]
  retval[,station:=NULL]

  retval <- dcast.data.table(retval, date + hour + location_code + municip_code + lat + lon ~ component)
  if(!"PM2.5" %in% names(retval)){
    retval[, PM2_5 := as.numeric(NA)]
  } else {
    setnames(retval,"PM2.5","PM2_5")
  }

  missing_components <- c(
    "PM2_5",
    "PM10",
    "PM1",
    "NOx",
    "NO2",
    "NO",
    "CO",
    "O3",
    "SO2"
    )
  missing_components <- missing_components[!missing_components %in% names(retval)]
  if(length(missing_components) > 0 ) retval[, (missing_components):= as.numeric(NA)]

  return(retval)
}

#' update_airquality
#' Updates the airquality db tables
#' @export
update_airquality <- function() {
  # define the db tables
  field_types <- c(
    "date" = "DATE",
    "hour" = "DOUBLE",
    "location_code" = "TEXT",
    "municip_code" = "TEXT",
    "lat" = "DOUBLE",
    "lon" = "DOUBLE",
    "PM1" = "DOUBLE",
    "PM2_5" = "DOUBLE",
    "PM10" = "DOUBLE",
    "NO" = "DOUBLE",
    "NO2" = "DOUBLE",
    "NOx" = "DOUBLE",
    "O3" = "DOUBLE",
    "SO2" = "DOUBLE",
    "CO" = "DOUBLE"
  )

  keys <- c(
    "location_code",
    "date",
    "hour"
  )

  airquality <- schema$new(
    db_config = config$db_config,
    db_table = glue::glue("airquality"),
    db_field_types = field_types,
    db_load_folder = "/xtmp/",
    keys = keys,
    check_fields_match = TRUE
  )

  airquality$db_connect()

  # get the last observed airquality date
  val <- airquality$dplyr_tbl() %>%
    dplyr::summarize(last_date = max(date, na.rm = T)) %>%
    dplyr::collect() %>%
    latin1_to_utf8()
  val <- val$last_date

  # figure out which dates need to be downloaded
  download_dates <- seq.Date(as.Date("2000-01-01"), lubridate::today()-1, 1)
  download_dates <- seq.Date(as.Date("2000-01-01"), as.Date("2000-06-01"), 1)
  if (!is.na(val)) download_dates <- download_dates[download_dates > val]

  if (length(download_dates) == 0) {
    return()
  }

  # split into 28 day groupings
  download_dates <- split(download_dates, ceiling(1:length(download_dates) / 28))

  # DO SOME DOWNLOADING
  res <- vector("list", length = length(download_dates))
  pb <- fhi::txt_progress_bar(min = 0, max = length(download_dates))
  plan <- airquality_info()
  for (i in seq_along(download_dates)) {
    utils::setTxtProgressBar(pb, i)

    date_start <- min(download_dates[[i]])
    date_end <- max(download_dates[[i]])

    temp <- airquality_download_dates(
      date_start = date_start,
      date_end = date_end,
      plan = plan
    )
    if(is.null(temp)) next
    if(nrow(temp)==0) next
    res[[i]] <- temp
  }

  res <- rbindlist(res)
  airquality$db_upsert_load_data_infile(res)

  # save "last date observed" to "rundate"
  val <- airquality$dplyr_tbl() %>%
    dplyr::summarize(last_date = max(date, na.rm = T)) %>%
    dplyr::collect() %>%
    latin1_to_utf8()

  update_rundate(
    package = "airquality",
    date_extraction = val$last_date,
    date_results = val$last_date,
    date_run = lubridate::today()
  )
}

#' get_airquality
#' Gets the air quality, population weighted at county and national levels
#' @param impute_missing Do you want missing data imputed?
#' @export
get_airquality <- function(impute_missing) {
  conn <- get_db_connection()
  use_db(conn, "sykdomspuls")

  if (!DBI::dbExistsTable(conn, "airquality")) update_airquality()

  temp <- tbl("airquality") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()

  weather <- get_weather(impute_missing = TRUE)

  skeleton <- expand.grid(
    date = unique(weather$date),
    hour = unique(temp$hour),
    location_code = unique(temp$location_code),
    component = unique(temp$component),
    stringsAsFactors = FALSE
  )
  setDT(skeleton)
  skeleton[temp,on=c("location_code","date","hour","component"), value:=value]
  skeleton[unique(temp[,.(location_code,municip_code)]),on=c("location_code"), municip_code:=municip_code]
  skeleton[weather, on=c("municip_code==location_code", "date"), tg:=tg]
  skeleton[weather, on=c("municip_code==location_code", "date"), rr:=rr]

  # check how much data we have
  skeleton[,.(
    non_missing = mean(!is.na(value))
  ),keyby=.(component)]

  # remove O3, CO, SO2, PM1, as there is very little data
  skeleton <- skeleton[!component %in% c("O3","CO","SO2","PM1")]

  # impute from municip_average
  skeleton[,average:=mean(value,na.rm=T),by=.(date, hour, municip_code, component)]
  skeleton[is.nan(average),average:=NA]

  sum(is.na(skeleton$average))
  for(i in unique(skeleton$component)){
    fit_data <- skeleton[component==i & !is.na(skeleton$average) & !is.na(skeleton$value)]
    fit <- lm(value ~ tg + rr + average + location_code, data=fit_data)
    pred_index <- which(
      skeleton$component==i &
      !is.na(skeleton$average) &
      is.na(skeleton$value) &
      skeleton$location_code %in% unique(fit_data$location_code)
      )
    x <- predict(fit,skeleton[pred_index])
    skeleton[pred_index, value:=x]
  }
  skeleton[, average:=NULL]

  mean(is.na(skeleton$value))
  mean(skeleton$value<0,na.rm=T)
  skeleton[value<0,value:=0]

  # impute from average
  skeleton[,average:=mean(value,na.rm=T),by=.(date, hour, component)]
  skeleton[is.nan(average),municip_average:=NA]

  sum(is.na(skeleton$average))
  for(i in unique(skeleton$component)){
    fit_data <- skeleton[component==i & !is.na(skeleton$average) & !is.na(skeleton$value)]
    fit <- lm(value ~ tg + rr + average + location_code, data=fit_data)
    pred_index <- which(
      skeleton$component==i &
        !is.na(skeleton$average) &
        is.na(skeleton$value) &
        skeleton$location_code %in% unique(fit_data$location_code)
    )
    x <- predict(fit,skeleton[pred_index])
    skeleton[pred_index, value:=x]
  }
  skeleton[, average:=NULL]

  mean(is.na(skeleton$value))
  mean(skeleton$value<0,na.rm=T)
  skeleton[value<0,value:=0]

  sk <- dcast.data.table(skeleton, date+hour+location_code+municip_code+tg+rr~component, value.var = "value")

  cor(sk[,.(NO,NO2,NOx,PM10,PM2.5,tg,rr)],use="pairwise.complete.obs")

  # NO and NO2 are very highly correlated
  fit_index <- !is.na(sk$NO) & !is.na(sk$NO2)
  pred_index <- is.na(sk$NO) & !is.na(sk$NO2)
  sum(fit_index)
  sum(pred_index)
  fit <- lm(NO ~ NO2 + tg + rr, data=sk[fit_index])
  x <- predict(fit, sk[pred_index])
  sk[pred_index, NO:=x]

  mean(is.na(sk$NO))
  mean(sk$NO<0,na.rm=T)
  sk[NO<0,NO:=0]

  # predict PM2.5 from PM10
  fit_index <- !is.na(sk$PM2.5) & !is.na(sk$PM10)
  pred_index <- is.na(sk$PM2.5) & !is.na(sk$PM10)
  sum(fit_index)
  sum(pred_index)
  fit <- lm(PM2.5 ~ PM10 + tg + rr, data=sk[fit_index])
  x <- predict(fit, sk[pred_index])
  sk[pred_index, PM2.5:=x]

  mean(is.na(sk$NO))
  mean(sk$NO<0,na.rm=T)
  sk[NO<0,NO:=0]

  # check # missing
  length(unique(sk[is.na(PM10)]$date))
  length(unique(sk[is.na(PM2.5)]$date))
  length(unique(sk[is.na(NO2)]$date))
  length(unique(sk[is.na(NO)]$date))
  length(unique(sk[is.na(NOx)]$date))

  # predict NO2 from PM10 and PM2.5
  fit_index <- !is.na(sk$NO2) & !is.na(sk$PM10) & !is.na(sk$PM2.5)
  pred_index <- is.na(sk$NO2) & !is.na(sk$PM10) & !is.na(sk$PM2.5)
  sum(fit_index)
  sum(pred_index)
  fit <- lm(NO2 ~ PM10 + PM2.5 + tg + rr, data=sk[fit_index])
  x <- predict(fit, sk[pred_index])
  sk[pred_index, NO2:=x]

  mean(is.na(sk$NO2))
  mean(sk$NO2<0,na.rm=T)
  sk[NO2<0,NO2:=0]

  # check # missing
  length(unique(sk[is.na(PM10)]$date))
  length(unique(sk[is.na(PM2.5)]$date))
  length(unique(sk[is.na(NO2)]$date))
  length(unique(sk[is.na(NO)]$date))
  length(unique(sk[is.na(NOx)]$date))

  # predict NO from PM10 and PM2.5 and NO2
  fit_index <- !is.na(sk$NO) & !is.na(sk$PM10) & !is.na(sk$PM2.5) & !is.na(sk$NO2)
  pred_index <- is.na(sk$NO) & !is.na(sk$PM10) & !is.na(sk$PM2.5) & !is.na(sk$NO2)
  sum(fit_index)
  sum(pred_index)
  fit <- lm(NO ~ PM10 + PM2.5 + NO2 + tg + rr, data=sk[fit_index])
  x <- predict(fit, sk[pred_index])
  sk[pred_index, NO:=x]

  mean(is.na(sk$NO))
  mean(sk$NO<0,na.rm=T)
  sk[NO<0,NO:=0]

  # check # missing
  length(unique(sk[is.na(PM10)]$date))
  length(unique(sk[is.na(PM2.5)]$date))
  length(unique(sk[is.na(NO2)]$date))
  length(unique(sk[is.na(NO)]$date))
  length(unique(sk[is.na(NOx)]$date))

  # predict NOx from PM10 and PM2.5 and NO2 and NO
  fit_index <- !is.na(sk$NOx) & !is.na(sk$PM10) & !is.na(sk$PM2.5) & !is.na(sk$NO2) & !is.na(sk$NO)
  pred_index <- is.na(sk$NOx) & !is.na(sk$PM10) & !is.na(sk$PM2.5) & !is.na(sk$NO2) & !is.na(sk$NO)
  sum(fit_index)
  sum(pred_index)
  fit <- lm(NOx ~ PM10 + PM2.5 + NO2 + NO + tg + rr, data=sk[fit_index])
  x <- predict(fit, sk[pred_index])
  sk[pred_index, NOx:=x]

  mean(is.na(sk$NOx))
  mean(sk$NOx<0,na.rm=T)
  sk[NOx<0,NOx:=0]

  # check # missing
  length(unique(sk[is.na(PM10)]$date))
  length(unique(sk[is.na(PM2.5)]$date))
  length(unique(sk[is.na(NO2)]$date))
  length(unique(sk[is.na(NO)]$date))
  length(unique(sk[is.na(NOx)]$date))

  # reshape to long
  skeleton <- melt.data.table(sk, id.vars = c(
    "location_code",
    "municip_code",
    "date",
    "hour",
    "tg",
    "rr"
  ))
  setnames(skeleton,"variable","component")
  # collapse down to municipality level
  skeleton <- skeleton[,.(
    value = median(value, na.rm=T),
    tg = mean(tg),
    rr = mean(rr)
  ),keyby=.(
    location_code=municip_code,
    component,
    date,
    hour
  )]
  skeleton <- skeleton[,.(
    value = max(value, na.rm=T),
    tg = mean(tg),
    rr = mean(rr)
  ),keyby=.(
    location_code,
    component,
    date
  )]
  skeleton[is.infinite(value), value:=NA]
  setorder(skeleton,location_code,component,date)

  days_remaining <- 10
  while(days_remaining>0){
    skeleton[,value_lag1:=shift(value,n=1L, type="lag"),by=.(location_code,component)]

    # start to regress
    for(i in unique(skeleton$component)){
      fit_data <- skeleton[component==i]
      fit <- lm(value ~ value_lag1 + tg + rr + location_code, data=fit_data)
      pred_index <- which(
        skeleton$component==i &
        is.na(skeleton$value)
      )
      x <- predict(fit,skeleton[pred_index])
      skeleton[pred_index, value:=x]
    }

    days_remaining <- length(unique(skeleton[is.na(value)]$date))
    print(days_remaining)
  }
  skeleton[, value_lag1:=NULL]
  skeleton[, tg:=NULL]
  skeleton[, rr:=NULL]

  return(skeleton)
}
