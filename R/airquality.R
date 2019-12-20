airquality_download_dates <- function(date_start, date_end) {
  url <- glue::glue("https://api.nilu.no/stats/day/{date_start}/{date_end}/manglerud?components=pm10")
  #url <- glue::glue("https://api.nilu.no/obs/historical/{date_start}%2000:00/{date_end}%2023:00/alnabru?components=pm10")
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
    pm10_min = min(value, na.rm = T),
    pm10_mean = mean(value, na.rm = T),
    pm10_max = max(value, na.rm = T)
  ), keyby = .(
    date
  )]

  return(d)
}

#' update_airquality
#' Updates the weather db tables
#' @export
update_airquality <- function() {
  # define the db tables
  field_types <- c(
    "date" = "DATE",
    "location_code" = "TEXT",
    "pm10_min" = "DOUBLE",
    "pm10_mean" = "DOUBLE",
    "pm10_max" = "DOUBLE"
  )

  keys <- c(
    "location_code",
    "date"
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
  download_dates <- seq.Date(as.Date("2010-01-01"), lubridate::today()-1, 1)
  if (!is.na(val)) download_dates <- download_dates[download_dates > val]

  if (length(download_dates) == 0) {
    return()
  }

  # split into 10 day groupings
  download_dates <- split(download_dates, ceiling(1:length(download_dates) / 10))

  # DO SOME DOWNLOADING
  res <- vector("list", length = length(download_dates))
  pb <- fhi::txt_progress_bar(min = 0, max = length(download_dates))
  for (i in seq_along(download_dates)) {
    utils::setTxtProgressBar(pb, i)

    date_start <- min(download_dates[[i]])
    date_end <- max(download_dates[[i]])

    date_start <- min(download_dates[[i]])
    try(
      res[[i]] <- airquality_download_dates(
        date_start = date_start,
        date_end = date_end
      ),
      TRUE
    )400 Bad Request — httpstatuses.com
    httpstatuses.com
    HTTP Status Code 400: The server cannot or will not process the request due to something that is perceived to be a client error (e.g., malformed request syntax, invalid request message framing, or deceptive request routing).
  }

  res <- rbindlist(res)
  res[, location_code := "municip0301"]

  # upload the results to the database
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
