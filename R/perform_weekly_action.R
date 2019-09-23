#' perform_weekly_action
#' @param file File that stores last used date
#' @param dev_always_performs Does dev always perform action?
#' @export
perform_weekly_action <- function(file, dev_always_performs = FALSE) {
  this_week <- fhi::isoyearweek(lubridate::today())
  perform_action <- TRUE

  if (file.exists(file)) {
    x <- readLines(file)
    if (this_week == x) {
      perform_action <- FALSE
    }
  }

  writeLines(this_week, con = file)

  if (dev_always_performs & !config$is_production) {
    return(TRUE)
  }

  return(perform_action)
}

#' action
#' @import R6
#' @export action
action <- R6::R6Class(
  "action",
  portable = FALSE,
  cloneable = FALSE,
  public=list(
    key = NULL,
    value = NULL,
    dev_always_performs = FALSE,
    production_days = c(1:7),
    first_date_of_production = "1900-01-01",
    initialize = function(
      key,
      value,
      dev_always_performs = FALSE,
      production_days = c(1:7),
      first_date_of_production = "1900-01-01"
      ) {
      value <<- value
      dev_always_performs <<- dev_always_performs
      production_days <<- production_days
      first_date_of_production <<- first_date_of_production

      if (is_final()) {
        key <<- glue::glue("FINAL_{key}")
      } else {
        key <<- glue::glue("PRELIM_{key}")
      }
    },
    can_perform_action = function() {
      perform_action <- TRUE

      old_value <- get_action(key)
      if (length(old_value) > 0) {
        if (value == old_value) {
          perform_action <- FALSE
        }
      }

      if (dev_always_performs & !config$is_production) {
        perform_action <- TRUE
      }

      if (config$is_production & lubridate::today() < first_date_of_production) {
        perform_action <- FALSE
      }

      return(perform_action)
    },
    action_performed = function() {
      update_action(key, value)
    },
    is_final = function() {
      today <- lubridate::wday(lubridate::today(), week_start = 1)
      return(today %in% production_days)
    },
    current_value = function() {
      get_action(key)
    }
  )
)

update_action <- function(key, value) {
  # date_extraction = date when the data file was extracted
  # date_results = the last date of the results
  # date_run = the date when the analysis was run
  field_types <- c(
    "keyx" = "TEXT",
    "value" = "TEXT"
  )

  keys <- c(
    "keyx"
  )

  action <- schema$new(
    db_config = config$db_config,
    db_table = "action",
    db_field_types = field_types,
    db_load_folder = "/xtmp/",
    keys = keys,
    check_fields_match = TRUE
  )

  action$db_connect()

  to_upload <- data.table(
    keyx = key,
    value = as.character(value)
  )

  action$db_upsert_load_data_infile(to_upload)
}

get_action <- function(key) {
  conn <- get_db_connection()
  use_db(conn, "sykdomspuls")

  if (!DBI::dbExistsTable(conn, "action")) {
    update_action("x", "x")
  }

  temp <- fd::tbl("action") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()

  return(temp[keyx == key, value])
}
