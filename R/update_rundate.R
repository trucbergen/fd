#' update_rundate
#' Updates the rundate db tables
#' @param package a
#' @param date_extraction date when the data file was extracted
#' @param date_results the last date of the results
#' @param date_run the date when the analysis was run
#' @export
update_rundate <- function(package, date_extraction, date_results, date_run) {
  # date_extraction = date when the data file was extracted
  # date_results = the last date of the results
  # date_run = the date when the analysis was run
  field_types <- c(
    "package" = "TEXT",
    "date_extraction" = "DATE",
    "date_results" = "DATE",
    "date_run" = "DATE"
  )

  keys <- c(
    "package"
  )

  rundate <- schema$new(
    db_config = config$db_config,
    db_table = "rundate",
    db_field_types = field_types,
    db_load_folder = "/xtmp/",
    keys = keys,
    check_fields_match = TRUE
  )

  rundate$db_connect()

  to_upload <- data.table(
    package = package,
    date_extraction = as.Date(date_extraction),
    date_results = as.Date(date_results),
    date_run = as.Date(date_run)
  )

  rundate$db_upsert_load_data_infile(to_upload)
}

#' get_rundate
#' Gets the rundate db table
#' @export
get_rundate <- function() {
  conn <- get_db_connection()
  use_db(conn, "sykdomspuls")

  if (!DBI::dbExistsTable(conn, "rundate")) {
    stop("you need to run update_rundate()")
  }

  temp <- dplyr::tbl(conn, "rundate") %>%
    dplyr::collect() %>%
    fd::latin1_to_utf8()
  return(temp)
}
