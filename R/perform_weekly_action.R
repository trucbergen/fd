#' perform_weekly_action
#' @param file File that stores last used date
#' @param dev_always_performs Does dev always perform action?
#' @export
perform_weekly_action <- function(file, dev_always_performs = FALSE) {
  if (dev_always_performs & config$is_dev) {
    return(TRUE)
  }
  this_week <- format.Date(lubridate::today(), "%V")
  perform_action <- TRUE

  if (file.exists(file)) {
    x <- readRDS(file)
    if (this_week == x) {
      perform_action <- FALSE
    }
  }

  return(perform_action)
}
