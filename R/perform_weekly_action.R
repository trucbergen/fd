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
