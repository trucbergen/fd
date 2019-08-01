set_computer_name <- function() {
  if (file.exists("/tmp/computer")) {
    con <- file("/tmp/computer", "r")
    name_computer <- readLines(con, n = 1)
    close(con)
  } else {
    name_computer <- "NO_NAME_FOUND"
  }
  Sys.setenv(COMPUTER = name_computer)
  config$name_computer <- name_computer
}

#' initialize
#' @param package a
#' @param package_dir a
#' @export
initialize <- function(
                       package,
                       package_dir = paste0("/dashboards/dashboards_", package)) {
  config$package <- package

  set_computer_name()
  if (Sys.getenv("RSTUDIO") != "1" & config$name_computer %in% config$name_production) {
    config$is_production <- TRUE
  } else if (config$name_computer %in% config$name_testing) {
    config$is_testing <- TRUE
  } else {
    msg(glue::glue("DEV computer name is: '{config$name_computer}' {Sys.getenv('RSTUDIO')}"))
    config$is_dev <- TRUE
  }

  if (config$is_dev) {
    suppressPackageStartupMessages(devtools::load_all(package_dir, export_all = FALSE, quiet = TRUE))
  } else {
    suppressPackageStartupMessages(library(package, character.only = TRUE))
  }

  config$is_initialized <- TRUE
}

#' Enhanced Messaging
#' @param txt Text
#' @param type msg, warn, err
#' @param syscallsDepth The number of syscalls included in the message. Set to 0 to disable.
#' @param newLine Should there be a new line at the start of the message?
#' @export msg
msg <- function(txt, type = "msg", syscallsDepth = 2, newLine = FALSE) {

  # make warnings print immediately
  op <- options("warn")
  on.exit(options(op))
  options(warn = 1)

  if (syscallsDepth < 0) stop("syscallsDepth cannot be less than zero")
  if (!type %in% c("msg", "warn", "err")) stop(sprintf("%s not msg, warn, err", type))

  startOfLine <- ""
  if (newLine) startOfLine <- "\r\n"

  fn <- switch(type,
    msg = base::message,
    warn = base::warning,
    err = base::stop
  )

  depth <- sys.nframe() - 1
  x <- sys.calls()
  if (depth >= 1 & syscallsDepth > 0) {
    depthSeq <- depth:1
    if (length(depthSeq) > syscallsDepth) depthSeq <- depthSeq[1:syscallsDepth]
    depthSeq <- rev(depthSeq)
    for (i in depthSeq) {
      base::message(startOfLine, "           ", depth - i + 1, "/", depth, ": ", deparse(x[[i]]))
    }
  }

  if (type == "msg") {
    if (config$is_initialized) {
      fn(sprintf("%s%s/%s/%s %s\r", startOfLine, Sys.time(), config$name_computer, config$package, txt))
    } else {
      fn(sprintf("%s%s %s\r", startOfLine, Sys.time(), txt))
    }
  } else {
    if (config$is_initialized) {
      fn(sprintf("%s%s/%s/%s %s\r", startOfLine, Sys.time(), config$name_computer, config$package, txt), call. = F)
    } else {
      fn(sprintf("%s%s %s\r", startOfLine, Sys.time(), txt), call. = F)
    }
  }
}

#' Dashboard folders
#'
#' This function finds folders according to the dashboard philosophy
#' @param inside where it is inside
#' @param ... an optional file
#' @export
path <- function(inside = "data_raw", ...) {
  fs::path("/", inside, config$package, ...)
}
