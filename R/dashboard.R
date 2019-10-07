

#' initialize
#' @param package a
#' @param package_dir a
#' @param load_package Load the package or not
#' @export
initialize <- function(
                       package,
                       package_dir = paste0("/dashboards/dashboards_", package),
                       load_package = TRUE) {
  config$package <- package

  if (config$is_dev) {
    msg(glue::glue("DEV computer name is: '{config$name_computer}'"))
    options(error = traceback)
  }

  if (load_package) {
    if (config$is_dev) {
      tryCatch(
        suppressPackageStartupMessages(devtools::load_all(package_dir, export_all = FALSE, quiet = TRUE)),
        error = function(x) {
          msg("Failed to load from folder, now loading from package library")
          suppressPackageStartupMessages(library(package, character.only = TRUE))
        }
      )
    } else {
      suppressPackageStartupMessages(library(package, character.only = TRUE))
    }
  }

  config$is_initialized <- TRUE
}

#' Enhanced Messaging
#' @param txt Text
#' @param type msg, warn, err
#' @param syscalls_depth The number of syscalls included in the message. Set to 0 to disable.
#' @param newLine Should there be a new line at the start of the message?
#' @param slack Should this also be posted to slack?
#' @export msg
msg <- function(txt, type = "msg", syscalls_depth = 0, newLine = FALSE, slack = FALSE) {
  if (slack & config$is_production) slack(glue::glue("{Sys.time()}: {txt}"))

  # make warnings print immediately
  op <- options("warn")
  on.exit(options(op))
  options(warn = 1)

  if (syscalls_depth < 0) stop("syscalls_depth cannot be less than zero")
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
  if (depth >= 1 & syscalls_depth > 0) {
    depthSeq <- depth:1
    if (length(depthSeq) > syscalls_depth) depthSeq <- depthSeq[1:syscalls_depth]
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
#' @param ... an optional path/file
#' @param package The name of the package
#' @export
path <- function(inside = "data_raw", ..., package = config$package) {
  fs::path("/", inside, package, ...)
}

#' Results folder
#'
#' This function gets the right folder for results 
#' @param results_folder_name name of the results folder
#' @param date the date of extraction
#' @export
results_folder <- function(results_folder_name, date){
  fd::path("results",
           results_folder_name,
           date
           )
}

#' Create latest folder
#' 
#' This function copies results_folder/date til results_folder/latest
#' @param results_folder_name name of the results folder
#' @param date the date of extraction
#' @export
create_latest_folder <- function(results_folder_name, date){
  from_folder <- results_folder(results_folder_name, date)
  to_folder <- fd::path("results", results_folder_name, "latest")
  processx::run("cp", c("-rT", from_folder, to_folder))
  
}
