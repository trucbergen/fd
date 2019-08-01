#' Flags/values to be used in the 'dashboards' scene
#' @export config
config <- new.env()

config$package <- "x"

config$name_computer <- "x"
config$name_production <- "smhb"
config$name_testing <- c("linux", "test", "temp")

config$is_production <- FALSE
config$is_testing <- FALSE
config$is_dev <- FALSE

config$is_initialized <- FALSE

config$mailgun_url <- "x"
config$mailgun_from <- "x"
config$mailgun_apikey <- "x"

config$frost_client_id <- Sys.getenv("FROST_CLIENT_ID", "c6d9bf2d-104c-4b5f-accf-d367b2220d62")
config$db_config <- list(
  driver = Sys.getenv("DB_DRIVER", "MySQL"),
  server = Sys.getenv("DB_SERVER", "db"),
  port = as.integer(Sys.getenv("DB_PORT", 3306)),
  user = Sys.getenv("DB_USER", "root"),
  password = Sys.getenv("DB_PASSWORD", "example"),
  db = Sys.getenv("DB_DB", "sykdomspuls")
)


#' Environment to store logs
#' test
#' @export logdata
logdata <- new.env()
logdata$x <- 1
