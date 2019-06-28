#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' use_db
#' @param conn a
#' @param db a
#' @export
use_db <- function(conn, db) {
  a <- DBI::dbGetQuery(conn, glue::glue({
    "SHOW DATABASES LIKE '{db}';"
  }))
  if (nrow(a) == 0) {
    a <- DBI::dbExecute(conn, glue::glue({
      "CREATE DATABASE {db};"
    }))
  }
  a <- DBI::dbExecute(conn, glue::glue({
    "USE {db};"
  }))
}

#' get_field_types
#' @param conn a
#' @param dt a
get_field_types <- function(conn, dt) {
  field_types <- vapply(dt, DBI::dbDataType,
    dbObj = conn,
    FUN.VALUE = character(1)
  )
  return(field_types)
}


load_data_infile <- function(conn, table, dt, file = "/xtmp/x123.csv") {
  fwrite(dt, file = file,
         logical01 = T,
         na = "\\N")

  sep <- ","
  eol <- "\n"
  quote <- '"'
  skip <- 0
  header <- T
  path <- normalizePath(file, winslash = "/", mustWork = TRUE)
  
  sql <- paste0(
    "LOAD DATA INFILE ", DBI::dbQuoteString(conn, path), "\n",
    "INTO TABLE ", DBI::dbQuoteIdentifier(conn, table), "\n",
    "FIELDS TERMINATED BY ", DBI::dbQuoteString(conn, sep), "\n",
    "OPTIONALLY ENCLOSED BY ", DBI::dbQuoteString(conn, quote), "\n",
    "LINES TERMINATED BY ", DBI::dbQuoteString(conn, eol), "\n",
    "IGNORE ", skip + as.integer(header), " LINES \n",
    "(",  paste0(names(dt), collapse = ","), ")"
  )
  DBI::dbExecute(conn, sql)
}

upsert_load_data_infile <- function(conn, table, dt, file = "/xtmp/x123.csv", fields) {
  temp_name <- paste0("a", round(stats::runif(1, 0, 1000000)))
  on.exit(DBI::dbRemoveTable(conn, temp_name))

  sql <- glue::glue("CREATE TEMPORARY TABLE {temp_name} LIKE {table};")
  DBI::dbExecute(conn, sql)

  # TO SPEED UP EFFICIENCY DROP ALL INDEXES HERE

  load_data_infile(conn = conn, temp_name, dt, file = file)

  vals_fields <- glue::glue_collapse(fields, sep = ", ")
  vals <- glue::glue("{fields} = VALUES({fields})")
  vals <- glue::glue_collapse(vals, sep = ", ")

  sql <- glue::glue("
    INSERT INTO {table} SELECT {vals_fields} FROM {temp_name}
    ON DUPLICATE KEY UPDATE {vals};
    ")
  DBI::dbExecute(conn, sql)
}

drop_all_rows <- function(conn, table) {
  a <- DBI::dbExecute(conn, glue::glue({
    "DELETE FROM {table};"
  }))
}

add_constraint <- function(conn, table, keys) {
  primary_keys <- glue::glue_collapse(keys, sep = ", ")
  sql <- glue::glue("
          ALTER table {table}
          ADD CONSTRAINT X_CONSTRAINT_X PRIMARY KEY ({primary_keys});")
  a <- DBI::dbExecute(conn, sql)
  # DBI::dbExecute(conn, "SHOW INDEX FROM x");
}

#' @export get_db_connection
get_db_connection <- function(driver="MySQL",
                              server="db",
                              port=3306,
                              user="root",
                              password="example")
  return(DBI::dbConnect(odbc::odbc(),
                     driver = driver,
                     server = server,
                     port = port,
                     user = user,
                     password = password))
  
