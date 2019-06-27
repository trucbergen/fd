#' schema class description
#'
#' @import data.table
#' @import R6
#' @export schema
#' @exportClass schema
schema <- R6Class("schema",
  public = list(
    dt = NULL,
    conn = NULL,
    db_table = NULL,
    db_field_types = NULL,
    db_load_file = NULL,
    keys = NULL,
    keys_with_length = NULL,
    initialize = function(dt = NULL, conn = NULL, db_table, db_field_types, db_load_file, keys) {
      self$dt <- dt
      self$conn <- conn
      self$db_table <- db_table
      self$db_field_types <- db_field_types
      self$db_load_file <- db_load_file
      self$keys <- keys
      self$keys_with_length <- keys

      ind <- self$db_field_types[self$keys] == "TEXT"
      self$keys_with_length[ind] <- paste0(self$keys_with_length[ind], " (20)")
      message(self$keys_with_length)
      if (!is.null(self$conn)) self$db_create_table()
    },
    db_connect = function(db_config){
        self$conn <- get_db_connection(
          driver = db_config$driver,
          server = db_config$server,
          port = db_config$port,
          user = db_config$user,
          password = db_config$password
        )
        fd:::use_db(self$conn, db_config$db)
        
    },
    db_create_table = function() {
      if (DBI::dbExistsTable(self$conn, self$db_table)) {
        message(glue::glue("Table {self$db_table} exists"))
      } else {
        message(glue::glue("Creating table {self$db_table}"))
        sql <- DBI::sqlCreateTable(self$conn, self$db_table, self$db_field_types,
          row.names = F, temporary = F
        )
        DBI::dbExecute(self$conn, sql)
        add_constraint(self$conn, self$db_table, self$keys_with_length)
      }
    },
    db_drop_table = function() {
      if (DBI::dbExistsTable(self$conn, self$db_table)) {
        DBI::dbRemoveTable(self$conn, self$db_table)
      }
    },
    db_load_data_infile = function(newdata) {
      a <- Sys.time()
      load_data_infile(
        conn = self$conn,
        table = self$db_table,
        dt = newdata,
        file = self$db_load_file
      )
      b <- Sys.time()
      dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)
      message(glue::glue("Loaded {nrow(newdata)} rows in {dif} seconds"))
    },
    db_upsert_load_data_infile = function(newdata) {
      a <- Sys.time()
      upsert_load_data_infile(
        conn = self$conn,
        table = self$db_table,
        dt = newdata[, names(self$db_field_types), with = F],
        file = self$db_load_file,
        fields = names(self$db_field_types)
      )
      b <- Sys.time()
      dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)
      message(glue::glue("Loaded {nrow(newdata)} rows in {dif} seconds"))
    },
    db_drop_all_rows = function() {
      drop_all_rows(self$conn, self$db_table)
    },
    get_data = function(...) {
      dots <- dplyr::quos(...)
      params <- c()

      for (i in seq_along(dots)) {
        temp <- rlang::quo_text(dots[[i]])
        temp <- stringr::str_extract(temp, "^[a-zA-Z0-9]+")
        params <- c(params, temp)
      }

      if (length(params) > length(keys)) {
        stop("Too many requests")
      }
      if (sum(!params %in% keys)) {
        stop("names(...) not in keys")
      }
      if (nrow(self$dt) > 0 | ncol(self$dt) > 0) {
        x <- self$get_data_dt(...)
      } else {
        x <- self$get_data_db(...)
      }
      return(x)
    },
    get_data_dt = function(...) {
      dots <- dplyr::quos(...)
      txt <- c()
      for (i in seq_along(dots)) {
        txt <- c(txt, rlang::quo_text(dots[[i]]))
      }
      if (length(txt) == 0) {
        return(self$dt)
      } else {
        txt <- paste0(txt, collapse = "&")
        return(self$dt[eval(parse(text = txt))])
      }
    },
    get_data_db = function(...) {
      dots <- dplyr::quos(...)
      retval <- self$conn %>%
        dplyr::tbl(self$db_table) %>%
        dplyr::filter(!!!dots) %>%
        dplyr::collect()
      setDT(retval)
      return(retval)
    },

    add_index_db = function() {
      txt <- glue::glue_collapse(glue::glue("`{self$keys}`(20)"), sep = ",")
      DBI::dbExecute(
        self$conn,
        glue::glue("ALTER TABLE `{self$db_table}` ADD INDEX `ind1` ({txt})")
      )
    },

    identify_dt_that_exists_in_db = function() {
      setkeyv(self$dt, self$keys)
      from_db <- self$get_data_db()
      setkeyv(from_db, self$keys)
      self$dt[, exists_in_db := FALSE]
      self$dt[from_db, exists_in_db := TRUE]
    }
  )
)
