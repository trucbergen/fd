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
    db_config = NULL,
    db_table = NULL,
    db_field_types = NULL,
    db_load_folder = NULL,
    keys = NULL,
    keys_with_length = NULL,
    check_fields_match = FALSE,
    initialize = function(dt = NULL, conn = NULL, db_config = NULL, db_table, db_field_types, db_load_folder, keys, check_fields_match = TRUE) {
      self$dt <- dt
      self$conn <- conn
      self$db_config <- db_config
      self$db_table <- db_table
      self$db_field_types <- db_field_types
      self$db_load_folder <- db_load_folder
      self$keys <- keys
      self$keys_with_length <- keys
      self$check_fields_match <- check_fields_match

      ind <- self$db_field_types[self$keys] == "TEXT"
      self$keys_with_length[ind] <- paste0(self$keys_with_length[ind], " (20)")
      if (!is.null(self$conn)) self$db_create_table()
    },
    db_connect = function(db_config = self$db_config) {
      self$conn <- get_db_connection(
        driver = db_config$driver,
        server = db_config$server,
        port = db_config$port,
        user = db_config$user,
        password = db_config$password
      )
      fd:::use_db(self$conn, db_config$db)
      self$db_create_table()
    },
    db_create_table = function() {
      create_tab <- TRUE
      if (DBI::dbExistsTable(self$conn, self$db_table)) {
        if (self$check_fields_match & !self$db_check_fields_match()) {
          message(glue::glue("Dropping table {self$db_table} because fields dont match"))
          self$db_drop_table()
        } else {
          create_tab <- FALSE
        }
      }

      if (create_tab) {
        message(glue::glue("Creating table {self$db_table}"))
        create_table(self$conn, self$db_table, self$db_field_types)
        add_constraint(self$conn, self$db_table, self$keys_with_length)
      }
    },
    db_drop_table = function() {
      if (DBI::dbExistsTable(self$conn, self$db_table)) {
        DBI::dbRemoveTable(self$conn, self$db_table)
      }
    },
    db_check_fields_match = function() {
      fields <- DBI::dbListFields(self$conn, self$db_table)
      if (sum(!fields %in% names(self$db_field_types)) > 0 | sum(!names(self$db_field_types) %in% fields) > 0) {
        return(FALSE)
      }
      return(TRUE)
    },
    db_load_data_infile = function(newdata) {
      a <- Sys.time()
      infile <- random_file(self$db_load_folder)
      write_data_infile(
        dt = newdata,
        file = infile
      )
      load_data_infile(
        conn = self$conn,
        table = self$db_table,
        dt = NULL,
        file = infile
      )
      b <- Sys.time()
      dif <- round(as.numeric(difftime(b, a, units = "secs")), 1)
      message(glue::glue("Loaded {nrow(newdata)} rows in {dif} seconds"))
    },
    db_upsert_load_data_infile = function(newdata, drop_indexes = NULL) {
      a <- Sys.time()
      infile <- random_file(self$db_load_folder)
      write_data_infile(
        dt = newdata[, names(self$db_field_types), with = F],
        file = infile
      )
      upsert_load_data_infile(
        db_config = self$db_config,
        table = self$db_table,
        dt = NULL,
        file = infile,
        fields = names(self$db_field_types),
        drop_indexes = drop_indexes
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
    dplyr_tbl = function() {
      retval <- self$conn %>%
        dplyr::tbl(self$db_table)
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
