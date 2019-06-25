#' schema class description
#'
#' @importFrom methods new
#' @import RMariaDB
#' @export schema
#' @exportClass schema
schema <- setRefClass("schema",
  fields = list(
    dt = "data.frame",
    db = "MariaDBConnection",
    db_table = "character",
    keys = "vector"
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)
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
      if (!is.null(nrow(.self$dt))){
        x <- .self$get_data_dt(dots)
      } else {
        x <- .self$get_data_db(dots)
      }
      return(x)
    },
    get_data_dt = function(dots) {
      txt <- c()
      for (i in seq_along(dots)) {
        txt <- c(txt, rlang::quo_text(dots[[i]]))
      }
      if (length(txt) == 0) {
        return(.self$dt)
      } else {
        txt <- paste0(txt, collapse = "&")
        return(.self$dt[eval(parse(text = txt))])
      }
    },
    get_data_db = function(dots) {
      retval <- .self$db %>%
        dplyr::tbl(db_table) %>%
        dplyr::filter(!!!dots) %>%
        dplyr::collect()
      # setDT(retval)
      return(retval)
    },

    drop_data_db = function(db_table_to_drop = NULL) {
      if (is.null(db_table_to_drop)) db_table_to_drop <- .self$db_table
      if (DBI::dbExistsTable(.self$db, db_table_to_drop)) {
        DBI::dbExecute(.self$db, DBI::SQL(glue::glue("DROP TABLE {db_table_to_drop}")))
      }
    },
    delete_data_db = function(newdata) {
      todelete <- paste0("a", stringr::str_remove_all(uuid::UUIDgenerate(), "-"))
      DBI::dbWriteTable(
        .self$db,
        todelete,
        newdata
      )

      query <- glue::glue_collapse(glue::glue("{.self$db_table}.{.self$keys} = {todelete}.{.self$keys}"), sep = " AND ")
      DBI::dbExecute(.self$db, DBI::SQL(glue::glue("
                             DELETE {.self$db_table}
                             FROM {.self$db_table}
                             INNER JOIN {todelete} ON {query}
                          ")))

      .self$drop_data_db(todelete)
    },
    upload_data_db = function(newdata) {
      .self$drop_data_db()

      DBI::dbWriteTable(
        .self$db,
        .self$db_table,
        newdata
      )
    },
    replace_data_db = function(newdata) {
      if (!DBI::dbExistsTable(.self$db, .self$db_table)) {
        stop(glue::glue("{.self$db_table} does not exist"))
      }
      .self$delete_data_db(newdata)
      DBI::dbAppendTable(db, .self$db_table, newdata)
    },
    add_index_db = function() {
      txt <- glue::glue_collapse(glue::glue("`{.self$keys}`(20)"), sep = ",")
      RMariaDB::dbExecute(
        .self$db,
        glue::glue("ALTER TABLE `{.self$db_table}` ADD INDEX `ind1` ({txt})")
      )
    }
  )
)
