#' schema class description
#'
#' @importFrom methods new
#' @import RMariaDB
#' @import data.table
#' @export schema
#' @exportClass schema
schema <- setRefClass("schema",
  fields = list(
    dt = "data.table",
    db = "MariaDBConnection",
    db_table = "character",
    keys = "vector"
  ),
  methods = list(
    initialize = function(...) {
      callSuper(...)

      if (nchar(.self$db_table) > 0) {
        if (!DBI::dbExistsTable(.self$db, .self$db_table)) {
          # {.self$db_table} does not exist
          if (!is.null(.self$dt)){
            message("initializing")
            .self$upload_empty_db(.self$dt)
          }
        }
      }
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
      if (nrow(.self$dt) > 0 | ncol(.self$dt) > 0) {
        x <- .self$get_data_dt(...)
      } else {
        x <- .self$get_data_db(...)
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
        return(.self$dt)
      } else {
        txt <- paste0(txt, collapse = "&")
        return(.self$dt[eval(parse(text = txt))])
      }
    },
    get_data_db = function(...) {
      dots <- dplyr::quos(...)
      retval <- .self$db %>%
        dplyr::tbl(db_table) %>%
        dplyr::filter(!!!dots) %>%
        dplyr::collect()
      setDT(retval)
      return(retval)
    },

    upload_data_db = function(newdata){
      newdata[,exists_in_db:=NULL]
      DBI::dbWriteTable(
        .self$db,
        .self$db_table,
        newdata
      )
    },

    upload_empty_db = function(skeleton){
      empty <- skeleton[1,]
      empty <- empty[-1,]
      .self$upload_data_db(empty)
    },

    drop_data_db = function(db_table_to_drop = NULL) {
      if (is.null(db_table_to_drop)) db_table_to_drop <- .self$db_table
      if (DBI::dbExistsTable(.self$db, db_table_to_drop)) {
        DBI::dbExecute(.self$db, DBI::SQL(glue::glue("DROP TABLE {db_table_to_drop}")))
      }
    },
    drop_matching_data_db = function(newdata) {
      newdata[,exists_in_db:=NULL]
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
    drop_all_and_upload_data_db = function(newdata) {
      .self$drop_data_db()

      upload_data_db(newdata)
    },
    drop_matching_and_append_data_db = function(newdata) {
      newdata[,exists_in_db:=NULL]
      if (!DBI::dbExistsTable(.self$db, .self$db_table)) {
        warning(glue::glue("{.self$db_table} does not exist"))
        upload_data_db(newdata)
      } else {
        .self$drop_matching_data_db(newdata)
        DBI::dbAppendTable(db, .self$db_table, newdata)
      }
    },
    add_index_db = function() {
      txt <- glue::glue_collapse(glue::glue("`{.self$keys}`(20)"), sep = ",")
      RMariaDB::dbExecute(
        .self$db,
        glue::glue("ALTER TABLE `{.self$db_table}` ADD INDEX `ind1` ({txt})")
      )
    },

    identify_dt_that_exists_in_db = function(){
      setkeyv(.self$dt,.self$keys)
      from_db <- .self$get_data_db()
      setkeyv(from_db,.self$keys)
      .self$dt[,exists_in_db:=FALSE]
      .self$dt[from_db,exists_in_db:=TRUE]
    }
  )
)
