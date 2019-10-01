e_url <- function() {
  glue::glue("{base_url}/messages", base_url = Sys.getenv("MAILGUN_URL", "X"))
}

e_from <- function() {
  Sys.getenv("MAILGUN_FROM", "X")
}

e_key <- function() {
  Sys.getenv("MAILGUN_APIKEY", "X")
}

e_subject <- function(subject, is_final = TRUE) {
  today <- lubridate::wday(lubridate::today(), week_start = 1)

  if (!config$is_production) {
    subject <- glue::glue("TEST: {subject}")
  }
  if (config$is_production & !is_final) {
    subject <- glue::glue("PRELIMINARY: {subject}")
  }
  return(subject)
}

e_footer <- function() {
  return(glue::glue("
<br><br>
  DO NOT REPLY TO THIS EMAIL! This email address is not checked by anyone!
  <br>
  To add or remove people to/from this notification list, send their details to richard.white@fhi.no
"))
}


#' mailgun
#' @param subject a
#' @param html a
#' @param to a
#' @param bcc a
#' @param attachments a
#' @param inlines a
#' @param include_footer a
#' @param is_final Is this a final or preliminary email?
#' @param ... a
#' @export
mailgun <- function(
                    subject,
                    html = " ",
                    to = NULL,
                    bcc = NULL,
                    attachments = NULL,
                    inlines = NULL,
                    include_footer = T,
                    is_final = TRUE,
                    ...) {
  if (is.null(to) & !is.null(bcc)) to <- "dashboardsfhi@gmail.com"
  if (include_footer) {
    html <- glue::glue(html, e_footer())
  }

  if (!is.null(to)) to <- glue::glue_collapse(to, sep = ",")
  if (!is.null(bcc)) bcc <- glue::glue_collapse(bcc, sep = ",")

  body <- list(
    from = e_from(),
    subject = e_subject(subject, is_final = is_final),
    html = html,
    to = to,
    bcc = bcc,
    ...
  )

  if (!is.null(attachments)) {
    att <- vector("list", length = length(attachments))
    for (i in seq_along(attachments)) {
      att[[i]] <- httr::upload_file(attachments[i])
    }
    names(att) <- rep("attachment", length(att))
    body <- c(body, att)
  }

  if (!is.null(inlines)) {
    inl <- vector("list", length = length(inlines))
    for (i in seq_along(inlines)) {
      inl[[i]] <- httr::upload_file(inlines[i])
    }
    names(inl) <- rep("inline", length(inl))
    body <- c(body, inl)
  }

  if (is.null(bcc)) body <- body[names(body) != "bcc"]

  httr::POST(
    url = e_url(),
    httr::authenticate("api", e_key()),
    encode = "multipart",
    body = body
  )
}

#' e_emails
#' @param project a
#' @param is_final Is final or preliminary email?
#' @export
e_emails <- function(project, is_final = TRUE) {
  if (config$is_production & is_final) {
    email_loc <- "/etc/gmailr/emails.xlsx"
  } else {
    email_loc <- "/etc/gmailr/emails_test.xlsx"
  }

  # do this to try and get around a permission error
  temp_loc <- fs::path(tempdir(), glue::glue("{uuid::UUIDgenerate()}.xlsx"))
  fs::file_copy(email_loc, temp_loc)

  fhi::with_dir(
    tempdir(),
    emails <- readxl::read_excel(temp_loc)
  )

  fs::file_delete(temp_loc)

  emails <- stats::na.omit(emails[[project]])

  return(emails)
}
