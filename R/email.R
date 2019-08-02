e_url <- function() {
  Sys.getenv("MAILGUN_URL","X")
}

e_from <- function() {
  Sys.getenv("MAILGUN_FROM","X")
}

e_key <- function() {
  Sys.getenv("MAILGUN_APIKEY","X")
}

e_subject <- function(subject) {
  if (!config$is_production) {
    subject <- glue::glue("TEST: {subject}")
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
#' @param include_footer a
#' @param ... a
#' @export
mailgun <- function(subject, html = " ", to = NULL, bcc = NULL, include_footer = T, ...) {
  if (is.null(to) & !is.null(bcc)) to <- "dashboardsfhi@gmail.com"
  if (include_footer) {
    html <- glue::glue(html, e_footer)
  }

  if (!is.null(to)) to <- glue::glue_collapse(to, sep = ",")
  if (!is.null(bcc)) bcc <- glue::glue_collapse(bcc, sep = ",")

  httr::POST(
    url = e_url(),
    httr::authenticate("api", e_key()),
    encode = "multipart",
    body = list(
      from = e_from(),
      subject = e_subject(subject),
      html = html,
      to = to,
      bcc = bcc,
      ...
    )
  )
}

#' e_emails
#' @param project a
#' @export
e_emails <- function(project) {
  if (config$is_production) {
    emails <- readxl::read_excel("/etc/gmailr/emails.xlsx")
  } else {
    emails <- readxl::read_excel("/etc/gmailr/emails_test.xlsx")
  }

  emails <- stats::na.omit(emails[[project]])

  return(emails)
}
