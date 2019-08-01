get_mailgun_info <- function() {
  if (file.exists("/etc/mailgun/mailgun.txt")) {
    con <- file("/etc/mailgun/mailgun.txt", "r")
    url <- readLines(con, n = 1)
    from <- readLines(con, n = 1)
    api_key <- readLines(con, n = 1)
    close(con)

    config$mailgun_url <- glue::glue("{url}/messages")
    config$mailgun_from <- from
    config$mailgun_apikey <- api_key
  }
}

e_url <- function() {
  if (config$mailgun_url == "x") {
    get_mailgun_info()
  }
  return(config$mailgun_url)
}

e_from <- function() {
  if (config$mailgun_from == "x") {
    get_mailgun_info()
  }
  return(config$mailgun_from)
}

e_key <- function() {
  if (config$mailgun_apikey == "x") {
    get_mailgun_info()
  }
  return(config$mailgun_apikey)
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
