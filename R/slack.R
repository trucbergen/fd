#' slack
#' @param txt a
#' @export
slack <- function(txt) {

  httr::POST(
    url = config$slack_webhook,
    encode = "json",
    body = list(
      text = txt
    )
  )
}
