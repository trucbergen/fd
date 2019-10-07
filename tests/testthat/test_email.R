context("fd")
library(mockery)

test_that("Test mailgun", {
  httr_mock <- mock(TRUE, cycle = TRUE)

  stub(mailgun, "httr::POST", httr_mock)
  stub(mailgun, "e_url", mock("http://testurl.com", cycle = TRUE))
  stub(mailgun, "e_key", mock("test-key", cycle = TRUE))
  mailgun(
    "Test email",
    html = "test_email"
  )

  expect_args(httr_mock, 1,
    url = "http://testurl.com",
    httr::authenticate("api", "test-key"),
    encoding = "multipart",
    body = list(
      from = Sys.getenv("MAILGUN_FROM", "X"),
      subject = glue::glue("TEST: Test email"),
      html = glue::glue("test_email<br><br>\nDO NOT REPLY TO THIS EMAIL! This email address is not checked by anyone!\n<br>\nTo add or remove people to/from this notification list, send their details to richard.white@fhi.no"),
      to = NULL
    )
  )

  config$is_production <- TRUE
  mailgun(
    "Test email",
    html = "test_email"
  )
  expect_args(httr_mock, 2,
    url = "http://testurl.com",
    httr::authenticate("api", "test-key"),
    encoding = "multipart",
    body = list(
      from = Sys.getenv("MAILGUN_FROM", "X"),
      subject = "Test email",
      html = glue::glue("test_email<br><br>\nDO NOT REPLY TO THIS EMAIL! This email address is not checked by anyone!\n<br>\nTo add or remove people to/from this notification list, send their details to richard.white@fhi.no"),
      to = NULL
    )
  )

  mailgun(
    "Test email",
    html = "test_email",
    is_final = FALSE
  )

  expect_args(httr_mock, 3,
    url = "http://testurl.com",
    httr::authenticate("api", "test-key"),
    encoding = "multipart",
    body = list(
      from = Sys.getenv("MAILGUN_FROM", "X"),
      subject = glue::glue("PRELIMINARY: Test email"),
      html = glue::glue("test_email<br><br>\nDO NOT REPLY TO THIS EMAIL! This email address is not checked by anyone!\n<br>\nTo add or remove people to/from this notification list, send their details to richard.white@fhi.no"),
      to = NULL
    )
  )
})
