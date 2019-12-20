

  #url <- glue::glue("https://www.vegvesen.no/ws/no/vegvesen/veg/parkeringsomraade/parkeringsregisteret/v1/parkeringstilbyder")
  #content <- httr::GET(url)

library(httr)
  url <- glue::glue("https://www.vegvesen.no/trafikkdata/api/")
  #content <- httr::GET(url)
  options(curl_interrupt = FALSE)
  query <- '{ "query": "{ trafficRegistrationPoints(searchQuery: {roadCategoryIds: [F] }) { id name location { coordinates { latLon { lat lon } } } } }", "variables": null}'
  "{ trafficRegistrationPoints(searchQuery: {roadCategoryIds: [F] }){ id name location { coordinates { latLon { lat lon } } } } }"
  resp <-httr::GET(url,query=query)

  http_type(resp)

