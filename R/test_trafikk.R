url <- "https://www.vegvesen.no/trafikkdata/api/"
GQL <- function(query, url){
  pbody <- list(query = query)
  res <- httr::POST(url, body = pbody, encode="json")
  res <- httr::content(res, as = "parsed", encoding = "UTF-8")
  if(!is.null(res$errors)){
    warning(jsonlite::toJSON(res$errors))
  }
  purrr::flatten(res$data)
}


# Define queries ----------------------------------------------------------

query <- '{
                trafficData(trafficRegistrationPointId: "44656V72812") {
    volume {
      byHour(from: "2019-10-24T12:00:00+02:00", to: "2019-10-24T14:00:00+02:00") {
        edges {
          node {
            from
            to
            total {
              volume
              coverage {
                percentage
              }
            }
          }
        }
      }
    }
  }
}'


# Run queries -------------------------------------------------------------

a <- GQL(query, url)

