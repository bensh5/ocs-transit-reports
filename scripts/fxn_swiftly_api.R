source("scripts/source.R")

# SWIFTLY API CALL FUNCTIONS

test_swiftly <- function(api_key) {
  url <- "https://api.goswift.ly/test-key"
  
  response <- VERB("GET", url, add_headers(Authorization = api_key, ''), content_type("application/octet-stream"))
  
  return(response)
}


