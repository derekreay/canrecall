library(httr)
library(anytime)

recall_date_api <- function(search = NULL, lang = NULL, cat = NULL, lim = NULL, datestart = NULL, dateend = NULL) {
  #path <- '/recall-alert-rappel-avis/api/search?search=peanuts&lang=en&cat=1&lim=5&off=0'
  if (!is.null(search)) {
    search1 <- paste('search=', search, sep = '')
  }
  if (!is.null(lang)) {
    if (lang %in% c('en', 'fr')) {
      lang <- paste('&lang=', lang, sep = '')
    } else {
      lang <-'&lang=en'
    }
  }
  if (!is.null(cat)) {
    if (cat %in% c(1,2,3,4)) {
      cat <- paste('&cat=', cat, sep = '')
    } else {
      cat <- NULL
    }
  }
  if (!is.null(lim)) {
    if (is.numeric(lim)) {
      lim <- paste('&lim=', lim, sep = '')
    } else {
      lim <- NULL
    }
  }
  path1 <- '/recall-alert-rappel-avis/api/'
  path <- paste(path1, 'search?', search1, lang, cat, lim, sep = '')
  print(path)
  url <- modify_url('https://healthycanadians.gc.ca/recall-alert-rappel-avis/api/', path = path)
  resp <- GET(url)
  
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- jsonlite::fromJSON(url)
  
  if (http_error(resp)) {
    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        status_code(resp),
        resp$request
      ),
      call. = FALSE
    )
  }
  
  parsed <- as.data.frame(parsed[1])
  names(parsed) <- gsub(x = names(parsed), pattern = ".*\\.", replacement = "")
  parsed$date_published <- anytime(parsed$date_published)
  
  #date stuff
  is.POSIXct <- function(x) inherits(x, "POSIXct")
  
  datestart <- anytime(datestart)
  if (is.POSIXct(datestart) == FALSE){
    cat('datestart entered in incorrect format')
  }
  
  dateend <- anytime(dateend)
  if (is.POSIXct(dateend) == FALSE){
    cat('dateend entered in incorrect format')
  }
  
  parsed[parsed$date_published > datestart & parsed$date_published < dateend,]
  
  
}

