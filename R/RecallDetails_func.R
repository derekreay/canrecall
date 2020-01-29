#install.packages("httr")
#install.packages("jsonlite")

require("httr")
require("jsonlite")

recalldetails_api <- function(search = NULL, lang = NULL, cat = NULL, lim = NULL, pOff = NULL) {
  if (!is.null(search)) {
    search <- paste('search=', search, sep = '')
  }else{
    search <- ''
  }
  
  if (!is.null(lang)) {
    if (lang %in% c('en', 'fr')) {
      if (search == ''){
        lang <- paste('lang=', lang, sep = '') 
      }else{
        lang <- paste('&lang=', lang, sep = '')
      }
    } else {
      if (search == ''){
        lang <-'lang=en'  
      }else{
        lang <-'&lang=en'
      }
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

  if (!is.null(pOff)) {
    if (is.numeric(pOff)) {
      pOff <- paste('&off=', lim, sep = '')
    } else {
      pOff <- NULL
    }
  }
  
    
  #http://healthycanadians.gc.ca/recall-alert-rappel-avis/api/search?search=peanuts&lang=en&cat=1&lim=5&off=0
  
  base <- "http://healthycanadians.gc.ca/recall-alert-rappel-avis"
  endpoint <- "/api/search?"
  
  urlApi <- paste(base,endpoint,search,lang,cat,lim,pOff, sep="")
  print(urlApi)
  
  response_recalldetail <- GET(urlApi)

  
  if (http_type(response_recalldetail) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  if (http_error(response_recalldetail)) {
    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>", 
        status_code(response_recalldetail),
        response_recalldetail$request
      ),
      call. = FALSE
    )
  }
  
  get_recalldetail_text <- content(response_recalldetail, as = "text", encoding = "UTF-8")
  #get_recentrecall_text
  
  get_recalldetail_json <- fromJSON(get_recalldetail_text, flatten = TRUE)
  df_recalldetail <- as.data.frame(get_recalldetail_json)
  
  names(df_recalldetail)[1] <- "recallId"
  names(df_recalldetail)[2] <- "title"
  names(df_recalldetail)[3] <- "department"
  names(df_recalldetail)[4] <- "date_published"
  names(df_recalldetail)[5] <- "category"
  names(df_recalldetail)[6] <- "url"
  names(df_recalldetail)[7] <- "count"
  
  return(df_recalldetail)
}

#xxx <- recalldetails_api('peanut', 'en', 1, 5, 0)
#xxx <- recalldetails_api('dog', 'fr', 1)
#xxx <- recalldetails_api('food', 'en', 2)
#View(xxx)
#search <- 'peanuts'
#lang <- 'en'
#cat <- 1
#lim <- 5
#off <- 0



