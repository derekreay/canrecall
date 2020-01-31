library(httr)
library(anytime)

recall_date_api <- function(search = NULL, lang = NULL, cat = NULL, datestart = NULL, dateend = NULL) {
  #handle errors on input
  if (!is.null(search)) {
    search1 <- paste('search=', search, sep = '')
  }
  if (!is.null(lang)) {
    if (lang %in% c('en', 'fr')) {
      lang <- paste('&lang=', lang, sep = '')
    } else {
      warning("lang not a supported language input. Defaulting search to en for english")
      lang <-'&lang=en'
    }
  }
  if (!is.null(cat)) {
    if (cat %in% c(1,2,3,4)) {
      cat <- paste('&cat=', cat, sep = '')
    } else {
      warning("Only values of 1,2,3 or 4 for cat or category of recall are accepted, searching with cat set to NULL")
      cat <- NULL
    }
  }
 
  path1 <- '/recall-alert-rappel-avis/api/'
  path <- paste(path1, 'search?', search1, lang, cat, '&lim=99999', sep = '')
  print(path)
  url <- modify_url('https://healthycanadians.gc.ca/recall-alert-rappel-avis/api/', path = path)
  response_recalldetail <- GET(url)
  
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
  
  get_recalldetail_json <- fromJSON(get_recalldetail_text, flatten = TRUE)
  df_recalldetail <- as.data.frame(get_recalldetail_json)
  
  #name columns
  names(df_recalldetail) <- gsub(x = names(df_recalldetail), pattern = ".*\\.", replacement = "")
  #convert datetime object to useful date
  df_recalldetail$date_published <- anytime(df_recalldetail$date_published)
  
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
  
  df_recalldetail<-df_recalldetail[df_recalldetail$date_published > datestart & df_recalldetail$date_published < dateend,]
  
  #return dataframe without counts column
  return(df_recalldetail[-7])
}

#resp2 <- recall_date_api(search = 'peanuts', cat = '1', lang = 'en', datestart = '2017-12-19', dateend = '2019-02-07')
#resp2


