#' recall_api_date
#'
#' A function to return recall data from the Canadian Recalls and Safety Alerts API, parsed by date range
#'dev
#' The result is a dataframe
#'
#' @param cat integer variable as 1 = FOOD, 2 = VEHICLES,
#' 3 = HEALTH PRODUCTS, 4 = CONSUMER PRODUCTS
#' @param lang string variable as 'en' = English, 'fr' = French,
#' for response language
#' @param search string variable to search the database for
#' @param datestart string variable to set earliest date in range
#' @param dateend string ariable to set latest date in range
#'
#' @import httr
#' @import anytime
#' @import jsonlite
#'
#' @export
#'
#' @examples
#' recall_api_date('banana', 'en', 1, '1/1/2019', '12/31/2019')
#' ## will return all recall results for 'banana' in 2019 in english
#'

recall_api_date <- function(search = NULL, lang = NULL, cat = NULL, datestart = NULL, dateend = NULL) {
  #handle errors on inputs
  if (!is.null(search)) {
    search <- paste('search=', search, sep = '')
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
  lim <- paste('&lim=', 99999, sep = '')

  path1 <- '/recall-alert-rappel-avis/api/'
  path <- paste(path1, 'search?', search, lang, cat, lim, sep = '')

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

  #is the date a POSIXct format date?
  is.POSIXct <- function(x) inherits(x, "POSIXct")

  #return incorrect format upon non POSIXct date
  #some incorrect dates can be entered, most of these are calculated integer values
  #ie 1/1/2010 or 1-1-1999. Since these will return small/decimal values, eliminate
  #date requests before the oldest data in the api, 1/1/1974
  datestart <- anytime(datestart)
  if (is.POSIXct(datestart) == FALSE){
    message('datestart entered in incorrect format')
  } else {
    if (as.POSIXct(datestart) <= as.POSIXct(anytime('1/1/1974'))){
      warning(paste('did you enter datestart incorrectly as ', datestart, '. No data exists before 1974'))
    }
  }

  dateend <- anytime(dateend)
  if (is.POSIXct(dateend) == FALSE){
    warning('dateend entered in incorrect format')
  } else {
    if (as.POSIXct(dateend) <= as.POSIXct(anytime('1/1/1974'))){
      warning(paste('did you enter dateend incorrectly as ', dateend, '. No data exists before 1974'))
    }
  }


  df_recalldetail<-df_recalldetail[df_recalldetail$date_published > datestart & df_recalldetail$date_published < dateend,]

  #return dataframe without counts column
  return(df_recalldetail[-7])

}

