#' recall_api
#'
#' A function to return recall data from the Canadian Recalls and Safety Alerts API
#'
#' The response is a dataframe
#'
#' @param cat integer variable as 1 = FOOD, 2 = VEHICLES,
#' 3 = HEALTH PRODUCTS, 4 = CONSUMER PRODUCTS
#' @param lang string variable as 'en' = English, 'fr' = French,
#' for response language
#' @param lim integer variable to limit results to n
#' @param search string variable to search the database for
#'
#' @import httr
#' @import anytime
#' @import jsonlite
#'
#' @export
#'
#' @examples
#' recall_api('banana', 'en', 1, 10)
#' ## will return 10 recall results for 'banana' in english
#'

recall_api <- function(search = NULL, lang = NULL, cat = NULL, lim = NULL) {
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
  if (!is.null(lim)) {
    if (is.numeric(lim)) {
      lim <- paste('&lim=', lim, sep = '')
    } else {
      warning("Lim only accepts integers, With incorrect input will only search for 5")
      lim <- NULL
    }

  }
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

  #return dataframe without counts column
  return(df_recalldetail[-7])

}

