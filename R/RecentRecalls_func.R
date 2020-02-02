library(anytime)

recentrecall_api <- function(lang = NULL) {
  #handle errors in language input
  if (!is.null(lang)) {
    if (lang %in% c('en', 'fr')) {
      lang <- lang
    } else {
      warning("lang not a supported language input. Defaulting search to en for english")
      lang <-'en'
    }
  }
  if (is.null(lang)){
    lang <- "en"
  }

  base <- "http://healthycanadians.gc.ca/recall-alert-rappel-avis"
  endpoint <- "/api/recent/"

  urlApi <- paste(base,endpoint,lang, sep="")
  #urlApi

  response_recentrecall <- GET(urlApi)
  #response_recentrecall

  if (http_type(response_recentrecall) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  if (http_error(response_recentrecall)) {
    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>",
        status_code(response_recentrecall),
        response_recentrecall$request
      ),
      call. = FALSE
    )
  }

  get_recentrecall_text <- content(response_recentrecall, as = "text", encoding = "UTF-8")
  get_recentrecall_text

  get_recentrecall_json <- fromJSON(get_recentrecall_text, flatten = TRUE)
  df_recentrecall <- as.data.frame(get_recentrecall_json)

  #Remove columns
  df_recentrecall <- df_recentrecall[ -c(5,10,15,20,25) ]

  #get names of first 4 columns
  columnnames <- names(df_recentrecall[,1:4])
  #create dataframe of first 4 columns
  df_format <- df_recentrecall[,1:4]

  #loop to make dataframe tall instead of wide for ease of use
  for (i in seq(5,17,4)) {
      temp <- df_recentrecall[,i:(i+3)]
      names(temp)<-columnnames
      df_format<-rbind(df_format,temp)
  }
  #rename columns
  names(df_format) <- gsub(x = names(df_format), pattern = ".*\\.", replacement = "")

  #convert date to something easier to interpret
  df_format$date_published <- anytime(df_format$date_published)

  return(df_format)

}

#xxx <- recentrecall_api(lang = 'es')
#View(xxx)


