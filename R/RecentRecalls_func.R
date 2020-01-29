#install.packages("httr")
#install.packages("jsonlite")

require("httr")
require("jsonlite")

recentrecall_api <- function(lang = NULL) {
  lang = NULL
  if (is.null(lang)){
    lang <- "en"
  }
  if (lang %in% c("en","fr")) {
    lang <- lang
  } else {
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
  
  #Remove column
  df_recentrecall <- df_recentrecall[ -c(5,10,15,20,25) ]
  
  #Rename colum name
  #ALL
  names(df_recentrecall)[1] <- "all_recallId"
  names(df_recentrecall)[2] <- "all_title"
  names(df_recentrecall)[3] <- "all_category"
  names(df_recentrecall)[4] <- "all_date_published"
  #FOOD
  names(df_recentrecall)[5] <- "food_recallId"
  names(df_recentrecall)[6] <- "food_title"
  names(df_recentrecall)[7] <- "food_category"
  names(df_recentrecall)[8] <- "food_date_published"
  #VEHICLE
  names(df_recentrecall)[9]  <- "vehicle_recallId"
  names(df_recentrecall)[10] <- "vehicle_title"
  names(df_recentrecall)[11] <- "vehicle_category"
  names(df_recentrecall)[12] <- "vehicle_date_published"
  #HEALTH
  names(df_recentrecall)[13] <- "health_recallId"
  names(df_recentrecall)[14] <- "health_title"
  names(df_recentrecall)[15] <- "health_category"
  names(df_recentrecall)[16] <- "health_date_published"
  #CPS
  names(df_recentrecall)[17] <- "cps_recallId"
  names(df_recentrecall)[18] <- "cps_title"
  names(df_recentrecall)[19] <- "cps_category"
  names(df_recentrecall)[20] <- "cps_date_published"
  
  return(df_recentrecall)
  
}

#xxx <- recentrecall_api()
#View(xxx)
