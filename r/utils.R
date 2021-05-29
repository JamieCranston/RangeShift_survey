
load_config <- function(){
  
  config <- yaml::read_yaml("config.yaml")
  
  return(config)
}

getOrdinalNumber <- function(num) {
  result <- ""
  if (!(num %% 100 %in% c(11, 12, 13))) {
    result <- switch(as.character(num %% 10), 
                     "1" = {paste0(num, "st")}, 
                     "2" = {paste0(num, "nd")},
                     "3" = {paste0(num, "rd")},
                     paste0(num,"th"))
  } else {
    result <- paste0(num, "th")
  }
  return(result)
}
