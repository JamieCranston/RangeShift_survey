
load_config <- function(){
  
  config <- yaml::read_yaml("config.yaml")
  
  return(config)
}
