#' @importFrom magrittr %>%
#' @importFrom rlang set_names
#' @importFrom dplyr mutate

addMouthPoint <- function(df, long = -90.199402, lat = 38.627003, buff = 1){
  if (buff==1){
    mouthpoint <- cbind(id = "rivermouth", expand.grid(x=long, y=lat)) %>%
      set_names(colnames(df))
  } else if (buff==2){
    mouthpoint <- cbind(id =c(1:9), expand.grid(x=c(long, long+1, long-1),
                                                y=c(lat, lat-1,lat+1))) %>%
      mutate(id = paste0("rivermouth",id)) %>%
      set_names(colnames(df))
  } else{
    return(df)
  }
  df <- rbind(mouthpoint, df)
  return(df)
}
