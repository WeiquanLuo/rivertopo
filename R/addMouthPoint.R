#' Add Mount point to site location datafarme
#' @importFrom magrittr %>%
#' @importFrom rlang set_names
#' @importFrom dplyr mutate
#' @export
#' @param df a dataframe containing sit information with columns: id, long, lat
#' @param long the longitude of the adding Mount point
#' @param lat the latitude of the adding Mount point
#' @param buff if buff = 1, add one point;
#' if buff = 2, then add 9 point around the given long & lat.

addMouthPoint <- function(df, long = -90.199402, lat = 38.627003, buff = 1){
  if (buff==1){
    mouthpoint <- cbind(id = "rivermouth", expand.grid(x=long, y=lat)) %>%
      set_names(colnames(df))
  } else if (buff==2){
    mouthpoint <- cbind(id =paste0("rivermouth",c(1:9)),
                        expand.grid(x=c(long, long+1, long-1),
                                    y=c(lat, lat-1,lat+1))) %>%
      set_names(colnames(df))
  } else{
    return(df)
  }
  df <- rbind(mouthpoint, df)
  return(df)
}
