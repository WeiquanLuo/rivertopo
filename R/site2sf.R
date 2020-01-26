#' convert site dataframe to simple feature
#' @importFrom magrittr %>%
#' @importFrom dplyr rename select
#' @importFrom sf st_as_sf
#' @export
#' @param df a dataframe contain id, long, lat for sites
#' @param id_cn a string for the name of id column in df
#' @param Lon_cn a string for the name of the longitude column in df
#' @param Lat_cn a string for the name of the latitude column in df
#' @param crs a coordinate reference system for Map projections attempt to portray
#' the surface of the earth or a portion of the earth on a flat piece of paper or
#' computer screen.
#'
site2sf <- function(df,id_cn ="id", Lon_cn = "longitude", Lat_cn = "latitude", crs= 4326){
  # cleanup data
  df_locs <- df %>% rename(longitude=Lon_cn, latitude=Lat_cn, id = id_cn)
  # convert to sf object
  df_locs <- st_as_sf(df_locs,
                      coords = c("longitude", "latitude"), # for point data
                      remove = F, # don't remove these lat/lon cols from df
                      crs = crs) # add projection (this is WGS84)
  return(df_locs)
}
