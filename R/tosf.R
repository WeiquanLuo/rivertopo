#' @importFrom magrittr %>%
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom sf st_as_sf

tosf <- function(df,id_cn, Lon_cn, Lat_cn, crs= 4326){
  # cleanup data
  df_locs <- df %>% rename(lon=Lon_cn, lat=Lat_cn, id = id_cn)
  # convert to sf object
  df_locs <- st_as_sf(df_locs,
                          coords = c("lon", "lat"), # for point data
                          remove = F, # don't remove these lat/lon cols from df
                          crs = crs) # add projection (this is WGS84)
  df_locs <- df_locs %>% select(id, lon, lat, geometry)
  return(df_locs)
}
