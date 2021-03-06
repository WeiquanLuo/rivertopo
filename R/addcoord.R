#' extract coordination from geometry column to be  X and Y the dataframe varible columns
#' @importFrom sf st_coordinates
#' @export
#' @param site_sf a sf object of sites

addcoord <- function(site_sf){
  # add COORDS in projected form (UTM)
  site_sf$X <- st_coordinates(site_sf)[,1]
  site_sf$Y <- st_coordinates(site_sf)[,2]
  return(site_sf)
}
