#' @importFrom sf st_coordinates
#' @export

addcoord <- function(site_sf){
  # add COORDS in projected form (UTM)
  site_sf$X <- sf::st_coordinates(site_sf)[,1]
  site_sf$Y <- sf::st_coordinates(site_sf)[,2]
  return(site_sf)
}
