#' To thin a sf object
#' @importFrom magrittr %>%
#' @importFrom maptools thinnedSpatialPoly
#' @importFrom methods as
#' @importFrom sf st_as_sf
#' @export
#' @param shp a simple feature object of shapefile for river centerline



thinshp <- function(shp){
  shp_st <- maptools::thinnedSpatialPoly(
    as(shp, "Spatial"), tolerance = 0.1,
    minarea = 0.001, topologyPreserve = TRUE)
  shp <- st_as_sf(shp_st)
  return(shp)
}
