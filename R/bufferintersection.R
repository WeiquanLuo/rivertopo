#' geospatial operation of intersection between the buffered river centerline sf and site sf
#' @importFrom magrittr %>%
#' @importFrom sf st_transform st_buffer st_intersection
#' @export
#' @param site_sf a sf object of sites
#' @param rivers a sf object of river centerline
#' @param buffer_dist the distance of buffering riversn centerline in meter


bufferintersection <- function(site_sf, rivers, buffer_dist = 1000){
  rivers_buffer <- rivers %>%
    st_transform(crs = 2163) %>%
    st_buffer(dist = buffer_dist) %>%
    st_transform(crs = 4326)# buffer_dist is in meter
  # plot(st_geometry(rivers_buffer), lwd = 0.3)
  site_buffer_sf <- st_intersection(site_sf, rivers_buffer)
  return(site_buffer_sf)
}
