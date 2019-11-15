#' convert vector.set to sf.set
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom purrr pmap map
#' @importFrom sf st_sf st_linestring
#' @export
#' @param vector.set a dataframe from segvert2vector() or dataframe must contain columns: from, seg0, vert0, x0, y0 to, seg1, vert1, x1, y1
#' @param crs a coordinate reference system for Map projections attempt to portray
#' the surface of the earth or a portion of the earth on a flat piece of paper or
#' computer screen.

# vector.set to sf.set
vector2sf <- function(vector.set, crs){
  # create matrix for st_linestring()
  vector2matrix <- function(x0, y0, x1, y1){
    matrix(c(x0, y0, x1, y1), ncol=2, byrow=TRUE)
  }
  geometry.set <- vector.set %>%
    select(x0, y0, x1, y1) %>%
    as.list %>%
    pmap(vector2matrix) %>%
    map(st_linestring)
  sf.set <- st_sf(vector.set, geometry=geometry.set, crs=crs)
  return(sf.set)
}
