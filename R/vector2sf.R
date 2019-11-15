#' convert vector.set to sf.set
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom purrr pmap map
#' @importFrom sf st_sf st_linestring
#' @export
#' @param vector.set a dataframe have

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
