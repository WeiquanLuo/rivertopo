#' @importFrom magrittr %>%
#' @importFrom raster crs
# importFrom rivertopo arc2vector vector2sf
#' @export

# TEST: riverVert <- riverVert
# convert arc.set to sf
arc2sf <- function(arc.set, riverVert){

  # arc.set to vector.set
  vector.set <- arc.set %>%
    arc2vector(riverVert = riverVert)

  # vector.set to sf.set
  sf.set <- vector.set %>%
    vector2sf(crs = crs(riverVert))

  return(sf.set)
}
