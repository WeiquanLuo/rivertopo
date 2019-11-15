#' convert arc.set to sf
#' @importFrom magrittr %>%
#' @importFrom raster crs
#' @export
#' @param arc.set the arcPair object from makearc contain two columns: from, to
#' @param riverVert a sf class object contain columns: id seg vert, X, Y, geometry

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
