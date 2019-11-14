#' @importFrom magrittr %>%
#' @importFrom raster crs
# importFrom rivertopo arc2vector vector2sf
#' @export


# TEST: riverVert <- riverVert
# convert arc.set to sf
arc2sf <- function(arc.set, riverVert){

  # arc.set to vector.set
  #source("arc2vector.R")
  vector.set <- arc.set %>%
    arc2vector(riverVert = riverVert)

  # vector.set to sf.set
  #source("vector2sf.R")

  sf.set <- vector.set %>%
    vector2sf(crs = crs(riverVert))

  return(sf.set)
}
