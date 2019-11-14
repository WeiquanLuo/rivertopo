#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom raster crs
#' @export
#'
# test for module
# arc_sf.set <- arc_sf.set
# site_sf <- site_select_sf
# mouthID <- "rivermouth"

# find the downstream seg
simplifybyselectsite <- function(site_sf, arc_sf.set, mouthID){

  # site to segvert.set
  source("site2segvert.R")
  # segvert.set to vector.set
  source("segvert2vector.R")
  # vector.set to sf.set
  source("vector2sf.R")

  # piping the function above
  ## 1. convert site to segvert.set: seg, vert, seg1, vert1.
  segvert.set <- site_sf %>%
    site2segvert(arc_sf.set = arc_sf.set); segvert.set
  ## 2. convert segvert.set to vector.set: from, seg0, vert0, x0, y0, to, seg1, vert1, x1, y1.
  vector.set <- segvert2vector(segvert.set,
                               site_sf= site_select_sf) %>%
    filter(from != mouthID); vector.set
  ## 3. convert vector.set to sf.set: add sf geometry
  sf.set <- vector.set %>%
    vector2sf(crs = raster::crs(arc_sf.set))

  return(sf.set)
}
