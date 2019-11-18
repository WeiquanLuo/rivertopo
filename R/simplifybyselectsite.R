#' Simplify the river topo (arc.set) with sites along the river
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom raster crs
#' @export
#' @param site_sf a site sf object with column: id, lon, lat, geometry, X, Y, and the snapped point information from the riverdist::xy2segvert() columns: seg, vert, snapdist
#' @param arc_sf.set a arc sf object from makearc() containing columns:
#' from, seg0, vert0, x0, y0, to, seg1, vert1, x1, y1, geometry
#' @param mouthID a string of the site id as the mouth point appear in site_sf

# test for module
# arc_sf.set <- arc_sf.set
# site_sf <- site_select_sf
# mouthID <- "rivermouth"

# find the downstream seg
simplifybyselectsite <- function(site_sf, arc_sf.set, mouthID){

  # piping the function above
  ## 1. convert site to segvert.set: seg, vert, seg1, vert1.
  segvert.set <- site2segvert(site_sf = site_sf,
                              arc_sf.set = arc_sf.set); segvert.set
  ## 2. convert segvert.set to vector.set: from, seg0, vert0, x0, y0, to, seg1, vert1, x1, y1.
  vector.set <- segvert2vector(segvert.set = segvert.set,
                               site_sf = site_sf) %>%
    filter(from != mouthID); vector.set
  ## 3. convert vector.set to sf.set: add sf geometry
  sf.set <- vector.set %>%
    vector2sf(crs = crs(arc_sf.set))

  return(sf.set)
}
