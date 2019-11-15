#' Convert site to the form of segment and vertice present in the river topological structure
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @export
#' @param site_sf a site sf object with column: id, lon, lat, geometry, X, Y, and the snapped point information from the riverdist::xy2segvert() columns: seg, vert, snapdist
#' @param arc_sf.set a arc sf object from makearc() containing columns:
#' from, seg0, vert0, x0, y0, to, seg1, vert1, x1, y1, geometry

# site to segvert.set
site2segvert <- function(site_sf, arc_sf.set){

  # piping the above functions
  # 1. reorder site with grouping by seg in closet order from endvert of each seg
  site <- reorderbyseg(site_sf = site_sf, arc_sf.set = arc_sf.set)
  # 2. connect to the next downstream site
  segvert.set <- site %>%
    linkwithinseg() %>%   # return segvert.set with site to site
    linkbetweenseg(arc_sf.set = arc_sf.set) # return segvert.set with avaliable site to startVert
  # 3. connect to first site in next downstream seg, iterate until no NA
  total <- nrow(segvert.set)-1
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  while (segvert.set %>% filter(connected==FALSE) %>% nrow() > 1){
    segvert.set <- segvert.set %>%
      linkwithinseg(site = site, site_sf = site_sf) %>% # return segvert.set with startVert to avaliable site
      linkbetweenseg(arc_sf.set = arc_sf.set) # return segvert.set with avaliable site to startVert
    setTxtProgressBar(pb,
                      nrow(segvert.set) - segvert.set %>%
                        filter(connected==FALSE) %>%
                        nrow())
  }
  close(pb)

  #segvert.set %>% group_by(connected) %>% summarize(n=n())
  segvert.set <- segvert.set %>% filter(connected ==TRUE)
  return(segvert.set)
}
