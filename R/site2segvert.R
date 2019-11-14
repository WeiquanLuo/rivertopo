#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom rivertopo reorderbyseg linkwithinseg linkbetweenseg
#' @export
# site to segvert.set
site2segvert <- function(site_sf, arc_sf.set){

  # piping the above functions
  # 1. reorder site grouped by seg in order of
  # 2. connect to the next downstream site
  # 3. connect to first site in next downstream seg
  site <- site_sf %>%
    reorderbyseg(arc_sf.set = arc_sf.set)
  segvert.set <- site %>%
    linkwithinseg() %>%   # return segvert.set with site to site
    linkbetweenseg(arc_sf.set = arc_sf.set) # return segvert.set with avaliable site to startVert
  # iterate until no NA
  total <- nrow(segvert.set)-1
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  while (segvert.set %>% filter(connected==FALSE) %>% nrow() > 1){
    segvert.set <- segvert.set %>% linkwithinseg(site = site) %>% # return segvert.set with startVert to avaliable site
      linkbetweenseg(arc_sf.set = arc_sf.set) # return segvert.set with avaliable site to startVert
    setTxtProgressBar(pb, nrow(segvert.set)- segvert.set %>% filter(connected==FALSE) %>% nrow())
  }
  close(pb)

  #segvert.set %>% group_by(connected) %>% summarize(n=n())
  segvert.set <- segvert.set %>% filter(connected ==TRUE)
  return(segvert.set)
}
