#' connect to the next downstream site across segment
#' @importFrom magrittr %>%
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr filter select mutate as_tibble rename left_join
#' @export
#' @param arc_sf.set a arc sf object from makearc() containing columns:
#' from, seg0, vert0, x0, y0, to, seg1, vert1, x1, y1, geometry
#' @param segvert.set a segvert.set compute by site2segvert() containing column: seg, vert, seg1, vert1, onnected.
#' Variable seg and vert is the snaped point from site to tth river, whereas column seg1, vert1 are the downstream snaped point


# link between seg
linkbetweenseg <- function(segvert.set, arc_sf.set){

  # select link between segment only
  arcbetweenseg <- arc_sf.set %>%
    st_drop_geometry() %>%
    filter(seg0 != seg1) %>%  # different seg
    select(seg0, vert0, seg1, vert1) %>%
    as_tibble(); arcbetweenseg

  # seperate link  within seg and between seg
  linksite <- segvert.set %>% filter(connected==TRUE); #linksite$seg %>% unique %>% sort %>% length
  unlinksite <- segvert.set %>% filter(connected==FALSE); # unlinksite$seg %>% unique %>% sort %>% length

  # find the startVert on next seg segment
  findStartVert <- function(unlinksite, arcbetweenseg){

    # connect to the startvert of next downstream seg
    unlinksite_update <- unlinksite %>%
      rename(seg0 = seg1, vert0 = vert1) %>%
      select(-vert0) %>%
      left_join(arcbetweenseg, by = c("seg0")) %>%
      select(seg, vert, seg1, vert1) %>%
      mutate(connected = FALSE); unlinksite_update

    return(unlinksite_update)
  }

  # replace the startvert of next downstream seg to be first site in the seg
  unlinksite_update <- unlinksite %>%
    findStartVert(arcbetweenseg = arcbetweenseg); unlinksite_update
  segvert.set <- rbind(linksite, unlinksite_update); segvert.set
  # segvert.set %>% group_by(connected) %>% summarize(n=n())
  return(segvert.set)
}
