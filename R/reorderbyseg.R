#' reorder site with grouping by seg in closet order from endvert of each seg
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select arrange distinct mutate desc
#' @importFrom purrr map
#' @export
#' @param site_sf a site sf object with column: id, lon, lat, geometry, X, Y, and the snapped point information from the riverdist::xy2segvert() columns: seg, vert, snapdist
#' @param arc_sf.set a arc sf object from makearc() containing columns:
#' from, seg0, vert0, x0, y0, to, seg1, vert1, x1, y1, geometry

# reorder site with grouping by seg in closet order from endvert of each seg
reorderbyseg <- function(site_sf, arc_sf.set){

  # find the *endvert of each seg* where sites locate
  endvert <- arc_sf.set %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    filter(seg0 != seg1) %>% # filter the arc between seg
    select(seg0, vert0) %>% # select endvert of seg or the from (downstream of the seg)
    arrange(seg0) %>%
    distinct() %>%
    filter(seg0 %in% site_sf$seg)# filter only seg where sites locate

  # testing
  # df <- site %>% left_join(endvert, by= c("seg"= "seg0")) %>% nest(-seg) %>% select(data); df<-df[2, ][[1]][[1]]; df
  reordervert <- function(df){
    if (nrow(df) ==1){
      return(df)
    } else {
      df <- df %>% mutate(vertdiff = abs(vert0 - vert)) %>% # the closer to the endvert of the seg, the smaller vertdiff
        arrange(desc(vertdiff)) %>% # arrange from farthest to closest to the endvert of the seg
        select(-vertdiff)
      return(df)
    }
  }

  site <- site_sf %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    left_join(endvert, by= c("seg"= "seg0")) %>%
    nest(-seg) %>%
    mutate(data = map(data, reordervert)) %>% # group by seg, arrange from farthest to closest to the endvert of the seg
    unnest() %>%
    select(seg, vert) %>%
    distinct()
  # note: site is arrange from the upstream to downstream with in group of seg
  return(site)
}
