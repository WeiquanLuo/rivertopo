#' @importFrom magrittr %>%
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr rename

# segvert.set to vector.set
segvert2vector <- function(segvert.set, site_sf){
  vector.set <- site_sf %>%
    st_drop_geometry() %>%
    select(id, seg, vert, X, Y) %>%
    left_join(segvert.set, by = c("seg", "vert")) %>%
    rename(seg0=seg, vert0 =vert, from = id, x0 = X, y0 = Y)

  complete_vector.set <- vector.set %>% left_join(
    vector.set %>% select(-seg1, -vert1) %>%
      rename(to=from, seg1=seg0, vert1=vert0, x1=x0, y1=y0),
    by = c("seg1", "vert1")) %>%
    select(from, seg0, vert0, x0, y0, to, seg1, vert1, x1, y1)
  return(complete_vector.set)
}
