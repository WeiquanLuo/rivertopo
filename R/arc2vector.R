#' Convert arc.set to vector.set
#' @importFrom magrittr %>%
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr select left_join
#' @importFrom rlang set_names
#' @export
#' @param arc.set the arcPair object from makearc contain two columns: from, to
#' @param riverVert a sf class object contain columns: id seg vert, X, Y, geometry

arc2vector <- function(arc.set, riverVert){
  lookuptable_locs2lonlat <- st_drop_geometry(riverVert)
  vector.set <- arc.set %>%
    select(from) %>%
    left_join(lookuptable_locs2lonlat, by= c("from" = "id" )) %>%
    set_names(c("from", "seg0", "vert0", "x0", "y0")) %>%
    cbind(arc.set %>%
            select(to) %>%
            left_join(lookuptable_locs2lonlat, by= c("to" = "id")) %>%
            set_names(c("to", "seg1", "vert1", "x1", "y1"))); # vector.set %>% head()
  return(vector.set)
}
