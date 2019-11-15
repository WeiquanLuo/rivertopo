#' convert rivers_net to site_sf
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select
#' @importFrom purrr pmap map map_dfr
#' @importFrom sf st_sf st_linestring
#' @importFrom utils head tail
#' @export
#' @param rivers_net a rivernetwork class object from riverdist package.
#' @param crs a coordinate reference system for Map projections attempt to portray
#' the surface of the earth or a portion of the earth on a flat piece of paper or
#' computer screen.

#riverVert <- rivernetwork2riverVert(rivers_net= rivers_net_fixed, crs = 2163)

# convert rivers_net to site_sf
rivernetwork2riverVert <- function(rivers_net, crs = 2163){

  makerivVert_id <- function(rivID){
    rivVert_id <- data.frame(id = c(paste0(rivID, "a"), paste0(rivID, "b")),
                             seg = rivID,
                             stringsAsFactors = FALSE)
    return(rivVert_id)
  }

  makerivVert <- function(lines){
    head_coord <- lines %>% head(1)
    tail_coord <- lines %>% tail(1)

    rivVert <- data.frame(vert = c(1, lines %>% nrow()),
                          X = c(head_coord[,1],tail_coord[,1]),
                          Y = c(head_coord[,2],tail_coord[,2]))
    rownames(rivVert) <- c()
    return(rivVert)
  }
  # build ID
  rivVert_id <- rivers_net$lineID$rivID %>% map_dfr(makerivVert_id)
  # build vertex and long/lat
  rivVert <- rivers_net$lines %>% map_dfr(makerivVert)

  riverVert <- cbind(rivVert_id, rivVert)

  riverVert <- rbind(riverVert %>% filter(seg==rivers_net$mouth$mouth.seg,
                                          vert==rivers_net$mouth$mouth.vert)) %>%
    rbind(riverVert %>% filter(seg==rivers_net$mouth$mouth.seg,
                               vert!=rivers_net$mouth$mouth.vert)) %>%
    rbind(riverVert %>% filter(seg!=rivers_net$mouth$mouth.seg))

  vector2matrix <- function(X,Y){
    matrix(c(X, Y), ncol=2, byrow=TRUE)
  }

  riverVert_geometry <- riverVert %>%
    select(X,Y) %>%
    as.list %>%
    pmap(vector2matrix) %>%
    map(st_linestring)

  riverVert <- st_sf(riverVert, geometry=riverVert_geometry, crs=crs)

  return(riverVert)
}
