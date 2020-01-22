#' Simplify the river topo (arc.set) with sites along the river
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select arrange desc
#' @importFrom sf st_drop_geometry
#' @importFrom raster crs
#' @importFrom stats setNames
#' @importFrom xts last
#' @export
#' @param site_sf a site sf object with column: id, lon, lat, geometry, X, Y, and the snapped point information from the riverdist::xy2segvert() columns: seg, vert, snapdist
#' @param arc_sf.set a arc sf object from makearc() containing columns:
#' from, seg0, vert0, x0, y0, to, seg1, vert1, x1, y1, geometry


#' @examples
#' example run
#' arc_select_sf.set <- simplifybyselectsite2(site_select_sf = site_select_sf, arc_sf.set = arc_sf.set)
#' g <- ggplot() +
#'  geom_sf(data = arc_select_sf.set, color = "blue") + # site topologyt
#'  geom_sf(data = rivers %>% st_transform(crs = 2163), alpha = 0.3) + # river network
#'  geom_point(data = st_drop_geometry(site_select_sf), color = "black", aes(text=sprintf("USGS ID: %s", id), x = X, y = Y)) + # site location
#'  ggtitle("Site Topology Computed by the River Network Topology")
#'plotly::ggplotly(g)


# test for module
# arc_sf.set <- arc_sf.set
# site_sf <- site_select_sf
simplifybyselectsite2 <- function(site_select_sf, arc_sf.set){
  find_link_4merge <- function(ID, site_select_sf, arc_sf.set) {

    stream_site <- site_select_sf %>% filter(id == ID)

    # determine the stream flow direction
    stream_arc <- arc_sf.set %>%
      filter(seg0 == stream_site$seg & seg1 == unique(stream_site$seg))
    merage_arc <- arc_sf.set %>% filter(seg1 == unique(stream_site$seg))
    upstream_arc <- merage_arc %>% filter(from != stream_arc$from)

    # get stream_site on same seg
    stream_site <- site_select_sf %>% filter(seg == unique(stream_site$seg))
    # order the site from downstream(to) to upstream(from)
    reorder_stie <- function(stream_site, stream_arc){
      if (stream_arc$vert0 < stream_arc$vert1){
        stream_site <- stream_site %>% arrange(desc(vert))
      } else if (stream_arc$vert0 > stream_arc$vert1){
        stream_site <- stream_site %>% arrange(vert)
      }
      return(stream_site)
    }
    stream_site <- reorder_stie(stream_site =stream_site, stream_arc = stream_arc)

    arc.set <-{}
    # order site within a segment, and make arc
    if (nrow(stream_site) > 1){
      for (n_site_on_seg in 1:(nrow(stream_site) -1) ){
        # input the stream_site table for all site on the same seg, output the link
        to <- stream_site[n_site_on_seg,] %>%
          st_drop_geometry() %>%
          select(id, seg, vert, X, Y, lon, lat) %>%
          setNames(c("to", "seg1", "vert1", "x1", "y1", "lon1", "lat1" ))
        from <- stream_site[(n_site_on_seg +1) ,] %>%
          st_drop_geometry() %>%
          select(id, seg, vert, X, Y, lon, lat) %>%
          setNames(c("from", "seg0", "vert0", "x0", "y0", "lon0", "lat0" ))
        Aarc <- cbind(from, to)
        arc.set <- arc.set %>% rbind(Aarc)
      }
    }

    # inititate snapping between seg, to get upstream_site on upstream seg
    stream_arc <- {}
    upstream_site <-{}
    upstream_arc.update <- {}
    # capture using upstream_site, dump using upstream_seg
    repeat{
      # check upstream_arc has site
      upstream_site <- upstream_site %>% rbind(site_select_sf %>% filter(seg %in% upstream_arc$seg0))
      # remove the recorded upstream_arc
      if (nrow(upstream_site)!=0) upstream_arc <- upstream_arc %>% filter(!(seg0 %in% unique(upstream_site$seg)))
      # find the next arc if not site, move the the next arc
      if (nrow(upstream_arc) != 0) {
        current_seg <- upstream_arc$seg1 %>% unique()
        upstream_seg <- upstream_arc$seg0
        upstream_arc <- arc_sf.set %>% filter(seg1 %in% upstream_seg & seg1 != seg0)
      }
      if (identical(upstream_arc, upstream_arc.update)){
        # if nrow(upstream_arc) != 0, mean reaching the end of topology but not site on it
        # if nrow(upstream_arc) == 0), mean reaching the segment with site.
        break
      } else {
        upstream_arc.update <- upstream_arc
      }
    }

    # get the to in the upstream_site
    if (nrow(upstream_site) >=1){
      # retain the first upstream_site
      upstream_site_select <- {}
      for (seg_candidate in 1:length(unique(upstream_site$seg))){
        upstream_site_to <- reorder_stie(stream_site = upstream_site %>%
                                           filter(seg == unique(upstream_site$seg)[seg_candidate]),
                                         stream_arc = arc_sf.set %>%
                                           filter(seg0 == unique(upstream_site$seg)[seg_candidate] & seg0 == seg1))
        upstream_site_select <- upstream_site_select %>% rbind(upstream_site_to[1,])
      }
      upstream_site <- upstream_site_select
    }
    # making link for between seg
    if (nrow(upstream_site)>0){
      for (n_site_between_seg in 1:nrow(upstream_site)){
        # input the stream_site table for all site on the same seg, output the link
        to <- stream_site %>%
          st_drop_geometry() %>%
          last() %>%
          select(id, seg, vert, X, Y, lon, lat) %>%
          setNames(c("to", "seg1", "vert1", "x1", "y1", "lon1", "lat1" ))
        from <- upstream_site[n_site_between_seg,] %>%
          st_drop_geometry() %>%
          select(id, seg, vert, X, Y, lon, lat) %>%
          setNames(c("from", "seg0", "vert0", "x0", "y0", "lon0", "lat0" ))
        Aarc <- cbind(from, to)
        arc.set <- arc.set %>% rbind(Aarc)
      }
    }

    return(arc.set)
  }

  arc_select_sf.set <- purrr::map_dfr(site_select_sf$id,
                                      function(x) find_link_4merge(ID = x,
                                                                   site_select_sf = site_select_sf,
                                                                   arc_sf.set = arc_sf.set)) %>%
    distinct() %>%
    vector2sf(crs = crs(site_select_sf))
  return(arc_select_sf.set)
}



