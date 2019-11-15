#' connect to the next downstream site within segment
#' @importFrom magrittr %>%
#' @importFrom tidyr nest
#' @importFrom dplyr select filter mutate distinct left_join
#' @importFrom rlang flatten
#' @importFrom stats setNames
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @export
#' @param site_sf a site sf object with column: id, lon, lat, geometry, X, Y, and the snapped point information from the riverdist::xy2segvert() columns: seg, vert, snapdist
#' @param segvert.set Null in the first run to identify any withinsegment downstream point. when not NULL, a segvert.set is a dataframe calculate by linkbetweenseg() contains column: seg, vert, seg1, vert1, onnected.
#' Variable seg and vert is the snaped point from site to tth river, whereas column seg1, vert1 are the downstream snaped point
#' @param site NULL in the non-first run. In the first run, site is the datafram contain two columns: seg, vert resulted from reorderbyseg()

# link within seg: site to site $ vert to site
linkwithinseg <- function(site, site_sf, segvert.set= NULL){

  if (is.null(segvert.set)){ # initialize by link site are on the same seg
    # test: data <- site %>% nest(vert); data <- data$data[[2]]; data
    linkinseg_site2site <- function(data){
      # since site is arrange from upstream to downstream
      segvert.set <- data %>% left_join(data.frame(vert=data$vert[-length(data$vert)],
                                                   vert1= data$vert[-1]), by="vert"); segvert.set
      return(segvert.set)
    }
    segvert.set <- site %>%
      nest(vert) %>%
      mutate(data = map(data, linkinseg_site2site)) %>% # connect site to downstream site
      unnest() %>%
      mutate(seg1 =seg) %>%
      select(seg, vert, seg1, vert1) %>%
      mutate(connected = !is.na(vert1)); segvert.set
    # segvert.set %>% group_by(connected) %>% summarize(n=n())
  } else {
    # seperate link  within seg and between seg
    linksite <- segvert.set %>% filter(connected==TRUE); #linksite$seg %>% unique %>% sort %>% length
    unlinksite <- segvert.set %>% filter(connected==FALSE); #unlinksite$seg %>% unique %>% sort %>% length

    # test: data <- checkVert %>% select(seg1, vert1) %>% left_join(site, by=c("seg1" ="seg")) %>% nest(vert); data[2,]; data <- data$data[[2]]; data
    linkinseg_vert2site <- function(data){
      data <- data %>% flatten() %>% unlist
      data <- data[1]
    }

    unlinksite_update <- unlinksite %>%
      rename(seg0 = seg1, vert0 = vert1) %>%
      left_join(reorderbyseg(site_sf = site_sf,
                             arc_sf.set = arc_sf.set) %>%
                  setNames(c("seg1","vert1")) %>%
                  nest(vert1) %>%
                  mutate(data = map(data, linkinseg_vert2site)) %>% # connect vert to downstream site by selecting the first site
                  unnest() %>%
                  mutate(vert1 = data) %>%
                  select(-data),
                by=c("seg0" ="seg1")) %>% # make line if seg is same
      mutate(seg1 = seg0) %>%
      select(seg, vert, seg1, vert1) %>%
      mutate(connected = !is.na(vert1)); unlinksite_update

    segvert.set <- rbind(linksite, unlinksite_update); segvert.set
    # segvert.set %>% group_by(connected) %>% summarize(n=n())
  }
  return(segvert.set)
}
