#' @importFrom magrittr %>%


# site to segvert.set
site2segvert <- function(site_sf, arc_sf.set){

  # reorder group by seg in closet order
  reorderbyseg <- function(site_sf, arc_sf.set){
    # find the *endvert of each seg* where sites locate
    endvert <- arc_sf.set %>%
      st_drop_geometry() %>%
      as_tibble() %>%
      filter(seg0 != seg1) %>% # filter the arc between seg
      select(seg0, vert0) %>% # select endvert of seg or the from (downstream of the seg)
      arrange(seg0) %>%
      distinct() %>%
      filter(seg0 %in% site_sf$seg); endvert # filter only seg where sites locate

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
      mutate(data = purrr::map(data, reordervert)) %>% # group by seg, arrange from farthest to closest to the endvert of the seg
      unnest() %>%
      select(seg, vert) %>%
      distinct()
    # note: site is arrange from the upstream to downstream with in group of seg
    return(site)
  }

  # link within seg: site to site $ vert to site
  linkwithinseg <- function(site, segvert.set= NULL){

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
        mutate(data = purrr::map(data, linkinseg_site2site)) %>% # connect site to downstream site
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
                    mutate(data = purrr::map(data, linkinseg_vert2site)) %>% # connect vert to downstream site by selecting the first site
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
