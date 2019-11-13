#' @importFrom magrittr %>%
#' @importFrom plyr laply
#' @importFrom dplyr tibble
#' @importFrom riverdist riverdistance
#' @importFrom plyr mdply
#' @importFrom dplyr pull
#' @importFrom dplyr arrange
#' @importFrom purrr map2
#' @importFrom riverdist detectroute
#' @importFrom plyr mdply
#' @importFrom dplyr left_join
#' @importFrom stats na.omit
#' @importFrom dplyr select
#' @importFrom dplyr mutate_all
#' @importFrom dplyr filter


# test for makearc
# riverVert <- riverVert
# rivers_net <- rivers_net_fixed
# prefix=""

makearc <- function(riverVert, rivers_net, prefix=""){

  # lookuptable module
  createlookuptable_seg2id <-function(riverVert, prefix){

    # create raw lookuptable
    lookuptable <- data.frame(id = laply(riverVert$id, function(x) paste0(prefix,x)),
                              seg = riverVert$seg,
                              vert = riverVert$vert)

    # calculate the route distance
    lookuptable$routedist <- tibble(startseg = lookuptable$seg[1],
                                    startvert = lookuptable$vert[1],
                                    endseg = lookuptable$seg,
                                    endvert = lookuptable$vert) %>%
      mdply(riverdistance, rivers=rivers_net) %>%
      pull(V1)

    # reorder
    lookuptable <- lookuptable %>% arrange(seg, routedist)
    return(lookuptable)
  }
  lookuptable <-createlookuptable_seg2id(riverVert = riverVert, prefix=prefix)

  # segment list vector from river mouth to location
  pairwise_seg <- expand.grid(lookuptable$seg[lookuptable$seg==rivers_net$mouth$mouth.seg & lookuptable$vert==rivers_net$mouth$mouth.vert],
                              lookuptable$seg[!(lookuptable$seg==rivers_net$mouth$mouth.seg & lookuptable$vert==rivers_net$mouth$mouth.vert)])

  seglist <- map2(pairwise_seg$Var1,
                  pairwise_seg$Var2,
                  function(x, y) detectroute(start=x, end=y, rivers=rivers_net)) %>%
    unique()

  # node_id vector from river mouth to location
  # TEST: x <-seglist[[1]]
  idlist <- plyr::llply(seglist, function(x) data.frame(seg=x) %>%
                          left_join(lookuptable, by = "seg") %>%
                          na.omit() %>%
                          select(id) %>%
                          laply(function(x) as.character(x)))

  # x<- idlist[[1]]
  makelink <- function(x){
    x <- rev(x)
    if (length(x)!=1){
      linklist <- data.frame(from = x[-length(x)], to = x[-1] )
      return(linklist)
    }
  }
  arc.set <- plyr::ldply(idlist, makelink) %>%
    distinct() %>%
    mutate_all(as.character) %>%
    filter(from!=to)
  return(arc.set)
}
