% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simplifybyselectsite2.R
\name{simplifybyselectsite2}
\alias{simplifybyselectsite2}
\title{Simplify the river topo (arc.set) with sites along the river}
\usage{
simplifybyselectsite2(site_select_sf, arc_sf.set)
}
\arguments{
\item{arc_sf.set}{a arc sf object from makearc() containing columns:
from, seg0, vert0, x0, y0, to, seg1, vert1, x1, y1, geometry}

\item{site_sf}{a site sf object with column: id, lon, lat, geometry, X, Y, and the snapped point information from the riverdist::xy2segvert() columns: seg, vert, snapdist}
}
\description{
Simplify the river topo (arc.set) with sites along the river
}
\examples{
example run
arc_select_sf.set <- simplifybyselectsite2(site_select_sf = site_select_sf, arc_sf.set = arc_sf.set)
g <- ggplot() +
 geom_sf(data = arc_select_sf.set, color = "blue") + # site topologyt
 geom_sf(data = rivers \%>\% st_transform(crs = 2163), alpha = 0.3) + # river network
 geom_point(data = st_drop_geometry(site_select_sf), color = "black", aes(text=sprintf("USGS ID: \%s", id), x = X, y = Y)) + # site location
 ggtitle("Site Topology Computed by the River Network Topology")
plotly::ggplotly(g)
}
