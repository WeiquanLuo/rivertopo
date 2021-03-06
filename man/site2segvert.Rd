% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/site2segvert.R
\name{site2segvert}
\alias{site2segvert}
\title{Convert site to the form of segment and vertice present in the river topological structure}
\usage{
site2segvert(site_sf, arc_sf.set)
}
\arguments{
\item{site_sf}{a site sf object with column: id, lon, lat, geometry, X, Y, and the snapped point information from the riverdist::xy2segvert() columns: seg, vert, snapdist}

\item{arc_sf.set}{a arc sf object from makearc() containing columns:
from, seg0, vert0, x0, y0, to, seg1, vert1, x1, y1, geometry}
}
\description{
Convert site to the form of segment and vertice present in the river topological structure
}
