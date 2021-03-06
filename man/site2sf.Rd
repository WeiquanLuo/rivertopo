% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/site2sf.R
\name{site2sf}
\alias{site2sf}
\title{convert site dataframe to simple feature}
\usage{
site2sf(df, id_cn, Lon_cn, Lat_cn, crs = 4326)
}
\arguments{
\item{df}{a dataframe contain id, long, lat for sites}

\item{id_cn}{a string for the name of id column in df}

\item{Lon_cn}{a string for the name of the longitude column in df}

\item{Lat_cn}{a string for the name of the latitude column in df}

\item{crs}{a coordinate reference system for Map projections attempt to portray
the surface of the earth or a portion of the earth on a flat piece of paper or
computer screen.}
}
\description{
convert site dataframe to simple feature
}
