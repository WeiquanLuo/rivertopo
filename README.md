
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rivertopo

<!-- badges: start -->

\[![Travis build
status](https://travis-ci.com/WeiquanLuo/rivertopo.svg?branch=master)
<!-- badges: end -->

The goal of rivertopo is to simplify a given tree topology (ie. for
River network) with given locations. For example:

  - convert object class `rivernetwork` from riverdist package to river
    topology in form of arcs between vertice.

<!-- end list -->

``` r
# resulted river topology
data.frame(from = c("Vertex1", "Vertex2"), to = c("Vertex2", "Vertex3"))
#>      from      to
#> 1 Vertex1 Vertex2
#> 2 Vertex2 Vertex3
```

  - convert a list of interesting location points into topological
    structure by mapping it on a given river topology

<!-- end list -->

``` r
# given location point:
data.frame(id = c("Point1", "Point2", "Point3"),
                   lat = c(42.032974, 44.032323, 47.123123),
                   long = c(-93.581543, -92.58345343, -96.2324543),
                   stringsAsFactors = FALSE)
#>       id      lat      long
#> 1 Point1 42.03297 -93.58154
#> 2 Point2 44.03232 -92.58345
#> 3 Point3 47.12312 -96.23245

# resulted location topology:
data.frame(from = c("Point1", "Point2"), to = c("Point2", "Point3"))
#>     from     to
#> 1 Point1 Point2
#> 2 Point2 Point3
```

## Installation

You can install the released version of rivertopo from
[CRAN](https://CRAN.R-project.org) with:

``` r
devtools::install_github("WeiquanLuo/rivertopo")
# install.packages("rivertopo") # not yet avaliable
```

## Workflow

<center>

![Workflow for rivertopo](inst/extdata/Workflow%20rivertopo.png)

</center>

## Example

<center>

![Topology (green color) of Selected USGS Water Sampling Site along
Topology (blue color) of River Topology within the Boundary of
Hydrological Unit 07 and 10](inst/extdata/riverNet.png)

</center>
