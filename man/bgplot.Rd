% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bgplot.R
\name{bgplot}
\alias{bgplot}
\title{Plotting of a biogeographic partitioning}
\usage{
bgplot(
  bdat,
  map = NULL,
  lng = NULL,
  lat = NULL,
  colors = NULL,
  labels = NULL,
  cell = "rownames",
  circles = TRUE,
  icosa = NULL,
  xlim = c(-180, 180),
  ylim = c(-90, 90),
  xlab = "longitude",
  ylab = "latitude",
  alpha = 0.5,
  between = NULL,
  fademap = 0.5,
  map.args = NULL,
  border = "gray50",
  grid.args = NULL,
  lab.args = NULL,
  axes = TRUE,
  asp = 1,
  add = FALSE
)
}
\arguments{
\item{bdat}{(\code{data.frame}) Biogeographic partitioning.}

\item{map}{(\code{SpatialPolygons, SpatialPolygonsDataFrame, RasterLayer}) A map object, plottable with \code{\link[chronosphere]{mapplot}}.}

\item{colors}{(\code{character}) Variable name of the colors.}

\item{labels}{(\code{character}) Variable name of the labels.}

\item{cell}{(\code{character}) Where is the cell information? "rownames" or column name.}

\item{circles}{(\code{logical}) Should the Equator and the 30 degree latitudinal circles be plotted?}

\item{icosa}{(\code{hexagrid}) or \code{trigrid} Optional argument, a grid from the icosa package (if cells of such grid are used). Otherise the biogeographic regions will be indicated with circles (not yet?).}

\item{xlim}{(\code{numeric}) the standard argument of plot() - not tested yet!}

\item{ylim}{(\code{numeric}) the standard argument of plot() - not tested yet!}

\item{xlab}{(\code{character}) the standard argument of plot() - not tested yet!}

\item{ylab}{(\code{character}) the standard argument of plot() - not tested yet!}

\item{alpha}{(\code{numeric}) Numeric value between 0 and 1 indicating the opacity of the partitioning.}

\item{between}{(\code{expression}) code to be executed between the map drawing and the plotting of the partitioning.}

\item{fademap}{(\code{numeric}) Numeric value between 0 and 1 indicating the opacity of the map.}

\item{map.args}{(\code{list}) Additional arguments passed to the \code{\link{mapplot}} function to render the maps.}

\item{grid.args}{(\code{logical}) Additional arguments to plot the grid itself.}

\item{lab.args}{(\code{logical}) Additional arguments to plot the labels with the text function.}

\item{axes}{(\code{logical}) Should the default axes be plotted?}

\item{asp}{(\code{numeric}) Same as in par(). Defaults to 1 (fixed aspect ratio).}

\item{add}{(\code{logical}) Should the plot be added to another plot?}
}
\description{
The function takes a single time slice from a grouping output and plots a geographic rendering.
}
\details{
Option B. coorindates go to bdat, triggering
}
\examples{
# Partition the Cenozoic 6 benthic subset of the Paleobiology Database
oneC6 <- bgpart(ceno6,bin=NULL, tax="trinomen", cell="icos", ocq=10, base="network", method="infomap")
# The used grid for plotting
library(icosa)
hex <- hexagrid(c(4,3), sp=TRUE)
# land polygons and colors for a nice plot
data(land)
bgplot(oneC6, map=land, colors="col", icosa=hex, labels="grouping")

}
