% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ceno6}
\alias{ceno6}
\title{Cenozoic 6 subset of the Paleobiology Database}
\format{
A \code{data.frame} with 61338 observations and 14 variables:
\describe{
	\item{\code{collection_no}}{Collection number. }
	\item{\code{early_interval}}{The older stratigraphic intercal. }
	\item{\code{late_interval}}{Younger stratigraphic interval. }
	\item{\code{group}}{Organismic group. }
	\item{\code{lat}}{Collection latitude. }
	\item{\code{lng}}{Collection longitude. }
	\item{\code{paleolat}}{Paleolatitude. }
	\item{\code{paleolng}}{Paleolongitude.}
	\item{\code{clgen}}{Concatenated class and genus name. }
	\item{\code{trinomen}}{Concatenated class, genus and species name. }
	\item{\code{icos}}{Face ID based on the positions of the paleocoordinates on an icosa::hexagrid(c(4,3)) grid. }
	\item{\code{icos2}}{Face ID based on the positions of the paleocoordinates on an icosa::hexagrid(c(7)) grid. }
	\item{\code{stg}}{Time interval identifier in the 95-bin stage-level timescale, implemented in the 'divDyn' R package.}
	\item{\code{stc}}{Spatiotemporal cell identifier: the icso2 and the stg values combined.}
}
}
\usage{
data(ceno6)
}
\description{
Fossil occurrences downloaded from the Paleobiology Database. This is a considerably old subset downloaded on 2018-12-05.
}
\keyword{datasets}
