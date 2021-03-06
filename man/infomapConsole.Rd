% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/networkBased.R
\name{infomapConsole}
\alias{infomapConsole}
\title{Function to run the infomap console application from within R.}
\usage{
infomapConsole(graph, bipartite, cargs = NULL, feedback = FALSE)
}
\arguments{
\item{graph}{(\code{igraph}) A graph.}

\item{bipartite}{(\code{logical}) Is the graph bipartite? The readin function will use this piece of information.}

\item{cargs}{Argumentation run with the console}

\item{feedback}{(\code{logical}) Should the functions output information about progress?}
}
\description{
This function reaches out from R, runs infomap, and loads its output to R. The infomap executable should be added to the system path environment variable (could be run from any directory).
}
