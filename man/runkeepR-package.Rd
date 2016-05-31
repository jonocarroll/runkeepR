\name{runkeepR-package}
\alias{runkeepR-package}
\alias{runkeepR}
\docType{package}
\title{
\packageTitle{runkeepR}
}
\description{
\packageDescription{runkeepR}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{runkeepR}
\packageIndices{runkeepR}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
\packageAuthor{runkeepR}

Maintainer: \packageMaintainer{runkeepR}
}
\references{
~~ Literature or other references for background information ~~
}

\examples{
\dontrun{
system.file("data", package="runkeepR")
routes <- load_tracks(system.file("data", package="runkeepR"))
plot_leaflet(routes, trackPal=topo.colors(10, alpha=0.5))
}

}

\keyword{ package }

