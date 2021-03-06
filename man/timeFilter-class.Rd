\name{timeFilter-class}
\docType{class}
\alias{timeFilter-class}
\alias{timeFilter} 
\alias{timeFilter-class}
\alias{show,timeFilter-method}

\title{Class "timeFilter" }

\description{

  Define a \code{\link{filter}} that removes stretches of unusual data
  distribution within a single parameter over time. This can be used to
  correct for problems during data acquisition like air bubbles or
  clods.

}


\section{Extends}{
  
  Class \code{"\linkS4class{parameterFilter}"}, directly.

  Class \code{"\linkS4class{concreteFilter}"}, by class
  \code{parameterFilter}, distance 2.

  Class \code{"\linkS4class{filter}"}, by class \code{parameterFilter},
  distance 3.

}


\section{Slots}{ 
  \describe{

    \item{\code{bandwidth}:}{Object of class \code{"numeric"}. The
      sensitivity of the filter, i.e., the amount of local variance of
      the signal we want to allow. }
    
    \item{\code{binSize}:}{Object of class \code{"numeric"}. The size
      of the bins used for the local variance and location
      estimation. If \code{NULL}, a reasonable default is used when
      evaluating the filter. }

    \item{\code{timeParameter}:}{Object of class \code{"character"},
      used to define the time domain parameter. If \code{NULL}, the
      filter tries to guess the time domain from the  \code{flowFrame}.}
    
    \item{\code{parameters}:}{Object of class \code{"character"},
      describing the parameters used to filter the \code{flowFrame}. }
    
    \item{\code{filterId}:}{Object of class \code{"character"},
      referencing the filter.}
    
  }
}


\section{Objects from the Class}{

  Objects can be created by calls of the form \code{new("timeFilter",
  ...)} or using the constructor \code{timeFilter}. Using the
  constructor is the recommended way of object instantiation:

}


\usage{

timeFilter(..., bandwidth=0.75, binSize, timeParameter,
filterId="defaultTimeFilter")


}

\arguments{
  
  \item{\dots}{ The names of the parameters on which the filter is
    supposed to work on. Names can either be given as individual
    arguments, or as a list or a character vector. }
    
  \item{filterId}{ An optional parameter that sets the \code{filterId}
    slot of this gate. The object can later be identified by this name.}
  
  \item{bandwidth, binSize}{ Numerics used to set the \code{bandwidth}
    and \code{binSize} slots of the object. }
  
  \item{timeParameter}{ Character used to set the \code{timeParameter}
    slot of the object.}
  
}


\value{

  Returns a \link{timeFilter} object for use in filtering
  \code{\link{flowFrame}}s or other flow cytometry objects.
  
}


\section{Methods}{
  \describe{
    
    \item{\%in\%}{\code{signature(x = "flowFrame", table =
	"timeFilter")}: The workhorse used to evaluate the filter on
      data. This is usually not called directly by the user. }
      
    \item{show}{\code{signature(object = "timeFilter")}: Print
      information about the filter. }
    
  }
}


\details{

  Clods and disturbances in the laminar flow of a FACS instrument can
  cause temporal aberrations in the data acquisition that lead to
  artifactual values. \code{timeFilters} try to identify such stretches
  of disturbance by computing local variance and location estimates and
  to remove them from the data.

}


\note{

  See the documentation of
  \code{\link[flowViz:timeLinePlot]{timeLinePlot}} in the 
  \code{\link[flowViz:flowViz-package]{flowViz}} package for details on
  visualizing temporal problems in flow cytometry data.

}


\author{ Florian Hahne }


\seealso{
  
  \code{\link{flowFrame}}, \code{\link[flowCore:filter-class]{filter}} for
  evaluation of \code{timeFilters} and \code{\link{split}} and
  \code{\link{Subset}}for splitting and subsetting of flow cytometry
  data sets based on that.  }

\examples{

## Loading example data
data(GvHD)
dat <- GvHD[1:10]

## create the filter
tf <- timeFilter("SSC-H", bandwidth=1, filterId="myTimeFilter")
tf

## Visualize problems
\dontrun{
library(flowViz)
timeLinePlot(dat, "SSC-H")
}

## Filtering using timeFilters
fres <- filter(dat, tf)
fres[[1]]
summary(fres[[1]])
summary(fres[[7]])

## The result of rectangle filtering is a logical subset
cleanDat <- Subset(dat, fres)

## Visualizing after cleaning up
\dontrun{
timeLinePlot(cleanDat, "SSC-H")
}

## We can also split, in which case we get those events in and those
## not in the gate as separate populations
allDat <- split(dat[[7]], fres[[7]])

par(mfcol=c(1,3))
plot(exprs(dat[[7]])[, "SSC-H"], pch=".")
plot(exprs(cleanDat[[7]])[, "SSC-H"], pch=".")
plot(exprs(allDat[[2]])[, "SSC-H"], pch=".")

}

\keyword{classes}
\keyword{methods}
