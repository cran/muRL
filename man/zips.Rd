\name{zips}
\Rdversion{1.1}
\alias{zips}
\docType{data}
\title{A .tab file of US ZIP code data for mapping recipients}

\description{A .tab file of United States ZIP code data for mapping recipients.  Called by \code{zip.plot} to match ZIP codes from letters to latitude and longitude coordinates, and then plot latitudes and longitudes on a user-selected map type.}

\usage{data(zips)}
\format{
  A data frame with 33233 observations on 4 variables.
  \describe{
    \item{\code{state}}{a factor containing state and territory abbreviations (with levels \code{AK}, \code{AL}, \ldots, \code{WY}).}
    \item{\code{zip}}{a factor containing three-digit, four-digit, five-digit, and three-digit-plus-wildcard formatted ZIP codes (with 33178 levels).}
    \item{\code{lat}}{a numeric vector of latitude coordinates.}
    \item{\code{lon}}{a numeric vector of longitude coordinates.}
  }
}

\source{The original file upon which \code{zips.tab} is based is available at \code{http://www.census.gov/tiger/tms/gazetteer/zcta5.txt}, which is linked from \code{http://www.census.gov/geo/www/gazetteer/places2k.html}.  The US Census Bureau's Geography Division produced these documents.
}

\references{Further information about ZIP Code Tabulation Areas (ZCTAs) is available at \code{http://www.census.gov/geo/ZCTA/zcta.html}.
}

\examples{
data(zips)

summary(zips$lat)
summary(zips$lon)
}

\keyword{datasets}
