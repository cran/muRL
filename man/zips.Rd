\name{zips}
\Rdversion{1.1}
\alias{zips}
\docType{data}
\title{A .tab file of US ZIP code data for mapping recipients}

\description{A .tab file of United States ZIP code data for mapping recipients.  Called by \code{zip.plot} to match ZIP codes from letters to latitude and longitude coordinates, and then plot latitudes and longitudes on a user-selected map type.}

\usage{data(zips)}
\format{
  A data frame with 33309 observations on 4 variables.
  \describe{
    \item{\code{state}}{a factor containing state and territory abbreviations (with levels \code{AK}, \code{AL}, \ldots, \code{WY}).}
    \item{\code{zip}}{a factor containing three-digit, four-digit, five-digit, and three-digit-plus-wildcard formatted ZIP codes (with 33188 levels).}
    \item{\code{lat}}{a numeric vector of latitude coordinates.}
    \item{\code{lon}}{a numeric vector of longitude coordinates.}
  }
}

\details{A few ZIP codes span more than one state, and thus appear more than once in \code{zips}.  See the Examples below for hints on extracting latitude and longitude.
}

\source{The original file upon which \code{zips.tab} is based is available at \code{http://www.census.gov/
tiger/tms/gazetteer/zcta5.txt}, which is linked from \code{http://www.census.gov/
geo/www/gazetteer/places2k.html}.  The US Census Bureau's Geography Division produced these documents.  A few additions to the originals have been made.  See the \code{muRL} CHANGELOG for details.
}

\references{Further information about ZIP Code Tabulation Areas (ZCTAs) is available at 

\code{http://www.census.gov/geo/ZCTA/zcta.html}.
}

\note{Not all US ZIP codes are currently included in this file.  If you have a ZIP code you would like included for plotting, please email the package maintainer with the following four pieces of information:  the state in which the ZIP code is located, the ZIP code itself, the latitude of the ZIP code to six decimal places (such as 38.643248), and the longitude of the ZIP code to six decimal places (such as -75.611025).  Please also provide the city and any other information required to verify the latitude and longitude for inclusion.}

\examples{
data(zips)

summary(zips$lat)
summary(zips$lon)

## Extracting latitude and longitude.
## Create a sample survey data frame with an ID variable, 
##   respondent ZIP code, state, and survey response:
svy1 <- data.frame(id = c(1,2,3,4), zip = c("10001", "10001", "63130", "380HH"), 
          state = c("NY", "NY", "MO", "AR"), resp = c(1,2,1,5))
svy1
## Since ZIP 380HH spans three states, all are included:
svy2 <- merge(svy1, zips, by = "zip", all.x = TRUE) 
svy2
## Merging by ZIP and state omits the duplicate 380HH entries:
svy3 <- merge(svy1, zips, by = c("zip", "state"), all.x = TRUE)
svy3
}

\keyword{datasets}
