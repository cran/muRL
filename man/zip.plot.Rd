\name{zip.plot}
\Rdversion{1.1}
\alias{zip.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot US ZIP codes, including locations of letter recipients.}
\description{Using United States ZIP codes, plots on a map the location of letter recipients.  State or county boundaries may be displayed.}
\usage{
zip.plot(data, zip.file = system.file("data", "zips.tab", package = 
"muRL"), map.type = "state", cex = 1, col = "black", pch = 20, jitter.factor = NULL, ...)
}
\arguments{
  \item{data}{a dataframe with ZIP codes in a column named '\code{zip}', such as the output of \code{read.murl}.}
  \item{zip.file}{a character string naming a \code{.tab} file with the columns for the latitude and longitude of ZIP codes, such as '\code{zips.tab}' provided in the \code{murl} package (the default).}
  \item{map.type}{the type of map for \code{map()} from the \code{maps} library to create.  See Details for more.}
  \item{cex}{a numerical value giving the amount by which plotting text and symbols should be magnified relative to the default.  Accepts, for example, a vector of values which are recycled.}
  \item{col}{a specification for the plotting color.}
  \item{pch}{the plotting character for \code{map()} from the \code{maps} library to use.}
  \item{jitter.factor}{a numeric specifying by how much points should be jittered before plotting.  See Details below for more.}
  \item{\dots}{other arguments to pass to \code{map()} from the \code{maps} library.  See Details below for more.}
}
\details{
\code{map.type} can be any valid map from the \code{maps} package.  For plotting the location of United States ZIP codes, \code{usa}, \code{state}, or \code{county} should be used.

See \code{help(par)} for more details on \code{cex}, \code{col}, and \code{pch}.

See \code{help(jitter)} for more details on \code{jitter.factor}.  \code{zip.plot} jitters latitude and longitude separately using the same factor. 

To plot only a region within the selected \code{map.type}, include the \code{map} argument \code{region = }.  For example, \code{zip.plot(..., region = ``Maryland'')} would plot only the recipients with ZIP codes in the US state of Maryland.
}

\note{\code{zip.plot} calls the \code{map} function in the \code{maps} package.  The \code{map} function places an object called \code{stateMapEnv} in the user's workspace.
}

\author{
Ryan T. Moore \email{rtm@wustl.edu} and Andrew Reeves \email{areeves@bu.edu}
}

\seealso{\code{\link{read.murl}}, \code{\link{zips}}}

\examples{
## Call murl object of sample addresses
data(murljobs)
zip.plot(murljobs)

## Read .csv to murl object
murljobs <- read.murl(system.file("data", "murljobs.csv", package = "muRL"))
## Specify US state to map
zip.plot(murljobs, map.type = "state", region = "maryland")
}