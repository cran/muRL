\name{read.murl}
\Rdversion{1.1}
\alias{read.murl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read a .csv file or R dataframe of letter recipients}
\description{Reads a \code{.csv} file or R dataframe of letter recipients, processes the column names for \code{write.murl}, checks whether United States ZIP codes conform to standard formats, and reports potential problems to the user.}
\usage{
read.murl(file = "murljobs.csv", header = TRUE, stringsAsFactors = FALSE, field.title = "title", field.fname = "fname", field.lname = "lname", fields.address = c("address1", "address2", "address3"), field.city = "city", field.state = "state", field.zip = "zipcode", field.position = "position", field.dept = "dept", field.institution = "institution", colClasses = c("character"), ...)
}

\arguments{
  \item{file}{the name of a \code{.csv} file or R dataframe.}
  \item{header}{a logical for whether the first row of the input file or dataframe is a header row.}
  \item{stringsAsFactors}{a logical for whether character strings should be stored as factors with levels.}
  \item{field.title}{a character string giving the name of the column containing recipients' titles (such as ``Doctor", "Mrs.", etc.).}
  \item{field.fname}{a character string giving the name of the column containing recipients' first names.}
  \item{field.lname}{a character string giving the name of the column containing recipients' last names.}
  \item{fields.address}{a vector of character strings giving the name(s) of the column(s) containing recipients' mailing address information.  Each column will be printed as its own row in the mailing address.}
  \item{field.city}{a character string giving the name of the column containing recipients' cities.}
  \item{field.state}{a character string giving the name of the column containing recipients' states or provinces.}
  \item{field.zip}{a character string giving the name of the column containing recipients' United States ZIP or other postal codes.}
  \item{field.position}{a character string giving the name of the column containing recipient-specific information, such as the specific position for which one is applying.}
  \item{field.dept}{a character string giving the name of the column containing additional information, such as the specific department offering the position for which one is applying.}
  \item{field.institution}{a character string giving the name of the column containing additional information, such as the institution offering the position for which one is applying.}
  \item{colClasses}{a vector of character strings indicating the class of each column.  Using \code{c(``character'')} ensures leading zeros in, for example, ZIP codes will be preserved.}
  \item{\dots}{other arguments to pass to \code{read.csv()} if the input file is a .csv file.}
}

\details{
Recipients' addresses are formatted for mailing as follows.  The first row contains the contents of the fields defined by \code{field.title}, \code{field.fname}, and \code{field.lname}.  Each of the fields defined by \code{fields.address} is formatted as a unique row.  The last row contains the contents of the fields defined by \code{field.city}, \code{field.state}, and \code{field.zip}.  

If the input file is an \R dataframe, then the argument \code{\ldots} is ignored.
}

\value{An \R dataframe containing the relevant information for creating a set of standardized but customizable letters to be mailed.}

%%\references{
%% ~put references to the literature/web site here ~
%%}

\author{Ryan T. Moore <\email{rtm@wustl.edu}> and Andrew Reeves <\email{areeves@bu.edu}>}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{write.murl}}, \code{\link{zip.plot}}
}

\examples{
## Specify path to .csv database of sample addresses
fpath <- system.file("data", "murljobs.csv", package = "muRL")

murljobs <- read.murl(fpath)
}

\keyword{IO}