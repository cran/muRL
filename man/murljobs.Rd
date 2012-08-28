\name{murljobs}
\Rdversion{1.1}
\alias{murljobs}
\docType{data}
\title{A sample dataframe of recipient information and addresses}

\description{This sample dataframe of recipient information and addresses includes columns with information required by the \code{muRL} package, as well as auxiliary columns with information related to a hypothetical mailmerge, but not required by \code{muRL}.}

\usage{data(murljobs)}
\format{
  A data frame with 8 observations on the following 15 variables.
  \describe{
    \item{\code{institution}}{a factor containing sample institution names (with levels \code{Christopher College}, \ldots, \code{University of State University}).}
    \item{\code{type}}{an auxiliary factor for sorting sample entries (with level \code{am}).}
    \item{\code{deadline}}{an auxiliary factor containing sample deadlines (with levels \code{1/5/2010}, \code{12/1/2009}).}
    \item{\code{title}}{a factor containing sample recipient titles (with levels \code{Dean}, \ldots, \code{Sargent}).}
    \item{\code{fname}}{a factor containing sample recipient first names (with levels \code{Frank}, \ldots, \code{Tim}).}
    \item{\code{lname}}{a factor containing sample recipient last names (with levels \code{Anderson}, \ldots, \code{Smithers}).}
    \item{\code{dept}}{a factor containing sample recipient information (with levels \code{Department of Political Science}, \code{Department of Politics}).}
    \item{\code{position}}{a factor containing sample position titles (with levels \code{assistant professor}, \ldots, \code{postdoctoral associate}).}
    \item{\code{subfield}}{a factor containing sample recipient information (with levels \code{American politics}, \ldots, \code{Governance Studies}).}
    \item{\code{address1}}{a factor containing sample recipient address first lines (with level \code{Graduate Admissions Committee}).}
    \item{\code{address2}}{a factor containing sample recipient address second lines (with levels \code{11 Smith Rd.}, \ldots, \code{Dept of Political Science}).}
    \item{\code{address3}}{a factor containing sample recipient address third lines (with levels \code{123 Main St}, \ldots, \code{Dept of Rock Music}).}
    \item{\code{city}}{a factor containing sample recipient cities (with levels \code{Allentown}, \ldots, \code{Topeka}).}
    \item{\code{state}}{a factor containing sample recipient states or provinces (with levels \code{CA}, \ldots, \code{WY}).}
    \item{\code{zip}}{a numeric vector containing sample recipient ZIP codes.}
  }
}

\source{Created by package authors.}

\examples{
data(murljobs)
}
\keyword{datasets}
