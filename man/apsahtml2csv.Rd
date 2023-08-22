\name{apsahtml2csv}
\Rdversion{1.1}
\alias{apsahtml2csv}
\title{Read, parse, and write to a .csv file APSA eJobs html files}
\description{Reads American Political Science Association (APSA) ``eJobs'' html files, parses the content of these files into a format for muRL to read, and writes that content to a .csv file.}
\usage{
apsahtml2csv(directory, file.name, file.ext = ".htm", verbose = TRUE)
}
\arguments{
  \item{directory}{a character string specifying the directory to which a set of APSA job announcement web pages have been downloaded.}
  \item{file.name}{a character string specifying the name of the file to which the data should be written.}
  \item{file.ext}{a character string specifying the extension of the files from which the data will be harvested.}
  \item{verbose}{a logical specifying whether the file name and current working directory should be printed.}
}

\details{
After logging in to eJobs, the job announcement site of the American Political Science Association (APSA), the user can search for and find the APSA web page announcing a single job listing.  The user can download the html from several such pages (usually with a simple ``Save As'' command, depending on one's operating system).  \code{apsahtml2csv} then parses the html code from these pages, and sorts and stores the relevant content.  A \code{.csv} file is written containing this content.

If the user downloads the APSA webpages using a different (or no) file extension, that extension (or "") should be specified using the \code{file.ext} argument.  Because \code{apsahtml2csv} uses the value of \code{file.ext} in a \code{grep} command, we strongly recommend that the directory specified by \code{directory} include only the downloaded webpages, and no other files or directories.

Institutions are inconsistent in how they enter the names of their jobs' contact representatives.  Thus, some tweaking of the output of \code{apsahtml2csv} may be required in order to create a \code{.csv} file that can be seemlessly read by \code{read.murl}.  Specifically, the user may have to take the single column of the output of \code{apsahtml2csv} called \code{contact}, and create columns called \code{title}, \code{fname}, and \code{lname}.  Additionally, the user may have to adjust the \code{position} and \code{subfield} columns, and institutions may report these somewhat differently.  
}

\value{An \R dataframe is created and a \code{.csv} file is written.  These include columns containing the APSA job listing ID number, the date the job advertisement was posted, the type of institution, the title and subfield of the position, the start date, salary, and region, the name of the institution and department, the name, address, city, state, ZIP code, and phone number of the individual to contact, the department or institution's web address, and a full paragraph description of the position.

The full paragraph description is stored in a column named \code{desc}.  Due to the current parsing strategy, this field may include some excess characters from the APSA html page.  
}

\author{
Ryan T. Moore \email{rtm@american.edu} and Andrew Reeves \email{reeves@wustl.edu}
}

\seealso{\code{\link{read.murl}}}

%\examples{}

\keyword{IO}