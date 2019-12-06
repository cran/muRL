\name{write.murl}
\Rdversion{1.1}
\alias{write.murl}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Write a .tex file of recipient information to be processed by LaTeX}

\description{Reads an \R dataframe of letter recipient- and position-specific data, such as the output of \code{read.murl}.  Creates a \code{.tex} file of the relevant data and LaTeX code, which can then be processed directly by pdflatex, for example.}

\usage{
write.murl(object, file.name = "mailmerge.tex", salutation = "Dear", 
sal.punct = ":", address.string = "123 Venus Flytrap Way\\\\\\\\Cincinnati, 
OH 45201\\\\\\\\ \\\\texttt{jfever@wkrp.edu}\\\\\\\\ 
\\\\texttt{http://www.wkrp.edu/jfever}\\\\\\\\513-555-5664", 
pad_if_zip4 = TRUE,
date = "\\\\today", letter.file = NULL, letter.text = NULL, valediction = "Sincerely,", 
signature = "Johnny Fever", opening = "", include.opening = FALSE, 
closing = "", include.closing = FALSE, contact_me = TRUE,
margin_geometry = NULL,
verbose = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{a dataframe of mailmerge data, such as an output from \code{read.murl()}.}
  \item{file.name}{a character string specifying the file name (and optionally, path) for the output \code{.tex} file.  See Details below for more.}
  \item{salutation}{a character string specifying the salutation to be used in the letters.}
  \item{sal.punct}{a character string specifying the punctuation to be used at the end of the salutation.}
  \item{address.string}{a character string specifying the return address to be used in the letters.  Note that two slashes (\code{\\\\}) should be used for every one usual LaTeX slash.}
  \item{pad_if_zip4}{a logical indicating whether to add a leading 0 (zero) to 
  any ZIP code with 4 or 9 characters.  Defaults to \code{TRUE} and warns the user.}
  \item{date}{an optional character string specifying the date.  Defaults to the current date.}
  \item{letter.file}{an optional character string specifying a file containing the body text of the letters.  See Details below for more.}
  \item{letter.text}{an optional character string containing the body text of the letters.  See Details below for more.}
  \item{valediction}{a character string specifying the valediction to be used in the letters.}
  \item{signature}{a character string specifying the signature to be used in the letters.}
  \item{opening}{a character string specifying the opening line to be used in the letters.  See Details below for more.}
  \item{include.opening}{a logical indicating whether an opening, customized line is to be used in the letters.  See Details below for more.}
  \item{closing}{a character string specifying the closing line to be used in the letters.  See Details below for more.}
  \item{include.closing}{a logical indicating whether a closing, letter-customized line is to be used in the letters.  See Details below for more.}
  \item{contact_me}{a logical indicating whether a 'contact me' sentence is added 
  to the closing.  See Details below for more.}
  \item{margin_geometry}{an optional numeric vector of length 4 containing the number of inches of margin for the top, bottom, left, and right of the letter.  See Details below for more.}
  \item{verbose}{a logical indicating whether the \code{file.name} and the current working directory should be printed after the file has been created.}
}
\details{
The dataframe used by \code{write.murl} should include columns for recipients' titles, first names, last names, addresses, cities, states, and ZIP codes, as well as information specific to the position for which the letter is in application.  \code{write.murl} is intended to operate on the output of \code{read.murl}, and thus requires that the column names for the fields above be ``title'', ``fname'', ``lname'', ``address1'' (and ``address2'', etc.), ``city'', ``state'', ``zip'', ``position'', ``subfield'', ``dept'', and ``institution''.  These field names are automatically created by \code{read.murl}.

The user may define the main body text of the letter in at least three ways.  First, \code{write.murl} includes some sample text by default.  The user could simply edit this text in the \code{.tex} file created by \code{write.murl}.  Second, the user could write the body text in a separate file (such as a \code{.txt} file) and specify that file's name using the \code{letter.file} argument.  Third, the user could define the entire body text as a string passed to the \code{letter.text} argument.

If both \code{letter.file} and \code{letter.text} are specified, \code{write.murl} appends the value of \code{letter.string} below the contents of the file specified by \code{letter.file}.

The opening line specified by argument \code{opening} should be of a grammatical form consistent with ``I write to apply for the position in''.  This phrase will then be followed by customized input, using the fields ``position'', ``subfield'', ``dept'', and ``institution'', as in the example in Value below.  To omit such a customized opening line, use the default 
\code{include.opening = FALSE}.  The example in Value below, and thus each letter, will include only the content defined in the LaTeX-defined ``body''.

The closing line specified by argument \code{closing} should be of a grammatical form consistent with ``I will be an asset to the''.  This phrase will then be followed by letter-customized 
input, using the fields ``position'', ``subfield'', and ``instShort''.  To omit such a customized closing line, use the default \code{include.closing = FALSE}.  If \code{contact_me} 
is \code{TRUE} (the default), then the phrase ``Please don't hesitate to contact me if more information would be helpful.'' is included.

If the \code{margin_geometry} argument is specified, the LaTeX package \code{geometry} will 
be used, and the four components of \code{margin_geometry} provide the margins for 
the top, bottom, left, and right of the letters.  If it is not specified, the default 
margins of \code{documentclass\{letter\}} are used.
}

\value{A \code{.tex} file of LaTeX code and recipient-specific content, to be processed directly by LaTeX.  Using the included \code{murljobs.csv} sample data, the \code{.tex} file created by \code{write.murl} includes for each position one code snippet that looks like the following:

\preformatted{
\begin\{letter\}
{Dr. Richard Sanders\\\Graduate Admissions Committee\\\123 Hello Way\\\Frederick MD 21701}
\opening{Dear Dr. Sanders:}
 \body 
\closing{Sincerely,} 
\end{letter}
}

}

\author{Ryan T. Moore \email{rtm@american.edu} and Andrew Reeves \email{reeves@wustl.edu}}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{read.murl}}}

\examples{
data(murljobs)

## Create mailmerge.tex required for LaTeX import
# write.murl(murljobs)

## Specify a file containing the letters' body text
## write.murl(murljobs, letter.file = "mybodytext.txt")

## Specify a string containing the letters' body text
# write.murl(murljobs, letter.text = "This is the whole body of my letters.")

## Specify salutation, valediction options (overwrites previous mailmerge.tex)
# write.murl(murljobs, file.name = "mailmerge.tex", salutation = "Greetings", 
#		     sal.punct = ",", valediction = "Truly Yours,", include.opening = FALSE)

## Specify opening line also (overwrites previous mailmerge.tex)
# write.murl(murljobs, file.name = "mailmerge.tex", salutation = "Greetings", 
#		     sal.punct = ",", valediction = "Truly Yours,", 
#		     opening = "I am applying for the job in", include.opening = TRUE)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{IO}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line