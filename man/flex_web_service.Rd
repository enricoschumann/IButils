\name{flex_web_service}
\alias{flex_web_service}
\title{
  Flex Web Service
}
\description{
  Retrieve queries via the Flex Web Service.
}
\usage{
flex_web_service(file, token, query, version = 3, delay = 2,
                 no.write.msg = TRUE, no.write.warn = TRUE,
                 verbose = TRUE)
}
\arguments{
  \item{file}{
    character: filename for the downloaded report
}
  \item{token}{
    character
}
  \item{query}{
    integer
}
  \item{version}{
    integer; currently only 3 is supported
}
  \item{delay}{
    integer: number of seconds to wait between sending
    the token and retrieving the flex-data report
}
  \item{no.write.msg}{
    logical: do not write file if it contains a message
}
  \item{no.write.warn}{
    logical: do not write file if it contains a warning
}
  \item{verbose}{
    logical: sets the 'quiet' argument of \code{\link{download.file}}
}

}
\details{

  Retrieve flex queries via
  \code{\link{download.file}}.

  The function also checks whether the downloaded file
  contains messages from IB (lines that start with
  \dQuote{\code{MSG}}).
  
}
\value{

  The function is called for its side effect:
  downloading and storing the flex query.

  If an attempt to download the report is made, the
  return value of \code{\link{download.file}} will be
  returned. 

  However, even if the download succeeds, the file may
  contain an error message. Even mere \sQuote{messages}
  often indicate that something did not work
  (e.g. particular accounts could not be
  included). Then, the return value will be invisible
  1.

}
\references{
  \url{https://www.interactivebrokers.com/en/software/am/am/reports/flex_web_service_version_3.htm}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{ib_hist_data}}, \code{\link{download.file}}
}
\examples{
\dontrun{
flex_web_service(file = "~/my_files/my_report.csv",
                 token = "12345678901234567890", ## character!
                 query = 123)
}
}
