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
    character (not numeric!)
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
    logical: sets the 'quiet' argument of
    \code{\link{download.file}}
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

  The function will first send \code{token} and
  \code{query} to IB. If the response does not contain
  a \preformatted{<Status>Success</Status>}, the
  complete response will be printed and a value of 1 is
  invisibly returned.

  Otherwise, an attempt to download the report is made,
  and the return value of \code{\link{download.file}}
  will be invisibly returned.

  However, even if the download succeeds, the file may
  contain error messages or warnings. Even mere
  \sQuote{messages} often indicate that something did
  not work (e.g. particular accounts could not be
  included). \code{flex_web_service} will print
  messages or warnings that it finds in the files, and
  the return value will be invisible 1.  The downloaded
  file will not be written in such a case, unless
  \code{no.write.msg} and \code{no.write.warn} are set
  to \code{FALSE}; both arguments default to
  \code{TRUE.}


}
\references{
  \url{https://ibkrcampus.com/ibkr-api-page/flex-web-service/}
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

file <- "~/Downloads/Net_Asset_Value_NAV_in_Base"
ans <- read_flex_report(file)

}
}
