\name{flex_web_service}
\alias{flex_web_service}
\title{
  Flex Web Service
}
\description{
  Retrieve queries via the Flex Web Service.
}
\usage{
flex_web_service(file, token, query, version = 3)
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
  value of \code{\link{download.file}} will be
  returned. Otherwise, an error occured, and the return
  value will be invisible 1.

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
