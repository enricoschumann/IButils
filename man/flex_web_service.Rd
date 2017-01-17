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
  Retrieve flex queries via \code{\link{download.file}}.
}
\value{

  Currently nothing useful. The function is called for
  its side effect: storing the flex query.

}
\references{
  \url{https://www.interactivebrokers.com/en/software/am/am/reports/flex_web_service_version_3.htm}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{ib_hist_data}}
}
\examples{
\dontrun{
flex_web_service(file = "~/my_files/my_report.csv",
                 token = "12345678901234567890", ## character!
                 query = 123)
}
}
