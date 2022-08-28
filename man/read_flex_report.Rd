\name{read_flex_report}
\alias{read_flex_report}
\title{
  Read Flex Report
}
\description{
  Read reports returned by the Flex Web Service.
}
\usage{
read_flex_report(file,
                 date.format = "yyyy-MM-dd",
                 time.format = "HH:mm:ss",
                 date.time.separator = ",",
                 \dots)
}
\arguments{
  \item{file}{
    character: filename for the downloaded report
  }
  \item{date.format}{
    string
  }
  \item{time.format}{
    string
  }
  \item{date.time.separator}{
    string
  }
  \item{\dots}{
    additional settings
  }

}
\details{

  Reads CSV reports from Flex Queries.




}
\value{
a list of data.frames

}
\references{
  \url{https://www.interactivebrokers.com/en/software/am/am/reports/flex_web_service_version_3.htm}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{flex_web_service}}
}
\examples{
\dontrun{
file <- "~/Downloads/Net_Asset_Value_NAV_in_Base"
ans <- read_flex_report(file)
}
}
