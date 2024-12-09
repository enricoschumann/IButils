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
                 \dots, fill = FALSE)
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
  \item{fill}{
    logical; see \code{\link{read.table}}
  }

}
\details{

  Reads CSV reports from Flex Queries.




}
\value{
a list of data.frames

}
\references{
  \url{https://www.ibkrguides.com/clientportal/performanceandstatements/flex.htm}
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
