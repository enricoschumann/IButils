\name{combine_files}
\alias{combine_files}
\title{
  Combine Data Files
}
\description{
  Combine files of downloaded price data
}
\usage{
combine_files(directory, max.rows = -1, pattern = NULL,
              verbose = TRUE, prefix = "processed___",
              delete.processed = FALSE, actual.timestamp = FALSE,
              sep = ",", id.rx = "(.*)__[0-9]+__[0-9]+$")
}
\arguments{
  \item{directory}{
    character
}
  \item{max.rows}{
    numeric
}
  \item{pattern}{
    character
}
  \item{verbose}{
    logical
}
  \item{prefix}{
    character
}
  \item{delete.processed}{
    logical
}
  \item{actual.timestamp}{
    logical
  }
  \item{sep}{
    character: the column separator
  }

}
\details{
  Read CSV files and combine them into one large file per symbol.
}
\value{
  a data.frame
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{ib_hist_data}}
}
\examples{
## TODO add examples
}
