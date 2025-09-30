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
    string: where to read files
  }
  \item{max.rows}{%

    numeric: maximum number of rows to put into a single file;
    currently ignored

  }
  \item{pattern}{
    string: passed to \code{\link{list.files}}
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
  \item{actual.timestamp}{%

    logical: if \code{TRUE}, use the actual min/max timestamp
    of the time-series in the name of the resulting file; if
    \code{FALSE}, use the min/max from the input filenames

  }
  \item{sep}{%

    character: the column separator

  }
}
\details{

  Read \acronym{CSV} files and combine them into one large
  file per symbol.

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
