\name{ib_hist_data}
\alias{ib_hist_data}
\title{
  Download Historical Data
}
\description{
  Download historical data from IB API and store them as text files.
}
\usage{
ib_hist_data(Symbol, Security_Type, Exchange, Currency, id = NULL,
             directory, barSize, durationStr = NULL,
             whatToShow = "TRADES",
             start = as.POSIXct(Sys.Date() - 30), end = Sys.time(),
             useRTH = FALSE,
             skip.from, skip.until, skip.tz = "",
             verbose = TRUE, trim = TRUE, accumulate = FALSE,
             port = 7496, sep = ",",
             filename = "\%id\%__\%start\%__\%end\%",
             filename.datetime.fmt = identity,
             backend = "rib",
             clientId = NULL,
             wait = 10, max.wait = 300)
}
\arguments{
  \item{Symbol}{

    character: the local symbol; can also be a contract
    (a list, see \code{\link[rib]{IBContract}}), in
    which case \code{Security_Type}, \code{Exchange} and
    \code{Currency} are ignored

  }

  \item{Security_Type}{
    character
  }

  \item{Exchange}{
    character
  }

  \item{Currency}{
    character
  }

  \item{id}{

    character; optional: the prefix for text files.
    If \code{NULL}, Symbol etc. are pasted together.

  }

  \item{directory}{
    character: the directory in which the files are stored
  }

  \item{barSize}{
    character
  }

  \item{durationStr}{

    character. If \code{NULL}, the function attempts to
    set a reasonably-long duration.

  }

  \item{whatToShow}{
    character: TRADES, MIDPOINT, BID or ASK
  }

  \item{start}{
    POSIXct
  }

  \item{end}{
    POSIXct
  }

  \item{useRTH}{
    logical
  }

  \item{skip.from}{
    character
  }

  \item{skip.until}{
    character
  }

  \item{skip.tz}{
    character
  }

  \item{verbose}{
    logical
  }

  \item{accumulate}{
    logical
  }

  \item{trim}{
    logical
  }
  \item{filename.datetime.fmt}{

    A function, which formats timestamps in filenames.
    Its single argument is the numeric representation
    of a timestamp (obtained, e.g., via
    \code{unclass(<timestamp>)}).

  }

  \item{sep}{
    character: the column separator, defaults to a comma.
  }

  \item{port}{
    the TWS port
  }

  \item{clientId}{either \code{NULL} or an
    integer. Specifying an integer is supported only
    for backend \code{rib}. If \code{NULL}, a random id
    is chosen.
  }

  \item{filename}{character: pattern for the filenames
    of the stored files. May include special patterns
    such as \sQuote{\code{\%id\%}}.
  }

  \item{backend}{a lowercase string or
    \code{NULL}. Supported are \code{ibrokers} or
    \code{rib}. Default is \code{rib}.
  }

}
\details{

  See IB API documentation
  \url{https://interactivebrokers.github.io/} The package
  uses functionality of either package \pkg{IBrokers} or
  \pkg{rib}, depending on the setting of backend.

  The function tries to download the data between end and
  start, beginning at the end, and fetching chunks of data
  defined by \code{durationStr}. See
  \url{https://ibkrcampus.com/campus/ibkr-api-page/twsapi-doc/#duration-per-bar}.

  Each chunk is stored, per default, as a flat file in CSV
  format.


}
\value{

  If \code{accumulate} is \code{TRUE}, a
  \code{\link{data.frame}} of the retrieved data.  If
  \code{accumulate} is \code{FALSE} (the default), a
  character vector: the names of the files (including the
  directory) that have been saved.  Files are written in
  tsdb format, which is CSV with timestamps written as
  their numeric representation.

}
\author{
  Enrico Schumann
}
\seealso{

  \code{\link{combine_files}} for aggregating files

  \code{\link[tsdb]{read_ts_tables}}

}
\examples{
\dontrun{
## define contract via ISIN
contr <- rib::Contract
contr$secIdType <- "ISIN"
contr$secId <- isin <- "DE0005557508"
contr$exchange <- "IBIS"
contr <- contract_details(contr)[[1]]
contr <- contr$contract

download.dir <- "~/Trading/Data/IB_downloads/"

barSize <- "5 mins"
whatToShow <- "MIDPOINT"

## previous 10 days
start <- Sys.time() - 86400 * 10

ib_hist_data(contr,
             id = "DE0005557508__XETRA",
             directory = download.dir,
             barSize = barSize,
             whatToShow = whatToShow,
             start = start,
             end = Sys.time())
}
}
