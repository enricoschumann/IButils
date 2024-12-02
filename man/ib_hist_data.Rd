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
             backend = "rib",
             clientId = NULL)
}
\arguments{
  \item{Symbol}{
    character
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
    character; optional: the prefix for text files. If NULL, Symbol
    etc. are pasted together
  }
  
  \item{directory}{
    character: the directory in which the files are stored
  }
  
  \item{barSize}{
    character
  }
  
  \item{durationStr}{
    character. If NULL, the function attempts to set a long
    duration. (This argument may be removed in the future since it
    rarely makes sense to set it explicitly.)
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
  
  \item{sep}{
    character: the column separator
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
    \code{rib}. Default is \code{NULL}, which
    translates into \code{ibrokers}.
  }

}
\details{
  See IB API documentation \url{https://interactivebrokers.github.io/}

  The package uses functionality of either package \pkg{IBrokers}
  or \pkg{rib}, depending on the setting of backend.



}
\value{

  If \code{accumulate} is \code{TRUE}, a \code{\link{data.frame}}
  of the retrieved data.  If \code{accumulate} is \code{FALSE}
  (the default), a character vector: the names of the file
  (including the directory) that have been saved.
  
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link[IBrokers]{reqHistoricalData}}
}
\examples{
\dontrun{
contr <- rib::Contract
contr$secIdType <- "ISIN"
contr$secId <- isin <- "DE0005557508"
contr$exchange <- "IBIS"
contr <- contract_details(contr)[[1]]
contr <- contr$contract

download.dir <- "~/Trading/Data/IB_downloads/"

barSize <- "5 mins"
whatToShow <- "MIDPOINT"

start <- structure(Sys.time() - 86400 * 7,
                   class = c("POSIXct", "POSIXt"))

ib_hist_data(Symbol = Symbol,
             Security_Type = Security_Type,
             Exchange = Exchange,
             Currency = Currency,
             id = id,
             directory = download_dir,
             barSize = barSize,
             whatToShow = whatToShow,
             start = start,
             end = Sys.time())
}
}
