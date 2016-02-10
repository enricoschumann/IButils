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
             directory, barSize, durationStr = NULL, whatToShow,
             start = as.POSIXct(Sys.Date() - 30), end = Sys.time(),
             verbose = TRUE)
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
    TRADES, MIDPOINT, etc.
}
  \item{start}{
    POSIXct
}
  \item{end}{
    POSIXct
}
  \item{verbose}{
    logical
}
}
\details{
Few.
}
\value{
  A character vector: the names of the file (including the directory)
  that have been saved.
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link[IBrokers]{reqHistoricalData}}
}
\examples{
\dontrun{
require("IButils")

Symbol <- "ZQ   SEP 16"
Security_Type  <- "FUT"
Exchange <- "ECBOT"
Currency <- "USD"

id <- "zq201609"

download_dir <- "~/Trading/Data/IB_downloads/"

barSize <- "5 mins"
whatToShow <- "MIDPOINT"

start <- structure(1423526400, class = c("POSIXct", "POSIXt"))
end <- start + 86400*20

ib_hist_data(Symbol = Symbol,
             Security_Type = Security_Type,
             Exchange = Exchange,
             Currency = Currency,
             id = id,
             directory = download_dir,
             barSize = barSize,
             whatToShow = whatToShow,
             start = start,
             end = start+86400*40)
}
}
