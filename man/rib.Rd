\name{rib}
\alias{contract_details}
\alias{contract_by_ISIN}
\alias{executions}
\alias{order_status}
\title{
  Helper Functions for TWS
}
\description{%

  Several convenience functions for connecting synchronously
  to the TWS.

}
\usage{%
executions(port = 7496, clientId = 1)
order_status(port = 7496, clientId = 1)
}
\arguments{%
  \item{port}{
    integer
  }
  \item{clientId}{
    integer
  }
}
\details{%

  The functions follow the same pattern: connect to the TWS,
  send requests, receive and process results, and disconnect.
  Results are typically arranged as data frames.

  \bold{Warning}: these functions are very experimental, and
  returned results may change.

}
\value{%

  A \code{\link{data.frame}}. If there are no executions,
  orders or positions, the functions return \code{NULL}
  invisibly.

}
\references{%
  \url{https://www.interactivebrokers.com/campus/ibkr-api-page/twsapi-doc/}
}
\author{
  Enrico Schumann
}
\seealso{
  \code{\link{ib_hist_data}}
}
\examples{
\donttest{

  ## Functions require an account with Interactive Brokers
  ## and a running TWS.

  executions(port = 7496)

  ## fetch details for contract via ISIN + market
  contr <- rib::Contract
  contr$secIdType <- "ISIN"
  contr$secId <- "DE0005557508"
  contr$exchange <- "IBIS"
  contr <- contract_details(contr)
}
}
