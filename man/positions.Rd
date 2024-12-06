\name{positions}
\alias{positions}
\title{
  Positions for All Accounts
}
\description{%

  Fetch positions for all accounts.

}
\usage{%
positions(port = 7496, clientId = 1,
          contractFields = c("conId", "localSymbol", "currency"),
          verbose = TRUE)
}
\arguments{%
  \item{port}{
    integer
  }
  \item{clientId}{
    integer
  }
  \item{contractFields}{

    character; contract information to include, see
    Examples

  }
  \item{verbose}{
    logical
  }
}
\details{%

  Fetch current positions (possibly for more than one
  account) for the running TWS session.


}
\value{%

  A \code{\link{data.frame}} with columns \code{account},
  \code{position} and \code{avgCost}, and contract data
  specified by argument \code{contractFields}.

  There are at least two attributes:
  \describe{
    \item{CashBalances}{a data.frame with columns \code{account},
      \code{currency} and \code{value}}
    \item{Contracts}{a list of contracts}
  }
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

  positions(port = 7496)

  ## contract data
  if (requireNamespace("rib"))
      names(rib::Contract)
  p <- positions(port = 7496,
                 contractFields = c("conId", "localSymbol", "strike",
                                    "lastTradeDateOrContractMonth"))

  ## show cash holdings
  attr(p, "CashBalances")

  ## get contract details for first position
  p[1, ]
  contract_details(attr(p, "Contracts")[[1]])

}
}
