\name{positions}
\alias{contract_details}
\alias{executions}
\alias{order_status}
\alias{positions}
\title{
  Helper Functions for TWS
}
\description{
  Several convenience functions for
  connecting synchronously to the TWS.

}
\usage{
executions(port = 7496, clientId = 1)
order_status(port = 7496, clientId = 1)
positions(port = 7496, clientId = 1)
}
\arguments{
  \item{port}{
    integer
  }
  \item{clientId}{
    integer
}
}
\details{
  The functions follow the same pattern: connect to the TWS,
  send requests, receive and process results, and disconnect.
  Results are typically arranged as data frames.

  Warning: these functions are very experimental, and
  returned results may change.

}
\value{

  A \code{\link{data.frame}}. If there are no executions, orders or
  positions, the functions return \code{NULL} invisibly.


}
%% \references{
%% %% ~put references to the literature/web site here ~
%% }
\author{
  Enrico Schumann
}
%% \note{
%% %%  ~~further notes~~
%% }

%% %% ~Make other sections like Warning with \section{Warning }{....} ~

%% \seealso{
%% %% ~~objects to See Also as \code{\link{help}}, ~~~
%% }
\examples{
\donttest{
  positions()


  ## fetch details for contract via ISIN + market
  contr <- rib::Contract
  contr$secIdType <- "ISIN"
  contr$secId <- "DE0005557508"
  contr$exchange <- "IBIS"
  contr <- contract_details(contr)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
