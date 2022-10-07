\name{message_codes}
\alias{message_codes}
\docType{data}
\title{
  Error Messages
}
\description{
  A table of message and error codes and their descriptions
}
\usage{data("message_codes")}
\format{
  A data frame with 306 observations on the following 3 variables.
  \describe{
    \item{\code{Code}}{a character vector}
    \item{\samp{TWS message}}{a character vector}
    \item{\samp{Additional notes}}{a character vector}
  }
}
\details{

  Scraped from the official API documentation.

}
\source{
  \url{"https://interactivebrokers.github.io/tws-api/message_codes.html"}
}
%% \references{
%% %%  ~~ possibly secondary sources and usages ~~
%% }
\examples{
data(message_codes)
}
\keyword{datasets}
