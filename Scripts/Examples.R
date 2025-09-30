library("tsdb")
library("zoo")
library("plotseries")
library("IButils")
library("rib")

download.dir <- "~/Downloads/IB/data"



## Fetch daily data (ETF)
C <- contract_by_ISIN("DE000A0H08H3", "IBIS")

data <- ib_hist_data(
    Symbol   = C$localSymbol,
    Exchange = C$exchange,
    Currency = C$currency,
    Security_Type = C$secType,
    start = as.Date("2025-01-01"),
    whatToShow = "TRADES",
    barSize = "1 day",
    duration = "100 D",
    directory = download.dir)


### ... and load files
ts <- NULL
for (file in data) {
    ts1 <- read_ts_tables(file,
                          dir = download.dir,
                          return.class = "zoo",
                          column.names = "%column%")
    ts <- rbind(ts, ts1)
}
plotseries(ts[, "close"])

### ... combine files
combine_files(download.dir)




## Fetch daily data, with 'accumulate'
C <- IButils:::contract_by_ISIN("DE000A0H08H3", "IBIS")
data <- ib_hist_data(
    Symbol = C$localSymbol,
    Exchange = C$exchange,
    Security_Type = C$secType,
    Currency = C$currency,
    start = as.Date("2025-01-01"),
    whatToShow = "TRADES",
    barSize = "1 day",
    duration = "300 D",
    directory = download.dir,
    accumulate = TRUE)
plotseries(data[["close"]], data[["timestamp"]])


## fetch 5-min bars
data <- ib_hist_data(
    Symbol = C$localSymbol,
    start = as.POSIXct(as.Date("2024-01-01 07:00:00")),
    Exchange = C$exchange,
    Security_Type = C$secType,
    Currency = C$currency,
    whatToShow = "TRADES",
    barSize = "5 mins",
    duration = "10 D",
    directory = download.dir)


##
data <- ib_hist_data(
    Symbol = "FGBL 20251208 M",
    Exchange = "EUREX",
    Security_Type = "FUT",
    Currency = "EUR",
    start = as.POSIXct(as.Date("2025-09-01")),
    whatToShow = "TRADES",
    barSize = "1 min",
    duration = "2 D",
    directory = download.dir)









## Help page
contr <- rib::Contract
contr$secIdType <- "ISIN"
contr$secId <- isin <- "DE0005557508"
contr$exchange <- "IBIS"
contr <- contract_details(contr)[[1]]
contr <- contr$contract


barSize <- "5 mins"
whatToShow <- "MIDPOINT"

start <- structure(Sys.time() - 86400 * 7,
                   class = c("POSIXct", "POSIXt"))

ib_hist_data(contr,
             id = "DE0005557508__XETRA",
             directory = download.dir,
             barSize = barSize,
             whatToShow = whatToShow,
             start = start,
             end = Sys.time())
