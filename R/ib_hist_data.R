ib_hist_data <- function(Symbol, Security_Type, Exchange,
                         Currency,
                         id = NULL,
                         directory,
                         barSize,
                         durationStr = NULL,
                         whatToShow,
                         start = as.POSIXct(Sys.Date()-30), ## POSIXct
                         end   = Sys.time(),                ## POSIXct
                         verbose = TRUE) {


    if (end < start)
        stop("end < start")

    all_files <- NULL

    if (is.null(id))
        id <- paste0(Symbol, Security_Type, Exchange, Currency, collapse = "-")

    if (substr(directory, nchar(directory), nchar(directory)) != "/") {
        directory <- paste0(directory, "/")
        warning("trailing backslash added to ", sQuote("directory"),
                ": ", directory)
    }

    by <- if (barSize  == "5 mins")
              trunc(60*60*24*4.5)
          else if (barSize  == "1 secs")
              1990
    
    if (is.null(durationStr)) {
        if (barSize  == "5 mins") {
            durationStr <- "5 D"
        } else if (barSize  == "1 secs") {
            durationStr <- "2000 S"
        }
    }
    
    twsc <- twsContract(local = Symbol,
                        sectype  = Security_Type,
                        exch = Exchange,
                        currency = Currency,
                        include_expired =
                            if (grepl("OPT|FUT|FOP", Security_Type)) "1" else "0",
                        conId = "", symbol = "", primary = "", 
                        expiry = "", strike = "", right = "", multiplier = "", 
                        combo_legs_desc = "", comboleg = "", secIdType = "", 
                        secId = "")

    t <- start

    while (t < end) {

        t <- min(t + by, end)
        
        tws <- twsConnect(sample.int(1e8, 1))
        res <- NULL
        res <- reqHistoricalData(tws,
                                 endDateTime = strftime(t, format = "%Y%m%d %H:%M:%S"),
                                 Contract = twsc,
                                 barSize  = barSize,
                                 duration = durationStr,
                                 useRTH = "0",
                                 whatToShow = whatToShow,
                                 verbose = FALSE,
                                 tickerId = "1")
        close(tws)

        if (is.null(res)) {
            times <- as.integer(c(unclass(t)))
            if (verbose)
                message("request data up to ", .POSIXct(max(times)),
                        ", but received no data")
            fn <- paste0(directory, id, "_", c(unclass(t)), "_",  c(unclass(t)))
            file.create(fn)
        } else {
            res <- as.zoo(res)
            times <- as.integer(
                c(unclass(index(res)))) ## make POSIXct numeric
                                        ## and strip 'tz'
            if (verbose)
                message("request data up to ", t,
                        ", received data from ", .POSIXct(min(times)),
                        " [", weekdays(.POSIXct(min(times)), TRUE), "]",
                        " to ",                .POSIXct(max(times)),
                        " [", weekdays(.POSIXct(max(times)), TRUE), "]")
            
            data <- as.data.frame(as.matrix(res))

            if (whatToShow == "TRADES") {
                data <- data[ , -7L, drop = FALSE]
                cnames <- c("timestamp", "open", "high", "low", "close",
                            "volume", "vwap", "count")
            } else if (whatToShow == "MIDPOINT" ||
                       whatToShow == "BID" ||
                       whatToShow == "ASK" ) {
                data <- data[ , 1:4, drop = FALSE]
                cnames <- c("timestamp", "open", "high", "low", "close")
            } else
                stop("unknown ibpricetype")
            
            data <- cbind(times, data)
            colnames(data) <- cnames
            
            fn <- paste0(directory, id, "_", min(times), "_", max(times))
            
            write.table(data, sep = ";",
                        row.names = FALSE, col.names = TRUE,
                        file = fn)                

        }
        all_files <- c(all_files, fn)

        if (.POSIXct(max(times)) >= end)
            break 

        Sys.sleep(10)
    }
    all_files
}



require("IButils")

Symbol <- "FGBL MAR 16"
Security_Type  <- "FUT"
Exchange <- "DTB"
Currency <- "EUR"

id <- "fgbl201603"

download_dir <- "~/Trading/Data/IB_downloads_check/"


barSize <- "1 secs"
whatToShow <- "TRADES"

start <- as.POSIXct(as.Date("2015-09-1"))
## end <- start + 86400*20

ib_hist_data(Symbol = Symbol,
             Security_Type = Security_Type,
             Exchange = Exchange,
             Currency = Currency,
             id = id,
             directory = download_dir,
             barSize = barSize,
             whatToShow = whatToShow,
             start = start)



## ## find files and assemble them
files <- sort(dir(download_dir, pattern = paste0("^", id)))
setwd(download_dir)
alldata <- NULL
for (f in files) {
    message("Processing ", f)
    if (length(readLines(f))) {
        data <- read.table(f, header = TRUE, sep = ";")
        names.data <- colnames(data)
        if (!is.null(alldata))
            data <- data[data[["timestamp"]] > max(alldata[["timestamp"]]), ]
        alldata <- rbind(alldata, data)
    }
    ## file.remove(f)
}

plot(.POSIXct(alldata[["timestamp"]]), alldata[["close"]], type = "S")
