ib_hist_data <- function(Symbol, Security_Type, Exchange,
                         Currency,
                         id = NULL,
                         directory,
                         barSize,
                         durationStr = NULL,
                         whatToShow,
                         start = as.POSIXct(Sys.Date()-30), ## POSIXct
                         end   = Sys.time(),                ## POSIXct
                         skip.from,
                         skip.until,
                         skip.tz = "",
                         verbose = TRUE,
                         trim = FALSE,
                         accumulate = FALSE) {

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

    by <- if (barSize  == "1 min")
              60*60*30
          else if (barSize  == "5 mins")
              trunc(60*60*24*4.5)
          else if (barSize  == "1 secs")
              1990
    
    if (is.null(durationStr)) {
        if (barSize  == "1 min") {
            durationStr <- "1 D"
        } else if (barSize  == "5 mins") {
            durationStr <- "5 D"
        } else if (barSize  == "1 secs") {
            durationStr <- "2000 S"
        }
    }
    
    twsc <- twsContract(local = Symbol,
                        sectype  = Security_Type,
                        exch = Exchange,
                        currency = Currency,
                        include_expired ="1",
                            ## if (grepl("OPT|FUT|FOP", Security_Type)) "1" else "0",
                        conId = "", symbol = "", primary = "", 
                        expiry = "", strike = "", right = "", multiplier = "", 
                        combo_legs_desc = "", comboleg = "", secIdType = "", 
                        secId = "")

    t <- start

    if (accumulate)
        all_data <- NULL
    
    while (t < end) {

        t <- min(t + by, end)
        if (barSize  == "1 secs") {
            tmp <- as.POSIXlt(t, tz = skip.tz)
            hms <- format(tmp, "%H:%M:%S")
            if (!missing(skip.until) && hms < skip.until) {
                t <- as.POSIXct(paste(format(t, "%Y-%m-%d"), skip.until))
                message("skipped from ", hms, " to ", as.character(t))
            }
            if (!missing(skip.from) && hms > skip.from) {
                t <- if (!missing(skip.until))
                         as.POSIXct(paste(format(t+86400, "%Y-%m-%d"), skip.until))
                     else
                         as.POSIXct(paste(format(t, "%Y-%m-%d"), "23:59:59"))
                    
            }
            
        }
        
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

            if (trim) {
                ii <- index(res) >= start & index(res) <= end
                times <- times[ii]
                data <- data[ii, , drop = FALSE]
            }
            
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

            if (accumulate) {
                all_data <- if (is.null(all_data))
                                data
                            else
                                rbind(all_data, data)
            }
            
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
    if (accumulate)
        all_data else all_files
}



## require("IButils")
## require("IBrokers")

## Symbol <- "ZQ   SEP 16"
## Security_Type  <- "FUT"
## Exchange <- "ECBOT"
## Currency <- "USD"

## id <- "zq201609"

## download_dir <- "~/Trading/Data/IB_downloads_check/"


## barSize <- "5 mins"
## whatToShow <- "MIDPOINT"

## start <- as.POSIXct(as.Date("2013-01-01"))
## end <- start + 86400*2000

## ib_hist_data(Symbol = Symbol,
##              Security_Type = Security_Type,
##              Exchange = Exchange,
##              Currency = Currency,
##              id = id,
##              directory = download_dir,
##              barSize = barSize,
##              whatToShow = whatToShow,
##              start = start)



## ## ## find files and assemble them
## files <- sort(dir(download_dir, pattern = paste0("^", id)))
## setwd(download_dir)
## alldata <- NULL
## for (f in files) {
##     message("Processing ", f)
##     if (length(readLines(f))) {
##         data <- read.table(f, header = TRUE, sep = ";")
##         names.data <- colnames(data)
##         if (!is.null(alldata))
##             data <- data[data[["timestamp"]] > max(alldata[["timestamp"]]), ]
##         alldata <- rbind(alldata, data)
##     }
##     ## file.remove(f)
## }

## plot(.POSIXct(alldata[["timestamp"]]), alldata[["close"]], type = "S")


## z <- zoo(alldata[["close"]],
##          .POSIXct(alldata[["timestamp"]]))

## ##save(z, file ="~/Aquila/Projects/2016_02_IMK/z.RData")



## directory <- "~/Trading/Data/IB_downloads_check/"
## dir(directory)

combine_files <- function(directory,
                          max.rows = -1,
                          pattern = NULL,
                          verbose = TRUE,
                          prefix = "processed___",
                          delete.processed = FALSE,
                          actual.timestamp = FALSE) {

    cwd <- getwd()
    setwd(directory)
    on.exit(setwd(cwd))

    filenames <- dir(directory, full.names = FALSE)
    if (is.null(prefix)) {
        symbols <- gsub("(.*)_[0-9]+_[0-9]+$", "\\1", filenames)
        symbols <- sort(unique(symbols))        
    }

    for (s in symbols) {
        alldata <- NULL
        files <- filenames[grep(s, filenames)]
        for (f in files) {
            if (verbose)
                message("processing ", f, appendLF=FALSE)

            if (!length(readLines(f, n = 1))) {
                if (verbose)
                    message(" ... skipped (empty file) ... OK")
                next
            }
            data <- read.table(f, header = TRUE, sep = ";", stringsAsFactors=FALSE)
            if (!is.null(alldata))
                data <- data[data[["timestamp"]] > max(alldata[["timestamp"]]), ]
            alldata <- rbind(alldata, data)
            if (verbose)
                message(" ... OK ")
            
        }
        ran <- if (actual.timestamp)
                   range(alldata$timestamp)
               else
                   range(as.numeric(c(gsub(".*_([0-9]+)_[0-9]+$", "\\1", files),
                                      gsub(".*_[0-9]+_([0-9]+)$", "\\1", files))))
        outfile <- paste0(s, "_", paste(ran, collapse = "_"))
        if (verbose)
            message("===> write combined file ", outfile, appendLF = FALSE)
        write.table(alldata, sep = ";",
                    row.names = FALSE, col.names = TRUE,
                    file = outfile)        
        if (verbose)
            message(" ... OK")
        if (verbose)
            message("===> rename files", appendLF = FALSE)
        if (prefix != "")
            file.rename(files, paste0(prefix, files))
        if (verbose)
            message(" ... OK")        
    }
    alldata
}
