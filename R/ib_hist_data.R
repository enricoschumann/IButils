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
                         trim = TRUE,
                         accumulate = FALSE) {

    if (end < start)
        stop("end < start")

    all_files <- NULL

    if (is.null(id))
        id <- paste0(Symbol, Security_Type, Exchange, Currency, collapse = "-")

    by <- if (barSize  == "1 min")
              60*60*30
          else if (barSize  == "5 mins")
              trunc(60*60*24*4.5)
          else if (barSize  == "1 secs")
              1990
    
    if (is.null(durationStr)) {
        if (barSize  == "1 min") {
            durationStr <- "3 D"
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
                        include_expired =
                            if (grepl("OPT|FUT|FOP", Security_Type))
                                "1" else "0",
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
            fn <- file.path(directory,
                            paste0(id, "_", c(unclass(t)), "_",  c(unclass(t))))
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
                                rbind(all_data,
                                      data[!(data$timestamp %in% all_data$timestamp), ])
            }
            
            fn <- file.path(directory,
                            paste0(id, "_", min(times), "_", max(times)))
            
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


latest_timestamp <- function(directory, id) {
    files <- sort(dir(directory, pattern = paste0("^", id, "_")))
    latest <- if (length(files))
                  max(as.numeric(gsub(".*_([0-9]+)$","\\1", files)))
              else
                  NA
    latest
}

flex_web_service <- function(file, token, query,
                             version = 3, delay = 2,
                             no.write.msg = TRUE,
                             no.write.warn = TRUE,
                             verbose = TRUE) {
    if (version != 3)
        stop("only version 3 is supported")
    if (!is.character(token))
        warning(sQuote("token"), " should be character")

    u <- paste0("https://gdcdyn.interactivebrokers.com/Universal/servlet/",
                "FlexStatementService.SendRequest",
                "?t=", token,
                "&q=", query,
                "&v=", version)
    res <- readLines(u)
    Sys.sleep(delay)
    if (res[[2L]] == "<Status>Success</Status>") {
        
        ref_code <- gsub("<.?ReferenceCode>", "", res[[3L]])
        u2 <- paste0("https://gdcdyn.interactivebrokers.com/Universal/servlet/",
                    "FlexStatementService.GetStatement?q=", ref_code,
                    "&t=", token, "&v=", version)
        tmp <- tempfile()
        ans <- download.file(u2, tmp, quiet = !verbose)

        content <- readLines(tmp)
        if (any(msg <- grepl("^[\"\']MSG", content))) {
            if (sum(msg) > 1)
                message("** Messages in file ", file, ":")
            else
                message("** Message in file ", file, ":")
            cat(gsub("^[\"\']MSG.*?,", "", content[msg]), sep = "\n")
            if (!no.write.msg)
                file.copy(tmp, file)
            else
                message("=> file ", file, " *not* written")
            ans <- 1
        }
        if (content[[2L]] == "<Status>Warn</Status>") {
            message("** Warning in file ", file, ":")
            cat(gsub("<.?ErrorMessage>", "", content[[4L]]), sep = "\n")
            if (!no.write.warn)
                file.copy(tmp, file)
            else
                message("=> file ", file, " *not* written")
            ans <- 1
        }
        
    } else {
        cat(res, sep = "\n")
        ans <- 1
    }
    invisible(ans)
}
