ib_hist_data <-
function(Symbol,
         Security_Type,
         Exchange,
         Currency,
         id = NULL,
         directory,
         barSize,
         durationStr = NULL,
         whatToShow,
         start = as.POSIXct(Sys.Date() - 30), ## POSIXct
         end   = Sys.time(),                  ## POSIXct
         useRTH = FALSE,
         skip.from,
         skip.until,
         skip.tz = "",
         verbose = TRUE,
         trim = TRUE,
         accumulate = FALSE,
         port = 7496,
         sep = ",",
         filename = "%id%_%start%_%end%",
         backend = NULL,
         clientId = NULL) {

    if (!dir.exists(directory))
        stop(sQuote("directory"), " does not exist")

    all_files <- NULL
    if (end < start)
        stop("end < start")

    if (is.null(id))
        id <- paste0(Symbol, Security_Type, Exchange, Currency, collapse = "-")

    if (barSize == "5 min") {
        if (verbose)
            message("NOTE 'barSize' changed from '", barSize, "' to 5 mins")
        barSize <- "5 mins"
    }


    if (is.null(durationStr)) {
        if (barSize  == "1 min") {
            durationStr <- "3 D"
        } else if (barSize  == "5 mins") {
            durationStr <- "5 D"
        } else if (barSize  == "1 secs") {
            durationStr <- "2000 S"
        } else {
            if (is.null(durationStr))
                stop("duration not specified")
        }
    }

    do.wait <- FALSE
    if (grepl("secs", barSize)) {
        ## https://interactivebrokers.github.io/tws-api/historical_limitations.html
        ## 1 secs , 5 secs , 10 secs , 15 secs , 30 secs
        do.wait <- TRUE
    }


    if (is.null(backend) ||
        tolower(backend) == "ibrokers") {


        by <- if (barSize  == "1 min")
                  60*60*30
              else if (barSize  == "5 mins")
                  trunc(60*60*24*4.5)
              else if (barSize  == "1 secs")
                  1990


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

            tws <- twsConnect(sample.int(1e8, 1), port = port)
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
                    message("request data up to ",
                            .POSIXct(max(times)),
                            ", but received no data")
                fn0 <- fill_in(filename,
                               id = id,
                               start = c(unclass(t)),
                               end = c(unclass(t)),
                               whatToShow = whatToShow,
                               delim = c("%", "%"))
                fn <- file.path(directory, fn0)
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

                fn0 <- fill_in(filename,
                               id = id,
                               start = min(times),
                               end = max(times),
                               whatToShow = whatToShow,
                               delim = c("%", "%"))
                fn <- file.path(directory, fn0)

                write.table(data, sep = sep,
                            row.names = FALSE,
                            col.names = TRUE,
                            file = fn)

            }
            all_files <- c(all_files, fn)

            if (.POSIXct(max(times)) >= end)
                break

            if (do.wait)
                Sys.sleep(10L)
        }
        if (accumulate)
            all_data
        else
            all_files


    } else if (tolower(backend) == "rib") {

        end0 <- end

        if (is.list(Symbol))
            contract <- Symbol
        else
            contract <- rib::IBContract(localSymbol = Symbol,
                                        secType  = Security_Type,
                                        exchange = Exchange,
                                        currency = Currency)

        contract$includeExpired <- grepl("OPT|FUT|FOP", contract$secType)

        H <- NULL
        wrap <- IBWrapHistData$new()
        ic <- rib::IBClient$new()
        cid <- if (is.null(clientId))
                   sample(1e8, size = 1) else clientId
        ic$connect(port = port, clientId = cid)
        on.exit(ic$disconnect())

        wait <- 0.2
        ic$checkMsg(wrap, timeout = wait)

        tickerId <- 1
        ic$reqHeadTimestamp(tickerId = tickerId,
                            contract = contract,
                            whatToShow = whatToShow,
                            useRTH = useRTH,
                            formatDate = "2")
        while (!ic$checkMsg(wrap, timeout = wait)) {
            Sys.sleep(0.1)
        }
        earliest <- as.numeric(wrap$context$headTimestamp)
        if (is.null(start))
            start <- earliest

        if (verbose)
            message("Data available back to ", as.character(.POSIXct(earliest)))
        while (start < end) {
            ## end <- as.POSIXct("2020-02-20 16:00:00", tz = "UTC")

            end <- format(end, "%Y%m%d %H:%M:%S")

            ic$reqHistoricalData(tickerId,
                                 contract,
                                 endDateTime = end,
                                 durationStr = durationStr,
                                 barSizeSetting = barSize,
                                 whatToShow = whatToShow,
                                 useRTH = useRTH,
                                 formatDate = "2",
                                 keepUpToDate = FALSE)

            while (!ic$checkMsg(wrap, timeout = wait)) {
                Sys.sleep(0.1)
            }
            h0 <- wrap$context$historical
            if (accumulate && !is.null(H[, "timestamp"]))
                h0 <- h0[h0$time < min(H[, "timestamp"]), ]

            if (whatToShow == "TRADES") {
                cnames <- c("timestamp", "open", "high", "low", "close",
                            "volume", "vwap", "count")
            } else if (whatToShow == "MIDPOINT" ||
                       whatToShow == "BID" ||
                       whatToShow == "ASK" ) {
                h0 <- h0[ , 1:5, drop = FALSE]
                cnames <- c("timestamp", "open", "high", "low", "close")
            } else
                stop("unknown ibpricetype")

            h0[[1L]] <- as.numeric(h0[[1L]])
            h0 <- as.matrix(h0)
            colnames(h0) <- cnames

            fn0 <- fill_in(filename,
                           id = id,
                           start = min(h0[, 1L]),
                           end   = max(h0[, 1L]),
                           whatToShow = whatToShow,

                           delim = c("%", "%"))

            fn <- file.path(directory, fn0)
            all_files <- c(all_files, fn)

            write.table(h0,
                        sep = sep,
                        row.names = FALSE,
                        col.names = TRUE,
                        file = fn)
            if (accumulate)
                H <- rbind(h0, H)

            end <- .POSIXct(min(h0[, 1L]))
            message("==> received data back to ", as.character(end))

            if (min(h0[, 1L]) <= earliest || min(h0[, 1L]) <= start)
                break

            if (do.wait)
                Sys.sleep(10L)

        }

        if (accumulate) {
            ## FIXME: transform to data.frame?
            ## H$time <- .POSIXct(as.numeric(H[, "timestamp"]))
            if (trim) {
                H <- H[H[, "timestamp"] >= start &
                       H[, "timestamp"] >= end0, ]
            }
            H
        } else {
            all_files
        }
    }
}

combine_files <- function(directory,
                          max.rows = -1,
                          pattern = NULL,
                          verbose = TRUE,
                          prefix = "processed___",
                          delete.processed = FALSE,
                          actual.timestamp = FALSE,
                          sep = ",") {

    cwd <- getwd()
    setwd(directory)
    on.exit(setwd(cwd))

    filenames <- list.files(directory,
                            full.names = FALSE,
                            recursive = FALSE,
                            include.dirs = FALSE)
    if (!is.null(prefix)) {
        excl <- dir(directory, full.names = FALSE,
                    pattern = paste0("^", prefix))
        filenames <- setdiff(filenames, excl)
    }
    symbols <- gsub("(.*)_[0-9]+_[0-9]+$", "\\1", filenames)
    symbols <- sort(unique(symbols))
    alldata <- NULL

    for (s in symbols) {
        alldata <- NULL
        files <- filenames[grep(s, filenames)]
        for (f in files) {
            if (verbose)
                message("processing ", f, appendLF=FALSE)

            if (length(readLines(f, n = 2)) < 2) {
                if (verbose)
                    message(" ... skipped (empty file) ... OK")
                next
            }
            data <- read.table(f, header = TRUE, sep = sep, stringsAsFactors=FALSE)
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
        write.table(alldata,
                    sep = sep,
                    row.names = FALSE,
                    col.names = TRUE,
                    file = outfile)
        if (verbose)
            message(" ... OK")
        if (verbose)
            message("===> rename files", appendLF = FALSE)
        if (prefix != "") {
            if (outfile %in% files)
                files <- setdiff(files, outfile)
            if (length(files))
                file.rename(files, paste0(prefix, files))
        }
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

        do.copy <- TRUE
        content <- readLines(tmp)
        if (any(msg <- grepl("^[\"\']MSG", content))) {
            message("** Message", if (sum(msg) > 1) "s",
                    " in file ", file, ":")
            cat(gsub("^[\"\']MSG.*?,", "", content[msg]), sep = "\n")
            if (no.write.msg) {
                do.copy <- FALSE
                message("=> file ", file, " *not* written")
            }
            ans <- 1
        }
        if (content[[2L]] == "<Status>Warn</Status>") {
            message("** Warning in file ", file, ":")
            cat(gsub("<.?ErrorMessage>", "", content[[4L]]), sep = "\n")
            if (no.write.warn) {
                do.copy <- FALSE
                message("=> file ", file, " *not* written")
            }
            ans <- 1
        }

    } else {
        do.copy <- FALSE
        cat(res, sep = "\n")
        ans <- 1
    }
    if (do.copy)
        file.copy(tmp, file, overwrite = TRUE)
    invisible(ans)
}
