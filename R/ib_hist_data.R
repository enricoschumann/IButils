ib_hist_data <-
function(Symbol,
         Security_Type,
         Exchange,
         Currency,
         id = NULL,
         directory,
         barSize,
         durationStr = NULL,
         whatToShow = "TRADES",
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
         filename = "%id%__%start%__%end%",
         filename.datetime.fmt = identity,
         backend = "rib",
         clientId = NULL) {

    t.type <- "POSIXct"
    if (grepl("day", barSize))
        t.type <- "Date"

    if (!dir.exists(directory))
        stop(sQuote("directory"), " does not exist")

    all.files <- NULL
    all.data <- NULL

    if (is.character(start)) {
        ## TODO transform date to posix
        start <- as.POSIXct(start)
    }

    if (is.character(end))
        ## TODO transform date to posix
        end <- as.POSIXct(end)

    if (end < start)
        stop("end < start")

    if (is.null(id))
        id <- paste0(if (!is.null(Symbol)) Symbol,
                     if (!is.null(Security_Type)) Security_Type,
                     if (!is.null(Exchange)) Exchange,
                     if (!is.null(Currency)) Currency,
                     collapse = "-")

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
        } else if (barSize  == "1 day") {
            durationStr <- "68 Y"
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
            all.data <- NULL

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
                    all.data <- if (is.null(all.data))
                                    data
                                else
                                    rbind(all.data,
                                          data[!(data$timestamp %in% all.data$timestamp), ])
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
            all.files <- c(all.files, fn)

            if (.POSIXct(max(times)) >= end)
                break

            if (do.wait)
                Sys.sleep(10L)
        }
        if (accumulate)
            all.data
        else
            all.files


    } else if (tolower(backend) == "rib") {

        .fdt <- function(s, tz = NULL, t.type = "POSIXct") {

            if (t.type == "POSIXct") {
                s <- .POSIXct(s)
                if (is.character(s)) {
                    if (is.null(tz))
                        t <- as.POSIXlt(s)
                    else if (tz == "UTC") {
                        t <- as.POSIXlt(s, tz = "UTC")
                    }
                } else
                    t <- s
                t <- as.POSIXct(t)
                t.utc <- as.POSIXlt(t, tz = "UTC")
            } else if (t.type == "Date") {
                t.utc <- strptime(paste(.Date(s), "235959"),
                                  tz = "UTC",
                                  format = "%Y-%m-%d %H%M%S")
            }

            format(t.utc, "%Y%m%d-%H:%M:%S")
        }

        if (is.list(Symbol))
            contract <- Symbol
        else
            contract <- rib::IBContract(localSymbol = Symbol,
                                        secType  = Security_Type,
                                        exchange = Exchange,
                                        currency = Currency)

        contract$includeExpired <- grepl("OPT|FUT|FOP", contract$secType)

        wr <- .wrap$new(showMessages = TRUE)
        ic <- rib::IBClient$new()

        cid <- if (is.null(clientId))
                   sample(1e7, size = 1) else clientId
        ic$connect(port = port, clientId = cid)
        ic$checkMsg(wr)
        on.exit(ic$disconnect())

        tickerId <- "1"
        ic$reqHeadTimestamp(tickerId = tickerId,
                            contract = contract,
                            whatToShow = whatToShow,
                            useRTH = useRTH,
                            formatDate = "2")
        while (!ic$checkMsg(wr, timeout = 0.5))
            Sys.sleep(0.1)

        head <- .POSIXct(as.numeric(wr$Data$headTimestamp[["1"]]))
        if (length(head) && start < head) {
            message("start changed to ", head)
            start <- head
        }

        if (t.type == "Date") {
            start <- as.Date(as.POSIXlt(start))
            start <- unclass(start)

            end   <- as.Date(as.POSIXlt(end))
            end   <- unclass(end)

        } else if (t.type == "POSIXct") {
            ## start/end become numeric, eg 1731312000
            start <- c(unclass(start))
            end   <- c(unclass(end))

        }

        end1 <- end
        while (end1 > start) {

            endDateTime <- .fdt(end1, t.type = t.type)

            rid <- sample.int(1e7, size = 1)
            ic$reqHistoricalData(as.character(rid),
                                 contract = contract,
                                 endDateTime = endDateTime,
                                 formatDate = 2,
                                 durationStr = durationStr,
                                 barSize = barSize,
                                 useRTH = useRTH,
                                 keepUpToDate = FALSE,
                                 whatToShow = whatToShow)

            n <- ic$checkMsg(wr, timeout = 0.5)
            res <- wr$Data$historicalData[[as.character(rid)]]

            done <- !is.null(res)
            try.start <- Sys.time()
            i <- 0
            while (!done && i < 10) {
                message("no data: wait")
                Sys.sleep(i)
                i <- i + 1
                n <- ic$checkMsg(wr, timeout = 0.5)
                res <- wr$Data$historicalData[[as.character(rid)]]
                if (as.numeric(Sys.time() - try.start, units = "secs") > 30) {
                    break
                }
            }


            if (is.null(res)) {
                message("no data received")
                break
            }

            ## returned timestamp is character:
            ## transform to numeric
            if (t.type == "POSIXct") {
                res$time <- as.numeric(res$time)
            } else if (t.type == "Date") {
                res$time <- unclass(as.Date(res$time, format = "%Y%m%d"))
            }
            end1 <- min(res$time)

            if (whatToShow == "TRADES") {
                res <- res[ , 1:8, drop = FALSE]
                cnames <- c("timestamp", "open", "high", "low", "close",
                            "volume", "vwap", "count")
            } else if (whatToShow == "MIDPOINT" ||
                       whatToShow == "BID" ||
                       whatToShow == "ASK" ) {
                res <- res[ , 1:5, drop = FALSE]
                cnames <- c("timestamp", "open", "high", "low", "close")
            } else
                stop("unknown ibpricetype")

            colnames(res) <- cnames

            if (accumulate) {
                all.data <- rbind(res[!res$timestamp %in% all.data$timestamp, ],
                                  all.data)
            }

            R <- range(res[["timestamp"]])
            start.label <- filename.datetime.fmt(R[1])
            end.label   <- filename.datetime.fmt(R[2])
            fn1 <- textutils::fill_in(filename,
                                      id = id,
                                      start = start.label,
                                      end   = end.label,
                                      whatToShow = whatToShow,
                                      delim = c("%", "%"))
            all.files <- c(all.files, fn1)

            write.table(res,
                        sep = sep,
                        row.names = FALSE,
                        col.names = TRUE,
                        quote = FALSE,
                        file = file.path(directory, fn1))


            if (end1 <= start)
                break

            Sys.sleep(14)
            ic$checkMsg(wr)
        }

        if (accumulate) {
            if (trim) {
                all.data <-
                    all.data[all.data$timestamp >= start &
                             all.data$timestamp <= end, , drop = FALSE]
            }
            if (t.type == "POSIXct") {
                all.data$timestamp <- .POSIXct(all.data$timestamp)

            } else if (t.type == "Date") {
                all.data$timestamp <-
                    .Date(all.data$timestamp)

            }
        }
    }

    if (accumulate)
        all.data
    else
        all.files
}


combine_files <- function(directory,
                          max.rows = -1,
                          pattern = NULL,
                          verbose = TRUE,
                          prefix = "processed___",
                          delete.processed = FALSE,
                          actual.timestamp = FALSE,
                          sep = ",",
                          id.rx = "(.*)__[0-9]+__[0-9]+$") {

    filenames <- list.files(directory,
                            full.names = FALSE,
                            recursive = FALSE,
                            include.dirs = FALSE)
    if (!is.null(prefix)) {
        excl <- dir(directory, full.names = FALSE,
                    pattern = paste0("^", prefix))
        filenames <- setdiff(filenames, excl)
    }
    symbols <- gsub(id.rx, "\\1", filenames)
    symbols <- sort(unique(symbols))
    alldata <- NULL

    for (s in symbols) {
        alldata <- NULL
        files <- filenames[grep(s, filenames)]
        for (f in files) {
            if (verbose)
                message("processing ", f, appendLF = FALSE)

            len <- length(readLines(file.path(directory, f), n = 2))
            if (len < 2L) {
                if (verbose)
                    message(" ... skipped (empty file) ... OK")
                next
            }
            data <- read.table(file.path(directory, f),
                               header = TRUE, sep = sep,
                               stringsAsFactors = FALSE)
            if (!is.null(alldata))
                data <- data[!data[["timestamp"]] %in% alldata[["timestamp"]], ]
            alldata <- rbind(alldata, data)
            if (verbose)
                message(" ... OK ")

        }
        ran <- if (actual.timestamp)
                   range(alldata$timestamp)
               else
                   range(as.numeric(c(gsub(".*__([0-9]+)__[0-9]+$", "\\1", files),
                                      gsub(".*__[0-9]+__([0-9]+)$", "\\1", files))))
        outfile <- paste0(s, "__", paste(ran, collapse = "__"))
        if (verbose)
            message("===> write combined file ", outfile, appendLF = FALSE)
        write.table(alldata,
                    sep = sep,
                    row.names = FALSE,
                    col.names = TRUE,
                    file = file.path(directory, outfile))
        if (verbose)
            message(" ... OK")
        if (verbose)
            message("===> rename files", appendLF = FALSE)
        if (prefix != "") {
            if (outfile %in% files)
                files <- setdiff(files, outfile)
            if (length(files))
                file.rename(file.path(directory, files),
                            file.path(directory, paste0(prefix, files)))
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
