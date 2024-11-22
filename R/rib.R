orders <- function(account = "",
                   instrument = "",
                   localSymbol,
                   secType,
                   exchange,
                   currency,
                   conId,
                   amount,
                   action,
                   type,
                   lmtPrice,
                   uuid = TRUE) {

    res <- data.frame(account,
                      instrument,
                      localSymbol,
                      secType,
                      exchange,
                      currency,
                      amount = abs(amount),
                      action,
                      type,
                      lmtPrice,
                      uuid = NA_character_,
                      stringsAsFactors = FALSE)
    if (isTRUE(uuid))
        res$uuid <- uuid::UUIDgenerate(n = nrow(res))
    else if (all(!is.na(uuid)) && (is.character(uuid) || is.numeric(uuid)))
        res$uuid <- uuid

    class(res) <- c("ib_orders", "data.frame")
    res
}

write_orders <- function(orders, dir, ...) {
    ## takes a data.frame of orders
    ## and writes to single files

    for (i in seq_len(nrow(orders)))
        write.table(orders[i, ], sep = ",",
                    row.names = FALSE,
                    col.names = TRUE,
                    file = file.path(dir, orders[i, "uuid"]))

}

read_orders <- function(dir, pattern = NULL, ...) {
    ## read single files into data.frame
    fs <- list.files(dir, full.names = TRUE)
    res <- vector("list", length(fs))
    for (f in fs)
        res[[f]] <- read.table(f, header = TRUE, sep = ",")
    res <- do.call(rbind, res)
    class(res) <- c("ib_orders", "data.frame")
    res
}

send_orders <- function(dir, sent.dir, ...,
                        port = 7496, clientId = 1) {

    fs <- list.files(dir, full.names = TRUE,
                     pattern = "[^~]$")

    wrap <- .wrap$new()
    ic   <- rib::IBClient$new()

    ic$connect(port=port, clientId=clientId)
    ic$checkMsg(wrap)
    on.exit({
        ic$disconnect()
        wrap$Settings$storeMessages <- FALSE

    })

    wrap$Settings$storeMessages <- TRUE

    for (f in fs) {
        ic$reqIds()
        ic$checkMsg(wrap)
        orderId <- wrap$Data$nextValidId
        if (is.null(wrap$Data$nextValidId)) {
            stop("no id")
        }
        wrap$Data$recentMessages <- list()

        o <- read.table(f, header = TRUE, sep = ",")
        contract <- rib::Contract
        contract$localSymbol <- o$localSymbol
        contract$exchange <- o$exchange
        contract$currency <- o$currency
        contract$secType <- o$secType

        order <- rib::Order
        order$action <- o$action
        order$totalQuantity <- o$amount
        order$orderRef <- o$uuid
        order$account <- o$account

        if (o$type == "LMT") {
            order$orderType <- "LMT"
            order$lmtPrice <- o$lmtPrice
        } else {
            order$orderType <- "MKT"
        }


        ic$placeOrder(orderId, contract, order)
        res <- ic$checkMsg(wrap)

        if (length(wrap$Data$recentMessages)) {
            e <- sapply(wrap$Data$recentMessages, `[[`, 2)
            if (any(no.send <- (e %in% 110))) {
                m <- sapply(wrap$Data$recentMessages, `[[`, 3)
                message("file ", f)
                message("==> [ERROR] ", m[no.send])
                next
            }
        }

        ic$reqAllOpenOrders()
        done <- FALSE
        while (!done) {
            count <- ic$checkMsg(wrap)
            if (count == 0L)
                done <- TRUE
            Sys.sleep(0.1)
        }

        known.orders <- sapply(lapply(wrap$Data$orders, `[[`, 3), `[[`, "orderRef")
        if (!o$uuid %in% known.orders) {
            message("file ", f)
            message("==> [WARNING] order ", o$uuid, " not found in current orders")
        }



        copied <- file.copy(f, sent.dir)
        if (copied)
            file.remove(f)
        else
            stop("could not move file ", f)
    }
    invisible(NULL)
    ## for (i in seq_len(nrow(orders))) {
    ##     write.table(orders[i, ], sep = ",",
    ##                 file = file.path(dir, orders$uuid[i]),
    ##                 row.names = FALSE, quote = TRUE)
    ## }
}

update_orders <- function(dir, ...) {
    ## fetch order status, move files to executed etc-.

}


## confirmed -- executions

## open -- created

## sent

## executed

## expired

## canceled

## simulated

####################################

## TODO use specific wraps for every function

IBWrap.IButils <-
    R6::R6Class("IBWrap.IButils",
    class=      FALSE,
    cloneable=  FALSE,
    lock_class= TRUE,

    inherit= rib::IBWrap,

    public = list(
        context = NULL,
        Data = NULL,

        initialize = function() {
                         self$context <- new.env()
                         self$context$orders <- list()
                         self$context$order_status <- list()
                         self$context$completed_orders <- list()
                         self$Data <- new.env()
                         self$Data$executions <- list()
                         self$Data$positions <- list()
                     },


        error = function(id, errorCode, errorString, advancedOrderRejectJson) {
            if (errorCode == 2104)
                cat(errorString, "\n")
            else
                cat(id, errorCode, errorString, "\n")
        },

        nextValidId = function(orderId) {
            self$Data$nextId <- orderId
        },

    managedAccounts = function(accountsList) {
        ## cat("Managed Accounts:", accountsList, "\n")
    },

    orderStatus = function(orderId, status, filled, remaining,
                           avgFillPrice, permId, parentId, lastFillPrice,
                           clientId, whyHeld, mktCapPrice) {
        self$Data$order_status[[as.character(permId)]] <-
            as.list(match.call())[-1L]
    },

    openOrder= function(orderId, contract, order, orderstate) {
        self$Data$orders[[as.character(order$permId)]] <- list(orderId = orderId,
                                                          contract = contract,
                                                          order = order,
                                                          orderstate = orderstate)

        ## self$Data$order <- list(id=orderId, contract=contract, order=order, orderstate=orderstate)
        ## cat("OpenOrder:", orderId, "\n")
    },

    openOrderEnd=       function() {
        ## cat(length(self$Data$orders), " orders found\n")
    },

    commissionReport=   function(commissionReport)
        cat("CommissionReport:", unlist(commissionReport), "\n"),

    completedOrder=     function(contract, order, orderState) {
        ## self$context$completed <- list(contract=contract, order=order, orderstate=orderState)
        self$context$completed_orders[[as.character(order$permId)]] <- list(contract=contract, order=order, orderstate=orderState)
        ## cat("completedOrder.\n")
    },

    completedOrdersEnd= function() {
        cat(length(self$context$completed_orders), " open orders found\n")
    },

    execDetails=        function(reqId, contract, execution) {
        self$Data$executions_contracts[[execution$execId]] <- contract
        self$Data$executions[[execution$execId]] <- execution
        ## cat("ExecDetails:", reqId, contract$symbol, execution$side, execution$shares, execution$price, "\n")
    },

    execDetailsEnd = function(reqId) {
        ## cat("ExecDetailsEnd:", reqId, "\n")
    },

    position = function(account, contract, position, avgCost) {
        L <- list(account = account, contract = contract,
                  position = position, avgCost = avgCost)
        self$Data$positions <- c(self$Data$positions, list(L))
        ## cat("Position:", account, contract$symbol, position, avgCost, "\n")
    },

    positionEnd=        function() {
        ## cat("PositionEnd.\n")
    }

    )
)

positions <- function(port = 7496, clientId = 1,
                      contractFields = c("conId", "localSymbol", "currency"),
                      verbose = TRUE) {

    if (!requireNamespace("rib"))
        stop("package ", sQuote("rib"), " is not available")

    wrap <- .wrap$new(storeMessages = TRUE,
                      showMessages = FALSE)

    ic   <- rib::IBClient$new()

    ic$connect(port = port, clientId = clientId)
    on.exit(ic$disconnect())

    done <- FALSE
    while (!done) {
        count <- ic$checkMsg(wrap)
        if (count == 0L)
            done <- TRUE else Sys.sleep(0.1)
    }

    if (verbose)
        message("Fetch positions ... ", appendLF = FALSE)
    ic$reqPositions()

    done <- FALSE
    while (!done) {
        count <- ic$checkMsg(wrap)
        if (count == 0L)
            done <- TRUE else Sys.sleep(0.1)
    }
    ic$cancelPositions()

    done <- FALSE
    while (!done) {
        count <- ic$checkMsg(wrap)
        if (count == 0L)
            done <- TRUE else Sys.sleep(0.1)
    }
    if (verbose)
        message("[done]")

    ## wrap$Data$positions
    ## ==> account, contract, position, avgcost


    if (verbose)
        message("Fetch account data")

    wrap$Data$accountData <- list()
    wrap$Data$portfolioData <- list()
    accounts <- wrap$Data$managedAccounts

    for (account in accounts) {
        message("    ", account, " ... ", appendLF = FALSE)
        ic$reqAccountUpdates(TRUE, account)
        done <- FALSE
        Sys.sleep(0.1)
        while (!done) {
            count <- ic$checkMsg(wrap)
            if (count == 0L)
                done <- TRUE else Sys.sleep(0.1)
        }
        ic$reqAccountUpdates(FALSE, account)
        message("[done]")
    }



    ## account summary (cash)
    if (verbose)
        message("Fetch account summary ... ", appendLF = FALSE)
    wrap$Data$accountSummary <- NULL
    ic$reqAccountSummary(1, groupName = "All", tags = "$LEDGER")
    done <- FALSE
    while (!done) {
        count <- ic$checkMsg(wrap)
        if (count == 0L)
            done <- TRUE
    }
    ic$cancelAccountSummary(1)
    ic$checkMsg(wrap)
    message("[done]")

    accountSummary <- do.call(rbind, wrap$Data$accountSummary)

    account <- unlist(lapply(wrap$Data$positions, `[[`, "account"))

    Contracts <- lapply(wrap$Data$positions, `[[`, "contract")
    contract <- do.call(rbind, Contracts)

    pos <- unlist(lapply(wrap$Data$positions, `[[`, "position"))

    cost <- unlist(lapply(wrap$Data$positions, `[[`, "avgCost"))

    accountData <- wrap$Data$accountData
    keys   <- unlist(accountData[seq(1, length(accountData), by = 4)])
    values <- unlist(accountData[seq(2, length(accountData), by = 4)])
    currencies <- unlist(accountData[seq(3, length(accountData), by = 4)])
    accounts <- unlist(accountData[seq(4, length(accountData), by = 4)])

    account_  <- NULL
    currency_ <- NULL
    value_    <- NULL
    for (account in unique(accounts)) {
        i <- account == accounts &
             keys == "CashBalance" &
             currencies != "BASE"
        account_  <- c(account_, rep.int(account, sum(i)))
        currency_ <- c(currency_, currencies[i])
        value_    <- c(value_, values[i])
    }

    CashBalance <- data.frame(account = account_,
                              currency = currency_,
                              value = as.numeric(value_))

    ans <- cbind(account,
                 contract = contract[, contractFields],
                 position = pos,
                 avgCost = cost)
    ans <- as.data.frame(ans)
    attr(ans, "AccountSummary") <- accountSummary
    attr(ans, "CashBalances") <- CashBalance
    attr(ans, "Contracts") <- Contracts
    ans
}

order_status <- function(port = 7496, clientId = 1) {

    if (!requireNamespace("rib"))
        stop("package ", sQuote("rib"), " is not available")

    wrap <- .wrap$new()
    ic   <- rib::IBClient$new()

    capture.output(ic$connect(port = port, clientId = clientId))
    on.exit(ic$disconnect())
    capture.output(ic$checkMsg(wrap))

    ## --------------
    ic$reqAllOpenOrders()
    done <- FALSE
    while (!done) {
        count <- ic$checkMsg(wrap)
        if (count == 0L)
            done <- TRUE
    }

    if (length(wrap$Data$orders)) {
        ans1 <- cbind(
            do.call(rbind, wrap$Data$orderStatus)
        )
        ans2 <- cbind(
            do.call(rbind, lapply(wrap$Data$orders, `[[`, "orderId")),
            do.call(rbind, lapply(wrap$Data$orders, `[[`, "contract")),

            do.call(rbind, lapply(wrap$Data$orders, `[[`, "order")),
            do.call(rbind, lapply(wrap$Data$orders, `[[`, "orderstate"))
        )
        ans <- merge(x = ans1, ans2, by = 0,
                     suffixes = c(".order_status", ".open_orders"))
        row.names(ans) <- ans[["Row.names"]]
        ans <- ans[, -1]

        class(ans) <- c("order_status", "data.frame")
        ans

    } else
        invisible(NULL)

}

print.order_status <- function(x, all = FALSE, ...) {
    if (!all)
        print.data.frame(x[, c("account", "orderId.order_status",
                               "status.order_status",
                               "conId", "localSymbol", "action",
                               "totalQuantity", "orderType",
                               "lmtPrice", "tif")], ...)
    else
        print.data.frame(x, ...)
    invisible(x)
}

executions <- function(port = 7496, clientId = 1) {

    if (!requireNamespace("rib"))
        stop("package ", sQuote("rib"), " is not available")

    wrap <- .wrap$new()
    ic   <- rib::IBClient$new()

    capture.output(ic$connect(port = port, clientId = clientId))
    on.exit(ic$disconnect())
    capture.output(ic$checkMsg(wrap))

    ## --------------

    ic$reqExecutions(1, filter = rib::ExecutionFilter)

    ic$checkMsg(wrap)
    ex <- cbind(do.call(rbind, wrap$Data$executions))
    ex <- as.data.frame(ex, stringsAsFactors = FALSE)
    for (cc in seq_len(length(ex)))
        ex[cc] <- unlist(ex[[cc]])
    if (!is.null(ex$time))
        ex$time <- as.POSIXct(ex$time, format = "%Y%m%d %H:%M:%S")
    if (!nrow(ex))
        invisible(NULL)
    else {
        attr(ex, "commissionReport") <- wrap$Data$commissionReport
        ex
    }
}

## send_orders <- function(orders, port = 7496, clientId = 1) {

##     if (!requireNamespace("rib"))
##         stop("package ", sQuote("rib"), " is not available")

##     wrap <- IBWrap.IButils$new()
##     ic   <- rib::IBClient$new(wrap)

##     capture.output(ic$connect(port = port, clientId = clientId))
##     on.exit(ic$disconnect())
##     capture.output(ic$checkMsg(wrap))

##     ## --------------

##     contract <- rib::Contract
##     contract$localSymbol <- "FGBL JUN 20"
##     contract$exchange <- "DTB"
##     contract$currency <- "EUR"
##     contract$secType <- "FUT"

##     order <- rib::Order
##     order$action <- "BUY"
##     order$totalQuantity <- 1
##     order$lmtPrice <- 169
##     order$orderType <- "LMT"
##     order$orderRef <- "Test1"

##     ic$placeOrder(id = wrap$Data$nextId, contract = contract, order)
##     capture.output(ic$checkMsg(wrap))


## }


contract_details <- function(localSymbol,
                             secType,
                             exchange,
                             currency,
                             port = 7496,
                             clientId = 1) {

    if (!requireNamespace("rib"))
        stop("package ", sQuote("rib"), " is not available")

    N <- 0
    if (is.character(localSymbol)) {
        N <- max(length(localSymbol),
                 length(secType),
                 length(exchange),
                 length(currency))
        localSymbol <- rep(localSymbol, N/length(localSymbol))
        secType     <- rep(secType,     N/length(secType))
        exchange    <- rep(exchange,    N/length(exchange))
        currency    <- rep(currency,    N/length(currency))
        Contracts <- vector("list", length = N)
        for (i in seq_len(N)) {
            contract <- rib::Contract
            contract["localSymbol"] <- localSymbol[i]
            contract["secType"] <- secType[i]
            contract["exchange"] <- exchange[i]
            contract["currency"] <- currency[i]

            Contracts[[i]] <- contract
        }

    } else {
        min.common <- 5
        ## single contract
        common <- names(rib::Contract) %in% names(localSymbol)
        if (sum(common) >= min.common) {
            N <- 1
            Contracts <- list()
            contract <- rib::Contract
            contract[names(localSymbol)] <- localSymbol
            Contracts[[1]] <- contract
        } else {
            common <- lapply(localSymbol,
                             function(x) {
                                 sum(names(rib::Contract) %in% names(x)) > min.common
                             })
            if (all(unlist(common))) {
                N <- length(localSymbol)
                Contracts <- localSymbol

            } else {
                stop("see doc for 'LocalSymbol'")
            }
        }
    }

    wrap <- .wrap$new(storeMessages = TRUE,
                      showMessages = FALSE)

    ic   <- rib::IBClient$new()

    msg1 <- capture.output(ic$connect(port = port, clientId = clientId))
    on.exit(ic$disconnect())
    n <- ic$checkMsg(wrap)
    while (n > 0) {
        Sys.sleep(0.2)
        n <- ic$checkMsg(wrap)
    }

    ans <- vector("list", length = N)
    for (i in seq_len(N)) {
        contract <- Contracts[[i]]
        contract$includeExpired <- grepl("OPT|FUT|FOP", contract$secType)
        ic$reqContractDetails("1", contract = contract)
        n <- ic$checkMsg(wrap)
        while (n > 0) {
            Sys.sleep(0.2)
            n <- ic$checkMsg(wrap)
        }

        ans[[i]] <- wrap$Data$contracts[["1"]]
    }

    msg <- as.data.frame(do.call(rbind, wrap$Data$recentMessages))
    colnames(msg) <- c("id", "errorCode",
                       "errorString", "advancedOrderRejectJson")
    attr(ans, "messages") <- msg
    ans
}

ib_hist_data2 <- function(contract,
                          durationString = "3 D",
                          barSizeSetting = "1 min",
                          whatToShow = "MIDPOINT",
                          start,
                          end = Sys.time(),
                          port = 7496,
                          wait = 1) {


    H <- NULL
    wrap <- rib::IBWrapSimple$new()
    ic <- rib::IBClient$new(wrap)
    ic$connect(port=port, clientId=sample(1e8, size = 1))
    on.exit(ic$disconnect())

    ic$checkMsg(timeout = wait)

    tickerId <- 0
    while (start < end) {
        ## end <- as.POSIXct("2020-02-20 16:00:00", tz = "UTC")

        end <- format(end, "%Y%m%d %H:%M:%S")
        ## tickerId <- tickerId + 1

        ic$reqHistoricalData(tickerId,
                             contract,
                             endDateTime = end,
                             durationStr = durationString,
                             barSizeSetting = barSizeSetting,
                             whatToShow = whatToShow,
                             useRTH = TRUE,
                             formatDate = "2",
                             keepUpToDate = FALSE)

        while (!ic$checkMsg(timeout = wait)) {
            ## nrow(wrap$context$historical)
            Sys.sleep(0.1)
        }
        h0 <- wrap$context$historical
        if (!is.null(H$time))
            h0 <- h0[h0$time < min(H$time), ]
        H <- rbind(h0, H)

        end <- .POSIXct(min(H[, "time"]))
    }
    H$time <- .POSIXct(as.numeric(H$time))
    H
}
