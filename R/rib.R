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

send_orders <- function(dir, sent.dir, ..., port = 7496, cliendId=1) {

    fs <- list.files(dir, full.names = TRUE,
                     pattern = "[^~]$")

    wrap <- wrap0$new()
    ic   <- rib::IBClient$new(wrap)

    ic$connect(port=7496, clientId=1)
    capture.output(ic$checkMsg(wrap))
    on.exit(ic$disconnect())

    if (is.null(wrap$Data$nextId)) {
        ic$reqIds()
        ic$checkMsg(wrap)
    }

    for (f in fs) {
        o <- read.table(f, header = TRUE, sep = ",")
        ic$reqIds()
        ic$checkMsg(wrap)
        orderId <- wrap$Data$nextId

        contract <- rib::Contract
        contract$localSymbol <- o$localSymbol
        contract$exchange <- o$exchange
        contract$currency <- o$currency
        contract$secType <- o$secType

        order <- rib::Order
        order$action <- if (o$amount > 1) "BUY" else "SELL"
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

positions <- function(port = 7496, clientId = 1) {

    if (!requireNamespace("rib"))
        stop("package ", sQuote("rib"), " is not available")
    
    wrap <- .wrap$new()
    ic   <- rib::IBClient$new()
    
    capture.output(ic$connect(port = port, clientId = clientId))
    on.exit(ic$disconnect())
    capture.output(ic$checkMsg(wrap))
    
    ic$reqPositions()
    
    done <- FALSE
    while (!done) {
        count <- ic$checkMsg(wrap)
        if (count == 0L)
            done <- TRUE
    }
    
    ic$cancelPositions()
    ic$checkMsg(wrap)
    
    account <- unlist(lapply(wrap$Data$positions, `[[`, "account"))
    
    contract <- lapply(wrap$Data$positions, `[[`, "contract")
    contract <- do.call(rbind, contract)
    
    pos <- unlist(lapply(wrap$Data$positions, `[[`, "position"))
    
    cost <- unlist(lapply(wrap$Data$positions, `[[`, "avgCost"))
    
    ans <- cbind(account,
                 contract,
                 position = pos,
                 avg.cost = cost)
    as.data.frame(ans)
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
                             port = 7496, clientId = 1) {

    if (!requireNamespace("rib"))
        stop("package ", sQuote("rib"), " is not available")

    wrap <- wrap0$new()
    ic   <- rib::IBClient$new()

    msg1 <- capture.output(ic$connect(port = port, clientId = clientId))
    on.exit(ic$disconnect())
    msg2 <- capture.output(n <- ic$checkMsg(wrap))

    if (is.list(localSymbol))
        contract <- localSymbol
    else
        contract <- rib::IBContract(localSymbol = localSymbol,
                                    secType  = secType,
                                    exchange = exchange,
                                    currency = currency)

    contract$includeExpired <- grepl("OPT|FUT|FOP", contract$secType)
    ic$reqContractDetails(1, contract = contract)
    msg3 <- capture.output(n <- ic$checkMsg(wrap))
    ans <- wrap$Data$contract[[1]]
    attr(ans, "messages") <- c(msg1, msg2, msg3)
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
