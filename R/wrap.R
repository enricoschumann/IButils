ib.wrap <-
    R6::R6Class("ib.wrap",
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
        self$context$completed_orders <- list()

        self$Data <- new.env()
        self$Data$executions <- list()
        self$Data$positions <- list()
    },


    error = function(id, errorCode, errorString) {
        cat(id, errorCode, errorString, "\n")
    },

    nextValidId = function(orderId) {
        self$Data$nextId <- orderId
        cat("Next OrderId:", orderId, "\n")
        orderId
    },

    managedAccounts = function(accountsList) {
        cat("Managed Accounts:", accountsList, "\n")
    },

    orderStatus = function(orderId, status, filled, remaining,
                           avgFillPrice, permId, parentId, lastFillPrice,
                           clientId, whyHeld, mktCapPrice) {
        cat("OrderStatus:", orderId, status, filled, remaining, avgFillPrice, permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice, "\n")
    },

    openOrder= function(orderId, contract, order, orderstate) {
        self$context$orders[[as.character(orderId)]] <- list(id=orderId, contract=contract, order=order, orderstate=orderstate)
        self$context$order <- list(id=orderId, contract=contract, order=order, orderstate=orderstate)
        ## cat("OpenOrder:", orderId, "\n")
    },

    openOrderEnd=       function() {
        cat(length(self$context$orders), " open orders found\n")
    },
    completedOrder=     function(contract, order, orderState) {
        ## self$context$completed <- list(contract=contract, order=order, orderstate=orderState)
        ## browser()
        self$context$completed_orders[[as.character(order$permId)]] <- list(contract=contract, order=order, orderstate=orderState)
        ## cat("completedOrder.\n")
    },

    completedOrdersEnd= function() {
        cat(length(self$context$completed_orders), " open orders found\n")
    },

    execDetails=        function(reqId, contract, execution) {
        self$context$ex_contract <- contract
        self$context$execution <- execution
        self$Data$executions[[execution$execId]] <- execution
        cat("ExecDetails:", reqId, contract$symbol, execution$side, execution$shares, execution$price, "\n")
    },

    execDetailsEnd = function(reqId) {
        cat("ExecDetailsEnd:", reqId, "\n")
    },

    position = function(account, contract, position, avgCost) {
        ## browser()
        L <- list(account = account, contract = contract,
                  position = position, avgCost = avgCost)
        self$Data$positions <- c(self$Data$positions, list(L))
        cat("Position:", account, contract$symbol, position, avgCost, "\n")
    },

    positionEnd=        function() {
        cat("PositionEnd.\n")
    }

    )
)
