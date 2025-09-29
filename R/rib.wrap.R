.wrap <-
R6::R6Class("IBWrap",

    class = FALSE,
    cloneable = FALSE,
    lock_class = TRUE,

    inherit= rib::IBWrap,
    public= list(

        Data= NULL,
        Settings = NULL,

        initialize= function(...) {

            self$Settings <- new.env()
            self$Settings$storeMessages <- FALSE
            self$Settings$showMessages  <- TRUE
            ARGS <- list(...)
            for (setting in names(ARGS)) {
                self$Settings[[setting]] <- ARGS[[setting]]
            }


            self$Data <- new.env()
            self$Data$orders      <- list()
            self$Data$orderStatus <- list()
            self$Data$executions  <- list()
            self$Data$contracts   <- list()
            self$Data$commissionReport <- list()
            self$Data$accountSummary <- list()

            self$Data$headTimestamp  <- list()
            self$Data$historicalData <- list()

            self$Data$recentMessages <- list()

            self$Data$Close <- list()

        },


        tickPrice= function(reqId, tickType, price, size, attrib) warning("default 'tickPrice' implementation"),

        tickSize= function(reqId, tickType, size) warning("default 'tickSize' implementation"),

        tickOptionComputation= function(reqId, tickType,
                                        tickAttrib, impliedVol,
                                        delta, optPrice, pvDividend,
                                        gamma, vega, theta, undPrice)
                                   warning("default 'tickOptionComputation' implementation"),

        tickGeneric= function(reqId, tickType, value) warning("default 'tickGeneric' implementation"),

        tickString= function(reqId, tickType, value) warning("default 'tickString' implementation"),

        tickEFP= function(reqId, tickType, basisPoints,
                          formattedBasisPoints, totalDividends,
                          holdDays, futureLastTradeDate, dividendImpact,
                          dividendsToLastTradeDate)
                     warning("default 'tickEFP' implementation"),

        orderStatus = function(orderId, status, filled, remaining,
                               avgFillPrice, permId, parentId, lastFillPrice,
                               clientId, whyHeld, mktCapPrice) {
            self$Data$orderStatus[[as.character(permId)]] <-
                as.list(match.call())[-1L]
        },


        openOrder= function(orderId, contract, order, orderState) {
            self$Data$orders[[as.character(order$permId)]] <-
                list(orderId = orderId,
                     contract = contract,
                     order = order,
                     orderState = orderState)
            if (self$Settings$showMessages)
                message("order ", orderId, ": added to/updated in Data$orders")
        },

        openOrderEnd= function() {
            invisible(NULL)
        },

        #    connectionClosed= function() warning("default implementation"),

        updateAccountValue= function(key, value, currency, accountName) {
            v <- list(key = key, value = value, currency = currency, accountName = accountName)
            self$Data$accountData <-
                c(self$Data$accountData, v)
        },
        updatePortfolio= function(contract, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName) {
            v <- list(contract, position, marketPrice, marketValue,
                      averageCost, unrealizedPNL, realizedPNL, accountName)
            self$Data$portfolioData <- c(self$Data$portfolioData, list(v))

        },

        updateAccountTime= function(timestamp) {
            invisible(NULL)
        },

        accountDownloadEnd= function(accountName) {
            invisible(NULL)
        },

        nextValidId= function(orderId) {
            self$Data$nextValidId <- orderId
        },

    contractDetails= function(reqId, contractDetails) {
      self$Data$contracts[[as.character(reqId)]] <- contractDetails
    },

    bondContractDetails= function(reqId, contractDetails) warning("default 'bondContractDetails' implementation"),

    contractDetailsEnd= function(reqId) {
        invisible(NULL)
    },

    execDetails=        function(reqId, contract, execution) {
        self$Data$executions_contracts[[execution$execId]] <- contract
        self$Data$executions[[execution$execId]] <- execution
        ## cat("ExecDetails:", reqId, contract$symbol, execution$side, execution$shares, execution$price, "\n")
    },

    execDetailsEnd = function(reqId) {
        ## cat("ExecDetailsEnd:", reqId, "\n")
    },

    error= function(id, errorTime, errorCode, errorString, advancedOrderRejectJson) {
        if (self$Settings$storeMessages) {
            n <- as.character(round(unclass(Sys.time()), 6))
            self$Data$recentMessages[[n]] <- list(id,
                                                  errorTime,
                                                  errorCode,
                                                  errorString,
                                                  advancedOrderRejectJson)
        }
        if (self$Settings$showMessages &&
            errorCode %in% c(2104, 2158, 2106)) {
            cat(format(errorCode, width = 6),
                ": ", errorString, "\n")
        } else if (self$Settings$showMessages)
            cat(format(id, width = 6),
                "|", format(errorCode, width = 6), ": ", errorString, "\n")
    },

    updateMktDepth= function(reqId, position, operation, side, price, size) warning("default 'updateMktDepth' implementation"),

    updateMktDepthL2= function(reqId, position, marketMaker, operation, side, price, size, isSmartDepth) warning("default 'updateMktDepthL2' implementation"),

    updateNewsBulletin= function(msgId, msgType, newsMessage, originExch) warning("default 'updateNewsBulletin' implementation"),

    managedAccounts= function(accountsList) {
      if (self$Settings$showMessages) {
          cat("Accounts: \n")
          acc <- paste(paste("  ",
                             sort(unlist(strsplit(accountsList, ",")))),
                       collapse = "\n")
          cat(acc, "\n\n")
      } else {
          self$Data$managedAccounts <- sort(unlist(strsplit(accountsList, ",")))
      }
      invisible(NULL)
    },

    receiveFA= function(faDataType, xml) warning("default 'receiveFA' implementation"),

    historicalData= function(reqId, bars) {
        self$Data$historicalData[[as.character(reqId)]] <- bars
        if (self$Settings$showMessages)
            message("request id: ", reqId, "; received rows: ",
                    nrow(self$Data$historicalData[[as.character(reqId)]]))
    },

    scannerParameters= function(xml) warning("default 'scannerParameters' implementation"),

    scannerData= function(reqId, data) warning("default 'scannerData' implementation"),

    realtimeBar= function(reqId, time, open, high, low, close,
                          volume, wap, count) {
        message(reqId, " | ", .POSIXct(time), " | ", open, " | ", close)
        self$Data$Close[reqId] <- close
    },

    currentTime= function(time) warning("default 'currentTime' implementation"),

    fundamentalData= function(reqId, data) warning("default 'fundamentalData' implementation"),

    deltaNeutralValidation= function(reqId, deltaNeutralContract) warning("default 'deltaNeutralValidation' implementation"),

    tickSnapshotEnd= function(reqId) warning("default 'tickSnapshotEnd' implementation"),

    marketDataType= function(reqId, marketDataType) warning("default 'marketDataType' implementation"),

    commissionReport= function(commissionReport) {
      self$Data$commissionReport[[commissionReport$execId]] <-
          commissionReport
    },

    position= function(account, contract, position, avgCost) {
      L <- list(account = account, contract = contract,
                position = position, avgCost = avgCost)
      self$Data$positions <- c(self$Data$positions, list(L))

    },

    positionEnd= function() {
        invisible(NULL)
    },

    accountSummary= function(reqId, account, tag, value, currency) {
        L <- list(account = account, tag = tag,
                  value = value, currency = currency)
        self$Data$accountSummary <- c(self$Data$accountSummary, list(L))
    },

    accountSummaryEnd= function(reqId) {
        invisible(NULL)
    },

    verifyMessageAPI= function(apiData) warning("default 'verifyMessageAPI' implementation"),

    verifyCompleted= function(isSuccessful, errorText) warning("default 'verifyCompleted' implementation"),

    displayGroupList= function(reqId, groups) warning("default 'displayGroupList' implementation"),

    displayGroupUpdated= function(reqId, contractInfo) warning("default 'displayGroupUpdated' implementation"),

    verifyAndAuthMessageAPI= function(apiData, xyzChallange) warning("default 'verifyAndAuthMessageAPI' implementation"),

    verifyAndAuthCompleted= function(isSuccessful, errorText) warning("default 'verifyAndAuthCompleted' implementation"),

#    connectAck= function() warning("default implementation"),

    positionMulti= function(reqId, account, modelCode, contract, position, avgCost) warning("default 'positionMulti' implementation"),

    positionMultiEnd= function(reqId) warning("default 'positionMultiEnd' implementation"),

    accountUpdateMulti= function(reqId, account, modelCode, key, value, currency) warning("default 'accountUpdateMulti' implementation"),

    accountUpdateMultiEnd= function(reqId) warning("default 'accountUpdateMultiEnd' implementation"),

    securityDefinitionOptionalParameter= function(reqId, exchange, underlyingConId, tradingClass, multiplier, expirations, strikes) warning("default 'securityDefinitionOptionalParameter' implementation"),

    securityDefinitionOptionalParameterEnd= function(reqId) warning("default 'securityDefinitionOptionalParameterEnd' implementation"),

    softDollarTiers= function(reqId, tiers) warning("default 'softDollarTiers' implementation"),

    familyCodes= function(familyCodes) warning("default 'familyCodes' implementation"),

    symbolSamples= function(reqId, contractDescriptions) warning("default 'symbolSamples' implementation"),

    mktDepthExchanges= function(depthMktDataDescriptions) warning("default 'mktDepthExchanges' implementation"),

    tickNews= function(tickerId, timestamp, providerCode, articleId, headline, extraData) warning("default 'tickNews' implementation"),

    smartComponents= function(reqId, map) warning("default 'smartComponents' implementation"),

    tickReqParams= function(reqId, minTick, bboExchange, snapshotPermissions) warning("default 'tickReqParams' implementation"),

    newsProviders= function(newsProviders) warning("default 'newsProviders' implementation"),

    newsArticle= function(reqId, articleType, articleText) warning("default 'newsArticle' implementation"),

    historicalNews= function(reqId, time, providerCode, articleId, headline) warning("default 'historicalNews' implementation"),

    historicalNewsEnd= function(reqId, hasMore) warning("default 'historicalNewsEnd' implementation"),

    headTimestamp= function(reqId, headTimestamp) {
        self$Data$headTimestamp[[as.character(reqId)]] <- headTimestamp

        ## message("headTimestamp ", headTimestamp)
        ## warning("default 'headTimestamp' implementation")
    },

    histogramData= function(reqId, data) warning("default 'histogramData' implementation"),

    historicalDataUpdate= function(reqId, bar) warning("default 'historicalDataUpdate' implementation"),

    rerouteMktDataReq= function(reqId, conId, exchange) warning("default 'rerouteMktDataReq' implementation"),

    rerouteMktDepthReq= function(reqId, conId, exchange) warning("default 'rerouteMktDepthReq' implementation"),

    marketRule= function(marketRuleId, priceIncrements) warning("default 'marketRule' implementation"),

    pnl= function(reqId, dailyPnL, unrealizedPnL, realizedPnL) warning("default 'pnl' implementation"),

    pnlSingle= function(reqId, position, dailyPnL, unrealizedPnL, realizedPnL, value) warning("default 'pnlSingle' implementation"),

    historicalTicks= function(reqId, ticks, done) warning("default 'historicalTicks' implementation"),

    historicalTicksBidAsk= function(reqId, ticks, done) warning("default 'historicalTicksBidAsk' implementation"),

    historicalTicksLast= function(reqId, ticks, done) warning("default 'historicalTicksLast' implementation"),

    tickByTickAllLast= function(reqId, tickType, time, price, size, attribs, exchange, specialConditions) warning("default 'tickByTickAllLast' implementation"),

    tickByTickBidAsk= function(reqId, time, bidPrice, askPrice, bidSize, askSize, attribs) warning("default 'tickByTickBidAsk' implementation"),

    tickByTickMidPoint= function(reqId, time, price) warning("default 'tickByTickMidPoint' implementation"),

    orderBound= function(permId, clientId, orderId) warning("default 'orderBound' implementation"),

    completedOrder= function(contract, order, orderState) warning("default 'completedOrder' implementation"),

    completedOrdersEnd= function() warning("default 'completedOrdersEnd' implementation"),

    replaceFAEnd= function(reqId, data) warning("default 'replaceFAEnd' implementation"),

    wshMetaData= function(reqId, data) warning("default 'wshMetaData' implementation"),

    wshEventData= function(reqId, data) warning("default 'wshEventData' implementation"),

    historicalSchedule= function(reqId, startDateTime, endDateTime, timeZone, sessions) warning("default 'historicalSchedule' implementation"),

    userInfo= function(reqId, whiteBrandingId) warning("default 'userInfo' implementation"),

    historicalDataEnd= function(reqId, startDate, endDate) warning("default 'historicalDataEnd' implementation"),

    currentTimeInMillis= function(timeInMillis) warning("default 'currentTimeInMillis' implementation")

    )
)
