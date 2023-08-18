.wrap <-
R6::R6Class("IBWrap",

    class=      FALSE,
    cloneable=  FALSE,
    lock_class= TRUE,

    inherit= rib::IBWrap,
    public= list(

        Data= NULL,

        initialize= function() {
            self$Data <- new.env()
            self$Data$orders <- list()
            self$Data$orderStatus <- list()
            self$Data$executions <- list()
            self$Data$contracts <- list()
            self$Data$commissionReport <- list()
            self$Data$accountSummary <- list()
        },


        tickPrice= function(tickerId, field, price, size, attrib) warning("default 'tickPrice' implementation"),

        tickSize= function(tickerId, field, size) warning("default 'tickSize' implementation"),

        tickOptionComputation= function(tickerId, tickType, tickAttrib, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice) warning("default 'tickOptionComputation' implementation"),

        tickGeneric= function(tickerId, tickType, value) warning("default 'tickGeneric' implementation"),

        tickString= function(tickerId, tickType, value) warning("default 'tickString' implementation"),

        tickEFP= function(tickerId, tickType, basisPoints, formattedBasisPoints, totalDividends, holdDays, futureLastTradeDate, dividendImpact, dividendsToLastTradeDate) warning("default 'tickEFP' implementation"),

        orderStatus = function(orderId, status, filled, remaining,
                               avgFillPrice, permId, parentId, lastFillPrice,
                               clientId, whyHeld, mktCapPrice) {
            self$Data$orderStatus[[as.character(permId)]] <-
                as.list(match.call())[-1L]
        },


        openOrder= function(orderId, contract, order, orderstate) {
            self$Data$orders[[as.character(order$permId)]] <-
                list(orderId = orderId,
                     contract = contract,
                     order = order,
                     orderstate = orderstate)
            message("order ", orderId, ": added to/updated in Data$orders")
        },

        openOrderEnd= function() {
            message("openOrderEnd")
        },

#    connectionClosed= function() warning("default implementation"),

        updateAccountValue= function(key, val, currency, accountName) warning("default 'updateAccountValue' implementation"),

        updatePortfolio= function(contract, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName) warning("default 'updatePortfolio' implementation"),

        updateAccountTime= function(timeStamp) warning("default 'updateAccountTime' implementation"),

        accountDownloadEnd= function(accountName) warning("default 'accountDownloadEnd' implementation"),

        nextValidId= function(orderId) {
            self$Data$nextValidId <- orderId
        },

    contractDetails= function(reqId, contractDetails) {
      message(reqId)
      message(contractDetails)
      self$Data$contracts[[as.character(contractDetails[[1]]$conId)]] <- contractDetails
    },

    bondContractDetails= function(reqId, contractDetails) warning("default 'bondContractDetails' implementation"),

    contractDetailsEnd= function(reqId) {
      message("contractDetailsEnd")
      message("reqId")
    },

    execDetails=        function(reqId, contract, execution) {
        self$Data$executions_contracts[[execution$execId]] <- contract
        self$Data$executions[[execution$execId]] <- execution
        ## cat("ExecDetails:", reqId, contract$symbol, execution$side, execution$shares, execution$price, "\n")
    },

    execDetailsEnd = function(reqId) {
        ## cat("ExecDetailsEnd:", reqId, "\n")
    },

    error= function(id, errorCode, errorString, advancedOrderRejectJson) {
      message(format(id, width = 6),
              "|", format(errorCode, width = 6), ": ", errorString)
    },

    updateMktDepth= function(id, position, operation, side, price, size) warning("default 'updateMktDepth' implementation"),

    updateMktDepthL2= function(id, position, marketMaker, operation, side, price, size, isSmartDepth) warning("default 'updateMktDepthL2' implementation"),

    updateNewsBulletin= function(msgId, msgType, newsMessage, originExch) warning("default 'updateNewsBulletin' implementation"),

    managedAccounts= function(accountsList) {
      cat("Accounts: \n")
      acc <- paste(paste("  ",
                         sort(unlist(strsplit(accountsList, ",")))),
                   collapse = "\n")
      cat(acc, "\n\n")
      invisible(NULL)
    },

    receiveFA= function(faDataType, xml) warning("default 'receiveFA' implementation"),

    historicalData= function(reqId, bar) warning("default 'historicalData' implementation"),

    scannerParameters= function(xml) warning("default 'scannerParameters' implementation"),

    scannerData= function(reqId, rank, contractDetails, distance, benchmark, projection, legsStr) warning("default 'scannerData' implementation"),

    realtimeBar= function(reqId, time, open, high, low, close, volume, wap, count) warning("default 'realtimeBar' implementation"),

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
        message("positionEnd: done")
    },

    accountSummary= function(reqId, account, tag, value, currency) {
        L <- list(account = account, tag = tag,
                  value = value, currency = currency)
        self$Data$accountSummary <- c(self$Data$accountSummary, list(L))
    },

    accountSummaryEnd= function(reqId) {
        message("accountSummaryEnd: done")
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

    tickNews= function(tickerId, timeStamp, providerCode, articleId, headline, extraData) warning("default 'tickNews' implementation"),

    smartComponents= function(reqId, theMap) warning("default 'smartComponents' implementation"),

    tickReqParams= function(tickerId, minTick, bboExchange, snapshotPermissions) warning("default 'tickReqParams' implementation"),

    newsProviders= function(newsProviders) warning("default 'newsProviders' implementation"),

    newsArticle= function(requestId, articleType, articleText) warning("default 'newsArticle' implementation"),

    historicalNews= function(requestId, time, providerCode, articleId, headline) warning("default 'historicalNews' implementation"),

    historicalNewsEnd= function(requestId, hasMore) warning("default 'historicalNewsEnd' implementation"),

    headTimestamp= function(reqId, headTimestamp) warning("default 'headTimestamp' implementation"),

    histogramData= function(reqId, data) warning("default 'histogramData' implementation"),

    historicalDataUpdate= function(reqId, bar) warning("default 'historicalDataUpdate' implementation"),

    rerouteMktDataReq= function(reqId, conid, exchange) warning("default 'rerouteMktDataReq' implementation"),

    rerouteMktDepthReq= function(reqId, conid, exchange) warning("default 'rerouteMktDepthReq' implementation"),

    marketRule= function(marketRuleId, priceIncrements) warning("default 'marketRule' implementation"),

    pnl= function(reqId, dailyPnL, unrealizedPnL, realizedPnL) warning("default 'pnl' implementation"),

    pnlSingle= function(reqId, pos, dailyPnL, unrealizedPnL, realizedPnL, value) warning("default 'pnlSingle' implementation"),

    historicalTicks= function(reqId, ticks, done) warning("default 'historicalTicks' implementation"),

    historicalTicksBidAsk= function(reqId, ticks, done) warning("default 'historicalTicksBidAsk' implementation"),

    historicalTicksLast= function(reqId, ticks, done) warning("default 'historicalTicksLast' implementation"),

    tickByTickAllLast= function(reqId, tickType, time, price, size, attribs, exchange, specialConditions) warning("default 'tickByTickAllLast' implementation"),

    tickByTickBidAsk= function(reqId, time, bidPrice, askPrice, bidSize, askSize, attribs) warning("default 'tickByTickBidAsk' implementation"),

    tickByTickMidPoint= function(reqId, time, midPoint) warning("default 'tickByTickMidPoint' implementation"),

    orderBound= function(orderId, apiClientId, apiOrderId) warning("default 'orderBound' implementation"),

    completedOrder= function(contract, order, orderState) warning("default 'completedOrder' implementation"),

    completedOrdersEnd= function() warning("default 'completedOrdersEnd' implementation"),

    replaceFAEnd= function(reqId, text) warning("default 'replaceFAEnd' implementation"),

    wshMetaData= function(reqId, dataJson) warning("default 'wshMetaData' implementation"),

    wshEventData= function(reqId, dataJson) warning("default 'wshEventData' implementation"),

    historicalSchedule= function(reqId, startDateTime, endDateTime, timeZone, sessions) warning("default 'historicalSchedule' implementation"),

    userInfo= function(reqId, whiteBrandingId) warning("default 'userInfo' implementation")
  )
)
