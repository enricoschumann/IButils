wrap0 <- R6::R6Class("IBWrap",

  class=      FALSE,
  cloneable=  FALSE,
  lock_class= TRUE,

  public= list(

      Data= NULL,

      initialize= function() self$Data <- new.env(),

    # Callbacks
    tickPrice= function(tickerId, field, price, size, attrib) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    tickSize= function(tickerId, field, size) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    tickOptionComputation= function(tickerId, tickType, tickAttrib, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    tickGeneric= function(tickerId, tickType, value) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    tickString= function(tickerId, tickType, value) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    tickEFP= function(tickerId, tickType, basisPoints, formattedBasisPoints, totalDividends, holdDays, futureLastTradeDate, dividendImpact, dividendsToLastTradeDate) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    orderStatus= function(orderId, status, filled, remaining, avgFillPrice, permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    openOrder= function(orderId, contract, order, orderstate) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    openOrderEnd= function() warning("default method for ", get("res", envir = parent.frame(1))$fname),

#    connectionClosed= function() warning("default method for ", get("res", envir = parent.frame(1))$fname),

    updateAccountValue= function(key, val, currency, accountName) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    updatePortfolio= function(contract, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    updateAccountTime= function(timeStamp) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    accountDownloadEnd= function(accountName) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    nextValidId= function(orderId) {
      self$Data$nextId <- orderId
      cat("Next valid Order Id is ", orderId, "\n", sep = "")
      invisible(NULL)
    },

    contractDetails= function(reqId, contractDetails) {
      self$Data$contract <- contractDetails
      invisible(NULL)
      ## warning("default method for ", get("res", envir = parent.frame(1))$fname)
  },

    bondContractDetails= function(reqId, contractDetails) warning("default method for ", get("res", envir = parent.frame(1))$fname),

  contractDetailsEnd= function(reqId) {
      ## browser()
      ## warning("default method for ", get("res", envir = parent.frame(1))$fname)
},

    execDetails= function(reqId, contract, execution) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    execDetailsEnd= function(reqId) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    error= function(id, errorCode, errorString, advancedOrderRejectJson) {
      cat(format(errorCode, width = 6), "|  ", "\n")
      ## warning("default method for ", get("res", envir = parent.frame(1))$fname)
  },

    updateMktDepth= function(id, position, operation, side, price, size) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    updateMktDepthL2= function(id, position, marketMaker, operation, side, price, size, isSmartDepth) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    updateNewsBulletin= function(msgId, msgType, newsMessage, originExch) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    managedAccounts= function(accountsList) {
      cat("Access to the following accounts: \n")
      acc <- paste(paste("  ", sort(unlist(strsplit(accountsList, ",")))),
                   collapse = "\n")
      cat(acc, "\n\n")
      invisible(NULL)
  },

    receiveFA= function(faDataType, xml) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    historicalData= function(reqId, bar) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    scannerParameters= function(xml) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    scannerData= function(reqId, rank, contractDetails, distance, benchmark, projection, legsStr) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    realtimeBar= function(reqId, time, open, high, low, close, volume, wap, count) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    currentTime= function(time) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    fundamentalData= function(reqId, data) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    deltaNeutralValidation= function(reqId, deltaNeutralContract) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    tickSnapshotEnd= function(reqId) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    marketDataType= function(reqId, marketDataType) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    commissionReport= function(commissionReport) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    position= function(account, contract, position, avgCost) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    positionEnd= function() warning("default method for ", get("res", envir = parent.frame(1))$fname),

    accountSummary= function(reqId, account, tag, value, currency) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    accountSummaryEnd= function(reqId) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    verifyMessageAPI= function(apiData) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    verifyCompleted= function(isSuccessful, errorText) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    displayGroupList= function(reqId, groups) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    displayGroupUpdated= function(reqId, contractInfo) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    verifyAndAuthMessageAPI= function(apiData, xyzChallange) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    verifyAndAuthCompleted= function(isSuccessful, errorText) warning("default method for ", get("res", envir = parent.frame(1))$fname),

#    connectAck= function() warning("default method for ", get("res", envir = parent.frame(1))$fname),

    positionMulti= function(reqId, account, modelCode, contract, position, avgCost) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    positionMultiEnd= function(reqId) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    accountUpdateMulti= function(reqId, account, modelCode, key, value, currency) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    accountUpdateMultiEnd= function(reqId) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    securityDefinitionOptionalParameter= function(reqId, exchange, underlyingConId, tradingClass, multiplier, expirations, strikes) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    securityDefinitionOptionalParameterEnd= function(reqId) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    softDollarTiers= function(reqId, tiers) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    familyCodes= function(familyCodes) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    symbolSamples= function(reqId, contractDescriptions) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    mktDepthExchanges= function(depthMktDataDescriptions) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    tickNews= function(tickerId, timeStamp, providerCode, articleId, headline, extraData) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    smartComponents= function(reqId, theMap) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    tickReqParams= function(tickerId, minTick, bboExchange, snapshotPermissions) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    newsProviders= function(newsProviders) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    newsArticle= function(requestId, articleType, articleText) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    historicalNews= function(requestId, time, providerCode, articleId, headline) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    historicalNewsEnd= function(requestId, hasMore) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    headTimestamp= function(reqId, headTimestamp) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    histogramData= function(reqId, data) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    historicalDataUpdate= function(reqId, bar) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    rerouteMktDataReq= function(reqId, conid, exchange) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    rerouteMktDepthReq= function(reqId, conid, exchange) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    marketRule= function(marketRuleId, priceIncrements) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    pnl= function(reqId, dailyPnL, unrealizedPnL, realizedPnL) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    pnlSingle= function(reqId, pos, dailyPnL, unrealizedPnL, realizedPnL, value) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    historicalTicks= function(reqId, ticks, done) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    historicalTicksBidAsk= function(reqId, ticks, done) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    historicalTicksLast= function(reqId, ticks, done) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    tickByTickAllLast= function(reqId, tickType, time, price, size, attribs, exchange, specialConditions) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    tickByTickBidAsk= function(reqId, time, bidPrice, askPrice, bidSize, askSize, attribs) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    tickByTickMidPoint= function(reqId, time, midPoint) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    orderBound= function(orderId, apiClientId, apiOrderId) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    completedOrder= function(contract, order, orderState) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    completedOrdersEnd= function() warning("default method for ", get("res", envir = parent.frame(1))$fname),

    replaceFAEnd= function(reqId, text) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    wshMetaData= function(reqId, dataJson) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    wshEventData= function(reqId, dataJson) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    historicalSchedule= function(reqId, startDateTime, endDateTime, timeZone, sessions) warning("default method for ", get("res", envir = parent.frame(1))$fname),

    userInfo= function(reqId, whiteBrandingId) warning("default method for ", get("res", envir = parent.frame(1))$fname)
  )
)

IBWrapHistData <- R6::R6Class("IBWrapHistData",

  class=      FALSE,
  cloneable=  FALSE,
  lock_class= TRUE,

  inherit= rib::IBWrap,

  public= list(

    # Environment holding results
    context= NULL,
    Data= NULL,

    initialize= function() self$context <- new.env(),

    # Override methods
    tickPrice=          function(tickerId, field, price, size, attrib)
                          cat("Price:", tickerId, field, price, size, unlist(attrib), "\n"),

    tickSize=           function(tickerId, field, size)
                          cat("Size:", tickerId, field, size, "\n"),

    tickOptionComputation= function(tickerId, tickType, tickAttrib, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice)
                             self$context$option <- list(tickerId, tickType, tickAttrib, impliedVol, delta, optPrice, pvDividend, gamma, vega, theta, undPrice),

    tickGeneric=        function(tickerId, tickType, value)
                          cat("Generic:", tickerId, tickType, value, "\n"),

    tickString=         function(tickerId, tickType, value)
                          cat("String:", tickerId, tickType, value, "\n"),

    orderStatus=        function(orderId, status, filled, remaining, avgFillPrice, permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice)
                          cat("OrderStatus:", orderId, status, filled, remaining, avgFillPrice, permId, parentId, lastFillPrice, clientId, whyHeld, mktCapPrice, "\n"),

    openOrder=          function(orderId, contract, order, orderstate) {
                          self$context$order <- list(id=orderId, contract=contract, order=order, orderstate=orderstate)
                          cat("OpenOrder:", orderId, "\n")
                        },

    openOrderEnd=       function()
                          cat("OpenOrderEnd.\n"),

    updateAccountValue= function(key, val, currency, accountName)
                          cat("AccountValue:", key, val, currency, accountName, "\n"),

    updatePortfolio=    function(contract, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName)
                          cat("updatePortfolio:", contract$symbol, position, marketPrice, marketValue, averageCost, unrealizedPNL, realizedPNL, accountName, "\n"),

    updateAccountTime=  function(timeStamp)
                          cat("AccountTime:", timeStamp, "\n"),

    accountDownloadEnd= function(accountName)
                          cat("AccountDownloadEnd:", accountName, "\n"),

    nextValidId=        function(orderId)
                          self$context$nextId <- orderId,

    contractDetails=    function(reqId, contractDetails)
                          self$Data$contract <- contractDetails,

    bondContractDetails=function(reqId, contractDetails)
                          self$context$bond <- contractDetails,

    contractDetailsEnd= function(reqId)
                          cat("Done ContractDetailsEnd:", reqId, "\n"),

    execDetails=        function(reqId, contract, execution) {
                          self$context$ex_contract <- contract
                          self$context$execution <- execution
                          cat("ExecDetails:", reqId, contract$symbol, execution$side, execution$shares, execution$price, "\n")
                        },

    execDetailsEnd=     function(reqId)
                          cat("ExecDetailsEnd:", reqId, "\n"),

    error=              function(id, errorCode, errorString, advancedOrderRejectJson) {
      switch(as.character(errorCode),
             "200" = stop("No security definition has been found for the request"),
             "2104" = NULL,  ## Market data OK
             "2106" = NULL,  ## HMDS data OK
             "2158" = NULL,  ## Sec-def data OK
             cat("Error:", id, errorCode, errorString, "\n")
             )
  },
    updateMktDepth=     function(id, position, operation, side, price, size)
                          cat("MktDepth:", id, position, operation, side, price, size, "\n"),

    updateMktDepthL2=   function(id, position, marketMaker, operation, side, price, size, isSmartDepth)
                          cat("MktDepthL2:", id, position, marketMaker, operation, side, price, size, isSmartDepth, "\n"),

    updateNewsBulletin= function(msgId, msgType, newsMessage, originExch) {
                          self$context$news <- newsMessage
                          cat("NewsBulletin:", msgId, msgType, originExch, "\n",
                                               newsMessage, "\n")
                        },

    managedAccounts=    function(accountsList)
                          self$context$accounts <- accountsList,

    receiveFA=          function(faDataType, xml) {
                          self$context$fa <- xml
                          cat("ReceiveFA:", faDataType, "\n")
                        },

    historicalData=     function(reqId, bar) {
                          self$context$historical <- bar
                          cat("received ", nrow(self$context$historical), "rows\n")
                          ## cat("Historical:", reqId, "Rows:", nrow(self$context$historical), "\n")
                        },

    scannerParameters=  function(xml) {
                          self$context$scannerParameters <- xml
                          cat("ScannerParameters: Received\n")
                        },

    scannerData=        function(reqId, rank, contractDetails, distance, benchmark, projection, legsStr) {
                          cat("Scanner:", reqId, length(rank), "\n")
                          for(i in seq_along(rank))
                            cat(" |", rank[i],
                                      paste0(contractDetails[[i]]$contract[c(1:3, 8)], collapse=" "),
                                      distance[i],
                                      benchmark[i],
                                      projection[i],
                                      legsStr[i], "|\n")
                        },

    realtimeBar=        function(reqId, time, open, high, low, close, volume, wap, count)
                          cat("realtimeBar:", reqId, time, open, high, low, close, volume, wap, count, "\n"),

    currentTime=        function(time)
                          cat("currentTime:", time, "\n"),

    fundamentalData=    function(reqId, data) {
                          self$context$fundamentalData <- data
                          cat("fundamentalData:", reqId, "\n")
                        },

    tickSnapshotEnd=    function(reqId)
                          cat("tickSnapshotEnd:", reqId, "\n"),

    marketDataType=     function(reqId, marketDataType)
                          cat("marketDataType:", reqId, map_int2enum("MarketData", marketDataType), "\n"),

    commissionReport=   function(commissionReport)
                          cat("CommissionReport:", unlist(commissionReport), "\n"),

    position=           function(account, contract, position, avgCost)
                          cat("Position:", account, contract$symbol, position, avgCost, "\n"),

    positionEnd=        function()
                          cat("PositionEnd.\n"),

    accountSummary=     function(reqId, account, tag, value, currency)
                          cat("Account:", reqId, account, tag, value, currency, "\n"),

    accountSummaryEnd=  function(reqId)
                          cat("AccountEnd:", reqId, "\n"),

    displayGroupList=   function(reqId, groups)
                          cat("DisplayGroupList:", reqId, groups, "\n"),

    displayGroupUpdated= function(reqId, contractInfo)
                          cat("DisplayGroupUpdated:", reqId, contractInfo, "\n"),

    positionMulti=      function(reqId, account, modelCode, contract, position, avgCost)
                          cat("PositionMulti:", reqId, account, modelCode, contract$symbol, position, avgCost, "\n"),

    positionMultiEnd=   function(reqId)
                          cat("PositionMultiEnd:", reqId, "\n"),

    accountUpdateMulti= function(reqId, account, modelCode, key, value, currency)
                          cat("AccountUpdateMulti:", reqId, account, modelCode, key, value, currency, "\n"),

    accountUpdateMultiEnd= function(reqId)
                             cat("AccountUpdateMultiEnd:", reqId, "\n"),

    securityDefinitionOptionalParameter= function(reqId, exchange, underlyingConId, tradingClass, multiplier, expirations, strikes)
                                           cat("SecDefOptParams:", reqId, exchange, underlyingConId, tradingClass, multiplier, length(expirations), length(strikes), "\n"),

    securityDefinitionOptionalParameterEnd= function(reqId)
                                              cat("SecDefOptParamsEnd:", reqId, "\n"),

    softDollarTiers=    function(reqId, tiers) {
                          cat("softDollarTiers:", reqId, nrow(tiers), "\n")
                          print(tiers)
                        },

    familyCodes=        function(familyCodes) {
                          cat("familyCodes:", nrow(familyCodes), "\n")
                          print(familyCodes)
                        },

    symbolSamples=      function(reqId, contractDescriptions) {
                          cat("symbolSamples:", reqId, length(contractDescriptions), "\n")
                          for(cd in contractDescriptions)
                            cat("  ", cd$contract$conId, cd$contract$symbol, cd$contract$primaryExchange, cd$contract$currency, cd$derivativeSecTypes, "\n")
                        },

    mktDepthExchanges=  function(depthMktDataDescriptions) {
                          self$context$mktDepthExchanges <- depthMktDataDescriptions
                          cat("mktDepthExchanges:", nrow(depthMktDataDescriptions), "\n")
                        },

    tickNews=           function(tickerId, timeStamp, providerCode, articleId, headline, extraData)
                          cat("tickNews:", tickerId, timeStamp, providerCode, articleId, headline, extraData, "\n"),

    tickReqParams=      function(tickerId, minTick, bboExchange, snapshotPermissions)
                          cat("ReqParams:", tickerId, minTick, bboExchange, snapshotPermissions, "\n"),

    newsProviders=      function(newsProviders) {
                          cat("newsProviders:", nrow(newsProviders), "\n")
                          print(newsProviders)
                        },

    newsArticle=        function(requestId, articleType, articleText) {
                          self$context$newsarticle <- articleText
                          cat("newsArticle:", requestId, articleType, "\n")
                        },

    historicalNews=     function(requestId, time, providerCode, articleId, headline)
                          cat("historicalNews:", requestId, time, providerCode, articleId, headline, "\n"),

    historicalNewsEnd=  function(requestId, hasMore)
                          cat("historicalNewsEnd:", requestId, hasMore, "\n"),

    headTimestamp=      function(reqId, headTimestamp) {
      ## cat("headTimestamp:", reqId, headTimestamp, "\n")
      self$context$headTimestamp <- headTimestamp
},

    histogramData=      function(reqId, data) {
                          self$context$histogram <- data
                          cat("histogramData:", reqId, nrow(data), "\n")
                        },

    historicalDataUpdate= function(reqId, bar)
                            cat("historicalDataUpdate:", reqId, unlist(bar), "\n"),

    marketRule=         function(marketRuleId, priceIncrements) {
                          cat("marketRule:", marketRuleId, nrow(priceIncrements), "\n")
                          print(priceIncrements)
                        },

    pnl=                function(reqId, dailyPnL, unrealizedPnL, realizedPnL)
                          cat("pnl:", reqId, dailyPnL, unrealizedPnL, realizedPnL, "\n"),

    pnlSingle=          function(reqId, pos, dailyPnL, unrealizedPnL, realizedPnL, value)
                          cat("pnlSingle:", reqId, pos, dailyPnL, unrealizedPnL, realizedPnL, value, "\n"),

    historicalTicks=    function(reqId, ticks, done) {
                          self$context$historicalTicks <- ticks
                          cat("historicalTicks:", reqId, done, "\n")
                        },

    historicalTicksBidAsk= function(reqId, ticks, done) {
                             self$context$historicalTicksBidAsk <- ticks
                             cat("historicalTicksBidAsk:", reqId, done, "\n")
                           },

    historicalTicksLast= function(reqId, ticks, done) {
                           self$context$historicalTicksLast <- ticks
                           cat("historicalTicksLast:", reqId, done, "\n")
                         },

    tickByTickAllLast=  function(reqId, tickType, time, price, size, attribs, exchange, specialConditions)
                          cat("tickByTickAllLast:", reqId, tickType, time, price, size, unlist(attribs), exchange, specialConditions, "\n"),

    tickByTickBidAsk=   function(reqId, time, bidPrice, askPrice, bidSize, askSize, attribs)
                          cat("tickByTickBidAsk:", reqId, time, bidPrice, askPrice, bidSize, askSize, unlist(attribs), "\n"),

    tickByTickMidPoint= function(reqId, time, midPoint)
                          cat("tickByTickMidPoint:", reqId, time, midPoint,
"\n"),

    orderBound=         function(orderId, apiClientId, apiOrderId)
                          cat("orderBound:", orderId, apiClientId, apiOrderId, "\n"),

    completedOrder=     function(contract, order, orderState) {
                          self$context$completed <- list(contract=contract, order=order, orderstate=orderState)
                          cat("completedOrder.\n")
                        },

    completedOrdersEnd= function()
                          cat("completedOrdersEnd.\n"),

    replaceFAEnd=       function(reqId, text)
                          cat("replaceFAEnd:", reqId, text, "\n")
  )
)
