contracts_by_ISIN <-
function(ISIN, exchange,
         port = 7496, clientId = 1,
         verbose = FALSE,
         max.wait = 30,...) {

    if (!requireNamespace("rib")) {
        warning("package ", sQuote("rib"), " is not available")
        return(invisible(NULL))
    }
    
    mL <- max(length(ISIN), length(exchange))

    ISIN <- rep(ISIN, mL/length(ISIN))
    exchange <- rep(exchange, mL/length(exchange))

    wrap <- IButils:::.wrap$new(storeMessages = TRUE,
                                showMessages = verbose)
    ic <- IBClient$new()

    ic$connect(port = port, clientId = clientId)
    on.exit({
        ic$disconnect()
    })

    ic$checkMsg(wrap)

    for (i in seq_along(ISIN)) {
        contr <- rib::Contract
        contr$secIdType <- "ISIN"
        contr$secId <- ISIN[i]
        contr$exchange <- exchange[i]

        ic$reqContractDetails(i, contr)
    }

    time <- Sys.time()
    done <- FALSE
    while (!done) {
        count <- ic$checkMsg(wrap)
        if (count == 0L)
            done <- TRUE
        if (as.numeric(Sys.time() - time, units = "secs") > max.wait) {
            warning("timeout reached")
            break
        }
    }
    ic$disconnect()

    ans <- data.frame(localSymbol = character(length(ISIN)),
                      secType = character(length(ISIN)),
                      exchange = character(length(ISIN)),
                      currency = character(length(ISIN)))
    row.names(ans) <- paste(ISIN, exchange, sep = " / ")

    
    ## fetch received contract data
    contracts <- wrap$Data$contracts

    ## match received data to requested data
    rec <- sapply(contracts, `[[`, "secIdList")
    i <- match(rec, ISIN)


    
    C <- lapply(contracts, `[[`, 1)
    for (i1 in i)
        for (col in colnames(ans) )
             ans[[col]][i] <- sapply(C, `[[`, col)

   
    attr(ans, "contractDetails") <- vector("list", length(ISIN))
    names(attr(ans, "contractDetails")) <- "ISIN"
    attr(ans, "contractDetails")[i] <- contracts
    ans
}
