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



read_flex_report <-
function(file,
         date.format = "yyyy-MM-dd",
         time.format = "HH:mm:ss",
         date.time.separator = ",",
         ...,
         fill = FALSE) {

    txt <- readLines(file, warn = FALSE)
    bof <- grep("^.?BOF", txt)
    eof <- grep("^.?EOF", txt)
    if (length(bof) && 1 != bof)
        warning("bof does not point to first line")
    if (length(eof) && length(txt) != eof)
        warning("eof does not match actual file length")


    ## BOA = Beginning Of Account
    accounts <- grep("^.?BOA", txt)
    if (length(accounts)) {
        accounts <- cbind(read.table(text = unique(txt[accounts]),
                                     sep = ",", quote= "\""),
                          start = accounts)
        ends <- grep("^.?EOA", txt)
        accounts <- cbind(accounts, end = ends)
    }

    ans <- vector("list", nrow(accounts))
    names(ans) <- as.character(accounts[[2L]])
    for (a in seq_len(nrow(accounts))) {
        A <- txt[seq(accounts$start[[a]] + 1,
                     accounts$end  [[a]] - 1)]

        sections <- grep("^.?BOS", A)
        if (length(sections)) {
            ends <- grep("^.?EOS", A)
            N <- as.character(accounts[[2L]][a])
            ans[[N]] <- list()

            for (s in seq_along(sections)) {
                sec.title <- read.table(
                    text = A[sections[s]],
                    sep = ",",
                    quote= "\"")[[3L]]
                sec.content <- read.table(
                    text = A[seq(sections[s] + 1, ends[s] - 1)],
                    sep = ",",
                    quote= "\"",
                    header = TRUE)
                ans[[N]][[sec.title]] <-
                    sec.content
            }
        }
    }

    secs <- unique(unlist(lapply(ans, names)))
    ans1 <- list()
    for (sec in secs) {
        for (a in seq_along(ans)) {
            if (a == 1L)
                S <- ans[[1L]][[sec]]
            else
                S <- merge(S, ans[[a]][[sec]], all = TRUE)
        }
        ans1[[sec]] <- S
    }
    ans1
}
