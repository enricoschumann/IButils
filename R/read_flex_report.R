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
