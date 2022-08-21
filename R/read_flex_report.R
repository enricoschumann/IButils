read_flex_report <-
function(file,
         date.format = "yyyy-MM-dd",
         time.format = "HH:mm:ss",
         date.time.separator = ",",
         ...) {

    txt <- readLines(file, warn = FALSE)
    bof <- grep("^.?BOF", txt)
    eof <- grep("^.?EOF", txt)
    if (length(txt) != eof)
        warning("eof does not match actual file length")

    sections <- grep("^.?BOS", txt)
    sections <- read.table(text = unique(txt[sections]),
                           sep = ",", quote= "\"")
    ans <- vector("list", nrow(sections))
    names(ans) <- sections[[3]]
    for (i in seq_along(ans)) {
        h <- grep(paste0("HEADER.,.", sections[i, 2]), txt)
        header <- unlist(read.table(text = txt[h[1L]],
                                    sep = ",", quote = "\""))
        ii <- grep(paste0("DATA.,.", sections[i, 2]), txt)
        if (length(ii)) {
            tb <- read.table(text = txt[ii], sep = ",", quote= "\"")
            colnames(tb) <- header
            ans[[i]] <- tb
        }
    }
    ans
}
