u <- "https://interactivebrokers.github.io/tws-api/message_codes.html"
raw <- readLines(u, encoding = "utf-8")

rows <- paste(raw, collapse = "")
rows <- regmatches(rows, gregexec("<tr>.*?</tr>", rows))

messages <- rows[[1]][grep("<td>[0-9]+", rows[[1]])]
cols <- strsplit(messages, "</td><td>")
if (!all(lengths(cols) == 3))
    stop("format has changed")

messages <- do.call(rbind, cols)
colnames(messages) <- c("Code", "TWS message", "Additional notes")



## Clean codes/text
j <- "Code"
messages[, j] <- gsub("<tr><td>", "", messages[, j])
messages[, j] <- trimws(messages[, j])

j <- "TWS message"
messages[, j] <- textutils::HTMLdecode(messages[, j])
messages[, j] <- trimws(messages[, j])

j <- "Additional notes"
messages[, j] <- gsub("</td></tr>", "", messages[, j])
messages[, j] <- gsub("<a[^>]+?>(.*?)</a>", "\\1", messages[, j])
messages[, j] <- gsub("<p[^>]+?>(.*?)</p>", "\\1", messages[, j])
messages[, j] <- textutils::HTMLdecode(messages[, j])
messages[, j] <- trimws(messages[, j])

message_codes <- as.data.frame(messages, check.names = FALSE)

save(message_codes,
     file = "~/Packages/IButils/data/message_codes.RData",
     version = 2)

library("orgutils")
filename <- "~/Packages/IButils/data/message_codes.txt"

cat("## -*- mode: org; -*-

## The data in this file are auto-generated from
##    https://interactivebrokers.github.io/tws-api/message_codes.html

", file = filename, append = FALSE, sep = "")

cat(toOrg(message_codes), file = filename, append = TRUE, sep = "\n")
