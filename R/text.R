#' Escape XHTML special characters
#'
#' Minimal helper that converts &, <, >, ' and " to XML-escaped equivalents.
#' @param x character vector
#' @return character
#' @export
escape_xhtml <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}

#' Split title/subtitle on colon
#' @param title string
#' @return list(title=..., subtitle=...)
#' @export
split_title_subtitle <- function(title) {
  title <- escape_xhtml(title)
  if (length(grep(":", title)) > 0) {
    parts <- strsplit(title, ": ")[[1]]
    list(title = parts[1], subtitle = ifelse(length(parts) > 1, parts[2], ""))
  } else list(title = title, subtitle = "")
}
