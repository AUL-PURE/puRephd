#' Simple assert helper
#' @param x logical
#' @param msg message
#' @keywords internal
assert_that <- function(x, msg) if (!isTRUE(x)) stop(msg, call. = FALSE)
