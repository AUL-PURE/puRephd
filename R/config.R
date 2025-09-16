#' Load PURE API configuration
#'
#' Reads API keys and base URLs from environment variables or function arguments.
#'
#' @param staging logical. If TRUE, use staging credentials; otherwise production.
#' @param api_key Optional API key to override environment variables.
#' @param base_url Optional base URL (e.g., 'pure.au.dk' or 'au-staging.elsevierpure.com').
#' @param awarding_org_uuid UUID of awarding institution; default may be overridden via env
#'   `PURE_AWARDING_ORG_UUID`.
#'
#' @return A list with `headers`, `base_url`, and `awarding_org_uuid`.
#' @export
load_config <- function(staging = FALSE,
                        api_key = NULL,
                        base_url = NULL,
                        awarding_org_uuid = Sys.getenv("PURE_AWARDING_ORG_UUID", 
                          unset = "fa9d17d6-3d7c-43bf-93f7-a18c40cf0778")) {
  if (isTRUE(staging)) {
    key <- api_key %||% Sys.getenv("PURE_API_KEY_STAGING")
    url <- base_url %||% Sys.getenv("PURE_BASE_URL_STAGING", unset = "au-staging.elsevierpure.com")
  } else {
    key <- api_key %||% Sys.getenv("PURE_API_KEY_PROD")
    url <- base_url %||% Sys.getenv("PURE_BASE_URL_PROD", unset = "pure.au.dk")
  }
  if (identical(key, "")) stop("API key not set. Use env vars or pass api_key.")
  headers <- c(`api-key` = key, `Content-Type` = "application/json")
  list(headers = headers, base_url = url, awarding_org_uuid = awarding_org_uuid)
}

#' Set PURE credentials into env vars (helper)
#' @param prod_key production API-key
#' @param staging_key staging API-key
#' @export
use_pure_credentials <- function(prod_key = NULL, 
                                 staging_key = NULL) {
  if (!is.null(prod_key)) Sys.setenv(PURE_API_KEY_PROD = prod_key)
  if (!is.null(staging_key)) Sys.setenv(PURE_API_KEY_STAGING = staging_key)
  invisible(TRUE)
}

`%||%` <- function(x, y) if (is.null(x) || (is.character(x) && identical(x, ""))) y else x
