#' GET from PURE API
#' @param base_url host
#' @param path resource path (e.g. 'research-outputs')
#' @param headers headers
#' @return parsed JSON (list) or NULL
#' @keywords internal
#' @export
pure_get <- function(base_url, path, headers) {
  url <- sprintf("https://%s/ws/api/%s", base_url, path)
  resp <- httr::GET(url, httr::add_headers(.headers = headers))
  if (resp$status_code == 200) {
    httr::content(resp, as = "parsed", type = "application/json")
  } else {
    NULL
  }
}


#' PUT to PURE API
#' @param base_url host
#' @param path resource path (e.g. 'research-outputs' or 'research-outputs/<uuid>')
#' @param headers headers
#' @param payload list to json
#' @return `httr` response object
#' @export
pure_put <- function(base_url, path, headers, payload) {
  url <- sprintf("https://%s/ws/api/%s", base_url, path)
  httr::PUT(url,
            body = jsonlite::toJSON(payload, auto_unbox = TRUE),
            encode = "json",
            httr::add_headers(.headers = headers))
}



#' Bulk download from PURE API
#' @param base_url host
#' @param path resource path (e.g. 'research-outputs')
#' @param headers headers
#' @param size size of payload to download at a time (set to 1000)
#' @param offset offset for payload (set to 0)
#' @return output bulk download
#' @export
bulk_download_from_api <- function(base_url, path, headers, size = 1000, offset = 0) {
  output <- list()
  start_time <- Sys.time()
  
  repeat {
    url <- sprintf("https://%s/ws/api/%s?size=%d&offset=%d", base_url, path, size, offset)
    
    response <- httr::GET(url, httr::add_headers(.headers = headers))
    
    if (httr::status_code(response) != 200) {
      cat("Error:", status_code(response), "\n")
      break
    }
    
    data_iter <- httr::content(response, as = "parsed", type = "application/json")
    items <- data_iter$items
    
    if (is.null(items)) items <- list()
    
    output <- c(output, items)
    
    if (length(items) == 0 || length(items) < size) {
      end_time <- Sys.time()
      cat(sprintf("Done. Retrieved %d items. Total runtime: %s\n", length(output), end_time - start_time))
      break
    }
    
    offset <- offset + size
    cat(sprintf("Offset: %d, All data: %d\n", offset, length(output)))
  }
  
  output
}


