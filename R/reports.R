#' Write standard reports
#' @param out_dir output directory
#' @param put_existing_df data.frame of responses
#' @param put_new_df data.frame of responses
#' @param failed_df optional failures summary
#' @export
write_reports <- function(out_dir, put_existing_df = NULL, put_new_df = NULL, failed_df = NULL) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M")
  if (!is.null(put_existing_df)) readr::write_csv(put_existing_df, file.path(out_dir, paste0("PUT_existing_ALL_", stamp, ".csv")))
  if (!is.null(put_new_df)) readr::write_csv(put_new_df, file.path(out_dir, paste0("PUT_new_ALL_", stamp, ".csv")))
  if (!is.null(failed_df)) readr::write_csv(failed_df, file.path(out_dir, paste0("ALL_FAILED_", stamp, ".csv")))
  invisible(TRUE)
}
