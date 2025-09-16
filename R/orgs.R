#' Map orgs + book series + faculty
#'
#' @param students_matched Tibble from match_students()$matched
#' @param mapping Tibble from read_mappings()
#' @return Tibble students with organization_uuid, bookseries titles/uuids, mainResearchArea_EN
#' @export
map_organizations_bookseries <- function(students_matched, mapping) {
  out <- dplyr::left_join(students_matched, mapping |> dplyr::select(-ID),
                          by = c("organization" = "c1"))
  out
}

#' Validate that book series exists for each thesis
#' @param x Tibble after mapping
#' @return Rows where bookseries_title is missing
#' @export
validate_bookseries <- function(x) {
  x |> dplyr::filter(is.na(.data$bookseries_title))
}
