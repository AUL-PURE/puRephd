#' Read grad school CSV
#'
#' @param path File-path to grad school CSV. As of now, the column names of the CSV have to follow a specific structure: The columns of the file have to have the following columns in the following order: 
# 1. "Full name": The PhD student's full name (FIRST_NAME LAST_NAME),
# 2. "AUID": The PhD student's employee-id,
# 3. "Last termination type": - , 
# 4. "Last termination date": Award date,
# 5. "Team name": Abbrev. faculty,
# 6. "Account": Name of PhD student's department,
# 7. "Dissertation title": Dissertation title (format: "TITLE: SUBTITLE"),
# 8. "Full name": The supervisor's full name (FIRST_NAME LAST_NAME),
# 9. "E-mail": The supervisor's official e-mail address,
# 10. "Role": Supervisor role ("Main supervisor", "Co-supervisor"),
# 11. "Account": Name of internal supervisor's department.
#' @return Tibble with normalized columns: full_name, AUID, termination_date, faculty, organization, dissertation_title, unique_id
#' @export
read_gradschool <- function(path) {
  stopifnot(file.exists(path))
  df <- readr::read_csv(path, show_col_types = FALSE, 
                        col_types = readr::cols(AUID = readr::col_character()))
  out <- df |>
    dplyr::transmute(
      full_name = .data[["Full name...1"]],
      AUID = .data$AUID,
      termination_date = .data[["Last termination date"]],
      faculty = .data[["Team name"]],
      organization = .data[["Account...6"]],
      dissertation_title = .data[["Dissertation title"]]
    ) |>
    dplyr::mutate(
      termination_date = as.Date(termination_date, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
      unique_id = AUID
    ) |>
    dplyr::distinct()
  out
}

#' Read existing theses export from PURE
#' @param path CSV with columns including AUID and thesis UUIDs
#' @return Tibble with columns: thesis_uuid, AUID
#' @export
read_existing_theses <- function(path) {
  stopifnot(file.exists(path))
  df <- readr::read_csv(path, show_col_types = FALSE)
  nm <- names(df)
  # Try to map columns
  auid_col <- nm[grepl("AUID|Medarbejder id", nm, ignore.case = TRUE)][1]
  uuid_col <- nm[grepl("UUID", nm, ignore.case = TRUE)][1]
  out <- df |>
    dplyr::transmute(
      thesis_uuid = .data[[uuid_col]],
      AUID = as.character(.data[[auid_col]])
    ) |>
    dplyr::distinct()
  out
}

#' Read accounts + institutes mapping
#' @param accounts_csv Path to Accounts_Oversigt_mod.csv (two columns mapping c1/c2)
#' @param institutes_csv Path to AU_institutes_mod_R.csv
#' @return Joined mapping tibble with organization_uuid, grad_school_org, bookseries_title, bookseries_uuid_staging, bookseries_uuid_production, mainResearchArea_EN
#' @export
read_mappings <- function(accounts_csv, institutes_csv) {
  acc <- readr::read_csv(accounts_csv, show_col_types = FALSE, col_names = FALSE)
  names(acc) <- c("c1", "c2")
  inst <- readr::read_csv(institutes_csv, show_col_types = FALSE)
  out <- dplyr::left_join(inst, acc |> dplyr::rename(grad_school_org = c2), by = c("grad_school_org" = "grad_school_org"))
  # The user file has UUID column for org
  out <- out |> dplyr::rename(organization_uuid = UUID)
  out
}
