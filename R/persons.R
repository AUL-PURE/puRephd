#' Get internal persons from PURE or cache
#'
#' @param base_url PURE base URL
#' @param headers Named character with API headers
#' @param download_new_and_save logical; if FALSE, read from previously cached data via user function
#' @param size size of payload to download at a time (set to 1000)
#' @param offset offset for payload (set to 0)
#' @return Tibble with columns: AUID, uuid, phd_organization_uuid, full_name, first_name, last_name, email, email_domain (saved as "./1_data/internal_persons_df.RDS")
#' @export
get_internal_persons <- function(base_url, headers, download_new_and_save = FALSE, size = 1000, offset=0) {
  
  if (download_new_and_save == TRUE) {
    dir.create("1_data", recursive = TRUE, showWarnings = FALSE)
    
    internal_persons <- bulk_download_from_api(base_url = base_url, path = "persons", headers, size = size, offset = offset)
    saveRDS(internal_persons,
            file = "./1_data/internal_persons.RDS")
    print(paste0("Downloaded internal persons from PURE. ", length(internal_persons), " rows."))
    
    # extract relevant information from each person
    rows <- list()
    # Iterate over each person
    for (person in internal_persons) {
      name <- person$name
      first_name <- name$firstName
      last_name <- name$lastName
      full_name <- paste(first_name, last_name)
      uuid <- person$uuid
      
      # Extract AUID
      identifiers <- person$identifiers %||% list()
      AUID <- NA
      for (id_obj in identifiers) {
        if (!is.null(id_obj$idSource) && id_obj$idSource == "au_pers") {
          AUID <- id_obj$value
          break
        }
      }
      
      # this is a workflow to get the students orgs, but in the dataset are also the supervisors
      # after a certain amount of time after their phd, the students' phd assoc. is deleted in PURE
      # it would be more robust to get the students' orgs from the grad school data instead
      # Determine organization UUID
      associations <- person$staffOrganizationAssociations %||% list()
      org_uuids <- unique(purrr::map_chr(associations, ~ .x$organization$uuid %||% NA_character_))
      org_uuids <- org_uuids[!is.na(org_uuids)]
      
      if (length(org_uuids) == 1) {
        phd_organization_uuid <- org_uuids[1]
      } else {
        phd_organization_uuid <- NA
        for (assoc in associations) {
          job_title_uri <- tolower(assoc$jobTitle$uri %||% "")
          if (str_detect(job_title_uri, "phd")) {
            phd_organization_uuid <- assoc$organization$uuid
            break
          } else if (!is.null(assoc$primaryAssociation) && assoc$primaryAssociation) {
            phd_organization_uuid <- assoc$organization$uuid
            break
          }
        }
      }
      
      # Extract emails
      # since supervisors do not have their AUID in the grad school data, we need to find another identifier. the email seems most reliable.
      emails <- unique(purrr::flatten_chr(map(associations, function(assoc) {
        purrr::map_chr(assoc$emails %||% list(), ~ .x$value %||% NA_character_)
      })))
      emails <- emails[!is.na(emails)]
      
      if (length(emails) == 0) {
        rows <- append(rows, list(data.frame(
          full_name = full_name,
          first_name = first_name,
          last_name = last_name,
          AUID = AUID,
          uuid = uuid,
          email = NA,
          email_id = NA,
          email_domain = NA,
          phd_organization_uuid = phd_organization_uuid,
          stringsAsFactors = FALSE
        )))
      } else {
        for (email in emails) {
          parts <- str_split(email, "@")[[1]]
          email_id <- parts[1]
          email_domain <- parts[2]
          rows <- append(rows, list(data.frame(
            full_name = full_name,
            first_name = first_name,
            last_name = last_name,
            AUID = AUID,
            uuid = uuid,
            email = email,
            email_id = email_id,
            email_domain = email_domain,
            phd_organization_uuid = phd_organization_uuid,
            stringsAsFactors = FALSE
          )))
        }
      }
    }
    
    # Combine all rows into a single data frame
    internal_persons_df <- dplyr::bind_rows(rows)
    saveRDS(internal_persons_df,
            file = "./1_data/internal_persons_df.RDS")
    print(paste0("Saved internal persons as data.frame. ", length(internal_persons_df), " rows."))
    
    return(internal_persons_df)
    
  } else {
    return(readRDS(file = "./1_data/internal_persons_df.RDS"))
  }
}

#' Match students against internal persons. Tries to match by 1. AUID, 2. full name.
#'
#' @param students Data.frame from read_gradschool()
#' @param persons Data.frame from get_internal_persons()
#' @param out_nomatch_path Optional path to save unmatched students CSV
#' @return List with $matched and $unmatched
#' @export
match_students <- function(students, persons, out_nomatch_path = NULL) {
  persons_students <- persons |>
    dplyr::filter(!is.na(.data$AUID)) |>
    dplyr::select(AUID, uuid, phd_organization_uuid) |>
    dplyr::distinct()

  merged <- dplyr::left_join(students, persons_students, by = "AUID")
  no_match <- merged |> 
    dplyr::filter(is.na(.data$uuid)) |>
    dplyr::select(-uuid, -phd_organization_uuid)

  # Try fallback by full_name
  fallback <- dplyr::left_join(no_match,
                               persons |> 
                                 dplyr::select(full_name, uuid, phd_organization_uuid) |> 
                                 dplyr::distinct(),
                               by = "full_name")

  matched <- dplyr::bind_rows(
    merged |> dplyr::filter(!is.na(.data$uuid)),
    fallback |> dplyr::filter(!is.na(.data$uuid))
  ) |> dplyr::distinct()

  still_unmatched <- fallback |> 
    dplyr::filter(is.na(.data$uuid)) |>
    dplyr::select(-uuid, -phd_organization_uuid)

  if (!is.null(out_nomatch_path)) {
    readr::write_csv(still_unmatched, out_nomatch_path)
  }
  list(matched = matched, unmatched = still_unmatched)
}

#' Match internal supervisors (drop externals). Matches supervisors by 1. their full name, 2. their very first and very last name, 3. their email.
#'
#' @param supervisors Tibble with columns: full_name, email, role, organization, unique_id
#' @param persons Tibble with internal persons info
#' @return Tibble with supervisor matches incl. uuid
#' @export
match_supervisors <- function(supervisors, persons) {
  # Split emails
  supervisors <- supervisors |> dplyr::mutate(
    email_domain = stringr::str_extract(email, "(?<=@).*"),
    internExtern = dplyr::if_else(stringr::str_detect(organization, "External"), "external", "internal")
  )
  sup_int <- supervisors |> dplyr::filter(.data$internExtern == "internal")

  persons_int <- persons |>
    # dplyr::filter(is.na(.data$email_domain) | grepl("(^|\\.)au\\.|(^|\\.)rm\\.", email_domain)) |>
    dplyr::mutate(first_first_name = stringr::word(first_name, start = 1),
                  last_last_name = stringr::word(last_name, -1)) |>
    dplyr::distinct()

  # 1) full name join
  by_name <- dplyr::left_join(sup_int,
                              persons_int |> 
                                dplyr::select(full_name, uuid) |> 
                                dplyr::distinct(),
                              by = "full_name")
  matched1 <- by_name |> dplyr::filter(!is.na(.data$uuid))
  remainder <- by_name |> dplyr::filter(is.na(.data$uuid))

  # 2) first-first + last-last
  by_ffll <- dplyr::left_join(
    remainder |> dplyr::mutate(first_first_name = stringr::word(full_name, 1),
                               last_last_name = stringr::word(full_name, -1)) |> dplyr::select(-uuid),
    persons_int |> dplyr::select(first_first_name, last_last_name, uuid) |> dplyr::distinct(),
    by = c("first_first_name", "last_last_name")
  ) |> dplyr::distinct() |> dplyr::select(-first_first_name, -last_last_name)

  matched2 <- by_ffll |> dplyr::filter(!is.na(.data$uuid))
  remainder2 <- by_ffll |> dplyr::filter(is.na(.data$uuid))

  # 3) by email
  by_email <- dplyr::left_join(
    remainder2 |> dplyr::filter(grepl("(^|\\.)au\\.|(^|\\.)rm\\.", email_domain)) |> dplyr::select(-uuid),
    persons_int |> dplyr::select(email, uuid) |> dplyr::distinct(),
    by = "email"
  )

  matched <- dplyr::bind_rows(matched1, matched2, by_email) |> dplyr::distinct() |>
    dplyr::select(-c(email_domain, internExtern)) |>     dplyr::distinct(unique_id, organization, full_name, .keep_all = TRUE)
  matched
}
