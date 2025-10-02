#' Run the full PhD theses workflow (with progress bar + robust errors)
#'
#' Orchestrates reading inputs, matching persons and supervisors, building payloads and
#' updating/creating theses in PURE. Produces CSV reports in `out_dir` and prints
#' progress messages for each major step.
#'
#' @param gradschool_csv       character. Path to CSV file of theses_to_be_created.csv.
#' @param existing_theses_csv  character. Path to existing PURE theses; CSV-file exported from report module (i.e., existing_theses_in_PURE.csv).
#' @param accounts_csv         character. Path to CSV-file linking internal organizations and their parent organizations (i.e., internal_organizations_and_their_parents.csv).
#' @param institutes_csv       character. Path to thesaurus file linking internal organizations with organization names as provided by the graduate school (i.e., institute_thesaurus.csv).
#' @param out_dir              character. Output directory for logs/reports. Is created via script, if nonexistent but path provided.
#' @param staging              logical. Use staging vs production configuration.
#' @param dry_run              logical. If TRUE, builds payloads but does not call API.
#' @param internal_persons     optional tibble. If `NULL`, uses `get_internal_persons()`
#'                             (which you must implement to return the expected columns).
#' @param verbose              logical. Print progress messages (default TRUE).
#' @param abort_on_error       logical. Stop on first error (default TRUE). If FALSE,
#'                             continues where possible and returns partial results.
#'
#' @return A list with the data and results:
#'         `students`, `students_matched`, `students_mapped`, `supervisors`,
#'         `supervisors_matched`, `json_existing`, `json_new`,
#'         `put_existing`, `put_new`, `failed_get`.
#'
#' @examples
#' \dontrun{
#' res <- run_phd_workflow(
#'   gradschool_csv      = "path/to/theses_to_be_created.csv",
#'   existing_theses_csv = "path/to/existing_theses_in_PURE.csv",
#'   accounts_csv        = "path/to/internal_organizations_and_their_parents.csv",
#'   institutes_csv      = "path/to/institute_thesaurus.csv",
#'   out_dir             = "path/to/output_directory",
#'   staging             = FALSE,
#'   dry_run             = TRUE,
#'   verbose             = TRUE,
#'   abort_on_error      = TRUE
#' )
#' }
#' @export
run_phd_workflow <- function(
    gradschool_csv,
    existing_theses_csv,
    accounts_csv,
    institutes_csv,
    out_dir,
    staging          = FALSE,
    dry_run          = TRUE,
    internal_persons = NULL,
    verbose          = TRUE,
    abort_on_error   = TRUE
) {
  
  # ---- small logging helpers -------------------------------------------------
  .ts  <- function() format(Sys.time(), "%H:%M:%S")
  
  say  <- function(type, msg) {
    if (isTRUE(verbose)) cat(sprintf("[%s] %s %s\n", .ts(), type, msg))
  }
  
  info <- function(msg) say("INFO ", msg)
  ok   <- function(msg) say("OK   ", msg)
  
  warn <- function(msg) {
    warning(msg, call. = FALSE)
    say("WARN ", msg)
  }
  
  err  <- function(msg) say("ERROR", msg)
  
  try_step <- function(expr, step_name) {
    info(sprintf("%s ...", step_name))
    tryCatch(
      {
        val <- force(expr)
        ok(step_name)
        val
      },
      error = function(e) {
        err(sprintf("%s — %s", step_name, conditionMessage(e)))
        if (isTRUE(abort_on_error)) {
          rlang::abort(paste0(step_name, ": ", conditionMessage(e)))
        }
        NULL
      }
    )
  }
  
  step_i    <- 0L
  n_steps   <- 13L  # total number of major steps below
  next_step <- function(label) {
    step_i <<- step_i + 1L
    if (isTRUE(verbose)) {
      cat(sprintf("\n== Step %d/%d: %s ==\n", step_i, n_steps, label))
    }
  }
  
  start_time <- Sys.time()
  on.exit({
    if (isTRUE(verbose)) {
      cat(sprintf(
        "\nCompleted in %0.1f sec.\n",
        as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      ))
    }
  })
  
  # ---- Step 1: configuration -------------------------------------------------
  next_step("Load configuration & check output folder")
  
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  cfg <- try_step(
    load_config(staging = staging),
    "Load configuration"
  )
  
  # ---- Step 2: input validation ---------------------------------------------
  next_step("Validate input files exist")
  
  must_exist <- function(p, label) {
    if (!file.exists(p)) {
      rlang::abort(sprintf(
        "%s not found: %s",
        label,
        normalizePath(p, winslash = "/", mustWork = FALSE)
      ))
    }
  }
  
  try_step({
    must_exist(gradschool_csv,      "gradschool_csv")
    must_exist(existing_theses_csv, "existing_theses_csv")
    must_exist(accounts_csv,        "accounts_csv")
    must_exist(institutes_csv,      "institutes_csv")
  }, "Validate file paths")
  
  # ---- Step 3: read inputs ---------------------------------------------------
  next_step("Read input datasets")
  
  students <- try_step(
    read_gradschool(gradschool_csv),
    "Read grad school CSV"
  )
  
  existing <- try_step(
    read_existing_theses(existing_theses_csv),
    "Read existing theses CSV"
  )
  
  mapping <- try_step(
    read_mappings(accounts_csv, institutes_csv),
    "Read accounts/institutes mapping"
  )
  
  # Non-fatal sanity check for mapping columns
  try_step({
    required <- c(
      "organization_uuid",
      "bookseries_title",
      "bookseries_uuid_staging",
      "bookseries_uuid_production",
      "mainResearchArea_EN"
    )
    missing <- setdiff(required, names(mapping))
    if (length(missing) > 0) {
      warn(sprintf(
        "Mapping is missing columns: %s",
        paste(missing, collapse = ", ")
      ))
    }
    invisible(TRUE)
  }, "Validate mapping columns (non-fatal)")
  
  # ---- Step 4: internal persons ---------------------------------------------
  next_step("Load internal persons")
  
  if (is.null(internal_persons)) {
    internal_persons <- try_step(
      get_internal_persons(
        base_url             = cfg$base_url,
        headers              = cfg$headers,
        download_new_and_save = TRUE
      ),
      "Fetch internal persons (or read cache)"
    )
  } else {
    ok("Using supplied `internal_persons` tibble")
  }
  
  # ---- Step 5: student matching ---------------------------------------------
  next_step("Match students with internal persons")
  
  m_students <- try_step(
    match_students(
      students,
      internal_persons,
      out_nomatch_path = file.path(out_dir, "students_unmatched.csv")
    ),
    "Match students"
  )
  
  students_matched <- m_students$matched
  
  if (nrow(m_students$unmatched) > 0) {
    warn(sprintf(
      "%d students not matched; written to students_unmatched.csv",
      nrow(m_students$unmatched)
    ))
  }
  
  # ---- Step 6: supervisors extraction ---------------------------------------
  next_step("Extract supervisors from grad school data")
  
  raw <- try_step(
    readr::read_csv(gradschool_csv, show_col_types = FALSE),
    "Re-read raw file for supervisor columns"
  )
  
  sup_cols <- c("Full name...8", "E-mail", "Role", "Account...11")
  has_sup  <- all(sup_cols %in% names(raw))
  
  supervisors <- try_step({
    if (!has_sup) {
      warn("Supervisor columns not found; continuing without supervisors")
      tibble::tibble(
        full_name   = character(),
        email       = character(),
        role        = character(),
        organization = character(),
        unique_id   = character()
      )
    } else {
      raw |>
        dplyr::select(dplyr::all_of(c(
          "Full name...8", "E-mail", "Role", "Account...11", "AUID"
        ))) |>
        dplyr::distinct() |>
        dplyr::rename(
          full_name   = `Full name...8`,
          email       = `E-mail`,
          role        = Role,
          organization = `Account...11`,
          unique_id   = AUID
        )
    }
  }, "Prepare supervisors tibble")
  
  # ---- Step 7: supervisor matching ------------------------------------------
  next_step("Match supervisors with internal persons (internal only)")
  
  sup_matched <- try_step(
    match_supervisors(supervisors, internal_persons),
    "Match supervisors"
  )
  
  # ---- Step 8: orgs & book series mapping -----------------------------------
  next_step("Map organizations, faculties and book series")
  
  students_mapped <- try_step(
    map_organizations_bookseries(students_matched, mapping),
    "Join org/bookseries/faculty mapping"
  )
  
  missing_bs <- try_step(
    validate_bookseries(students_mapped),
    "Validate book series (non-fatal)"
  )
  
  if (nrow(missing_bs) > 0) {
    warn(sprintf(
      "%d theses have missing book series mapping",
      nrow(missing_bs)
    ))
    readr::write_csv(missing_bs, file.path(out_dir, "missing_bookseries.csv"))
  }
  
  # ---- Step 9: determine existing vs new ------------------------------------
  next_step("Partition into existing vs new theses")
  
  theses_not_in_pure <- try_step(
    dplyr::anti_join(students_mapped, existing, by = "AUID"),
    "Compute new theses"
  )
  
  theses_in_both <- try_step(
    dplyr::semi_join(students_mapped, existing, by = "AUID") |>
      dplyr::left_join(existing, by = "AUID"),
    "Compute existing theses"
  )
  
  info(sprintf(
    "Theses existing: %d, new: %d",
    nrow(theses_in_both),
    nrow(theses_not_in_pure)
  ))
  
  # ---- Step 10: fetch existing templates from PURE --------------------------
  next_step("Download existing theses from PURE as templates")
  
  existing_templates <- list()
  failed_get         <- tibble::tibble()
  
  if (isTRUE(nrow(theses_in_both) > 0)) {
    
    pb <- progress::progress_bar$new(
      format = " downloading [:bar] :percent eta: :eta",
      total  = length(unique(theses_in_both$thesis_uuid)),
      clear  = FALSE,
      width  = 60
    )
    
    for (u in unique(theses_in_both$thesis_uuid)) {
      pb$tick()
      tpl <- tryCatch(
        pure_get(cfg$base_url, sprintf("research-outputs/%s", u), cfg$headers),
        error = function(e) NULL
      )
      if (is.null(tpl)) {
        failed_get <- dplyr::bind_rows(
          failed_get,
          tibble::tibble(thesis_uuid = u)
        )
      } else {
        existing_templates[[u]] <- tpl
      }
    }
    
    if (nrow(failed_get) > 0) {
      warn(sprintf("Failed to download %d existing theses", nrow(failed_get)))
    }
    
  } else {
    info("No existing theses to download")
  }
  
  # ---- Step 11: build payloads ----------------------------------------------
  next_step("Build JSON payloads (existing and new)")
  
  json_existing <- try_step({
    out <- list()
    if (length(existing_templates) > 0) {
      for (auid in unique(theses_in_both$AUID)) {
        uu   <- theses_in_both$thesis_uuid[theses_in_both$AUID == auid][1]
        tpl  <- existing_templates[[uu]]
        srow <- students_mapped[students_mapped$AUID == auid, ][1, ]
        sup  <- sup_matched[sup_matched$unique_id == auid, ]
        out[[uu]] <- build_existing_payload(
          template         = tpl,
          student          = srow,
          supervisors      = sup,
          award_org_uuid   = cfg$awarding_org_uuid,
          staging          = staging
        )
      }
    }
    out
  }, "Build payloads for existing theses")
  
  json_new <- try_step({
    out <- list()
    if (nrow(theses_not_in_pure) > 0) {
      template_raw <- jsonlite::fromJSON(
        system.file("extdata", "phd_template.json", package = "purephd"),
        simplifyVector = FALSE
      )
      for (auid in unique(theses_not_in_pure$AUID)) {
        srow <- students_mapped[students_mapped$AUID == auid, ][1, ]
        sup  <- sup_matched[sup_matched$unique_id == auid, ]
        out[[auid]] <- build_new_payload(
          template    = template_raw,
          student     = srow,
          supervisors = sup,
          staging     = staging
        )
      }
    }
    out
  }, "Build payloads for new theses")
  
  # ---- Step 12: upload (optional) -------------------------------------------
  next_step(if (dry_run) "Dry run — skipping API upload" else "Upload to PURE API (PUT)")
  
  put_existing_df <- NULL
  put_new_df      <- NULL
  
  if (!isTRUE(dry_run)) {
    
    put_existing_df <- try_step({
      if (length(json_existing) > 0) {
        info(sprintf("Updating %d existing theses", length(json_existing)))
        put_existing <- purrr::imap(
          json_existing,
          ~ pure_put(
            cfg$base_url,
            sprintf("research-outputs/%s", .y),
            cfg$headers,
            .x
          )
        )
        tibble::tibble(
          ID       = names(put_existing),
          Response = purrr::map_int(put_existing, ~ .x$status_code)
        )
      } else {
        tibble::tibble(ID = character(), Response = integer())
      }
    }, "PUT existing theses")
    
    put_new_df <- try_step({
      if (length(json_new) > 0) {
        info(sprintf("Creating %d new theses", length(json_new)))
        put_new <- purrr::imap(
          json_new,
          ~ pure_put(cfg$base_url, "research-outputs", cfg$headers, .x)
        )
        tibble::tibble(
          ID       = names(put_new),
          Response = purrr::map_int(put_new, ~ .x$status_code)
        )
      } else {
        tibble::tibble(ID = character(), Response = integer())
      }
    }, "PUT new theses")
  }
  
  # ---- Collect failures into a single failed_df ------------------------------
  # (If you already added this earlier, keep your version. Otherwise, paste the
  # consolidated failed_df construction here as discussed previously.)
  failed_list <- list()
  
  # 1) Students without internal person match
  if (exists("m_students") && nrow(m_students$unmatched) > 0) {
    failed_list$unmatched_students <- m_students$unmatched |>
      dplyr::transmute(
        AUID     = .data$AUID,
        source   = "students",
        reason   = "no_person_match",
        id       = .data$AUID,
        response = NA_integer_
      )
  }
  
  # 2) Failed GET for existing theses
  if (exists("failed_get") && nrow(failed_get) > 0) {
    map_uuid_auid <- theses_in_both |>
      dplyr::select(thesis_uuid, AUID) |>
      dplyr::distinct()
    
    failed_list$failed_get <- failed_get |>
      dplyr::left_join(map_uuid_auid, by = "thesis_uuid") |>
      dplyr::transmute(
        AUID     = .data$AUID,
        source   = "pure_get",
        reason   = "get_non_200",
        id       = .data$thesis_uuid,
        response = NA_integer_
      )
  }
  
  # 3) Failed PUT (existing updates) — non-200
  if (!is.null(put_existing_df) && nrow(put_existing_df) > 0) {
    failed_list$put_existing <- put_existing_df |>
      dplyr::filter(.data$Response != 200L) |>
      dplyr::left_join(
        theses_in_both |>
          dplyr::select(thesis_uuid, AUID) |>
          dplyr::distinct(),
        by = c("ID" = "thesis_uuid")
      ) |>
      dplyr::transmute(
        AUID     = .data$AUID,
        source   = "pure_put_existing",
        reason   = paste0("response_", .data$Response),
        id       = .data$ID,
        response = .data$Response
      )
  }
  
  # 4) Failed PUT (new creations) — non-201
  if (!is.null(put_new_df) && nrow(put_new_df) > 0) {
    failed_list$put_new <- put_new_df |>
      dplyr::filter(.data$Response != 201L) |>
      dplyr::transmute(
        AUID     = .data$ID,   # for new, ID = AUID
        source   = "pure_put_new",
        reason   = paste0("response_", .data$Response),
        id       = .data$ID,
        response = .data$Response
      )
  }
  
  failed_df <- dplyr::bind_rows(failed_list, .id = "bucket")
  
  # ---- New report: subset of gradschool_csv with only failed rows ------------
  # We rely on `raw` (unmodified grad school data) and the AUID column in it.
  failed_gradschool_path <- NULL
  
  if (exists("failed_df") && nrow(failed_df) > 0) {
    
    failed_auids <- failed_df |>
      dplyr::filter(!is.na(.data$AUID)) |>
      dplyr::pull(.data$AUID) |>
      as.character() |>
      unique()
    
    if (length(failed_auids) > 0 && "AUID" %in% names(raw)) {
      
      # Make sure AUID types match for a safe join/filter
      failed_gradschool_rows <- raw |>
        dplyr::mutate(AUID = as.character(.data$AUID)) |>
        dplyr::filter(.data$AUID %in% failed_auids)
      
      # Only write if we actually found rows
      if (nrow(failed_gradschool_rows) > 0) {
        failed_gradschool_path <- file.path(
          out_dir,
          paste0("GRADSCHOOL_ONLY_FAILED_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
        )
        readr::write_csv(failed_gradschool_rows, failed_gradschool_path)
        
        # nice console message
        ok(sprintf(
          "Wrote %d failed grad school rows to: %s",
          nrow(failed_gradschool_rows),
          failed_gradschool_path
        ))
      } else {
        warn("No matching rows found in grad school CSV for the failed AUIDs.")
      }
      
    } else {
      warn("Cannot create grad school subset: no failed AUIDs or 'AUID' column missing in grad school CSV.")
    }
  } else {
    info("No failures recorded; skipping grad school failed subset.")
  }
  
  
  # ---- Step 13: reports ------------------------------------------------------
  next_step("Write reports to out_dir")
  
  try_step(
    write_reports(
      out_dir,
      put_existing_df = put_existing_df,
      put_new_df      = put_new_df,
      failed_df       = if (exists("failed_df") && nrow(failed_df) > 0) failed_df else NULL
    ),
    "Write CSV reports"
  )
  
  
  # ---- Result bundle ---------------------------------------------------------
  list(
    students            = students,
    students_matched    = students_matched,
    students_mapped     = students_mapped,
    supervisors         = supervisors,
    supervisors_matched = sup_matched,
    json_existing       = json_existing,
    json_new            = json_new,
    put_existing        = put_existing_df,
    put_new             = put_new_df,
    failed_get          = failed_get
  )
}

