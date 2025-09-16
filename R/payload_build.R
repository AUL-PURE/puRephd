#' Build payload for existing thesis update. NOTE: this code is designed for the PURE version of Aarhus University. Chances are high, that you need to adapt the URIs etc. for your institution's version of PURE.
#' @param template list (existing object)
#' @param student row for one AUID (with mapping applied)
#' @param supervisors tibble rows for that AUID
#' @param award_org_uuid default awarding org
#' @param staging logical; chooses bookseries uuid field, as bookseries-uuids can be different on staging
#' @return list payload
#' @export
build_existing_payload <- function(template, student, supervisors, award_org_uuid, staging = FALSE) {
  # Ensure type
  template$type$uri <- "/dk/atira/pure/researchoutput/researchoutputtypes/thesis/doc"
  template$type$term$en_GB <- "PhD thesis"
  template$type$term$da_DK <- "Ph.d.-afhandling"
  template$typeDiscriminator <- "Thesis"

  # Dates
  date_parts <- strsplit(as.character(student$termination_date), "-")[[1]]

  # Title/subtitle: keep template title if present; else use student
  if (length(grep(":", template$title$value)) > 0) {
    s <- split_title_subtitle(template$title$value)
    template$title$value <- s$title
    template$subTitle$value <- s$subtitle
  } else {
    template$subTitle$value <- ""
  }

  # Publication status: set to published at award date
  existing_idx <- NULL
  for (i in seq_along(template$publicationStatuses)) {
    if (!is.null(template$publicationStatuses[[i]]$publicationStatus$uri) &&
        template$publicationStatuses[[i]]$publicationStatus$uri == "/dk/atira/pure/researchoutput/status/published") {
      existing_idx <- i; break
    }
  }
  status <- list(
    publicationStatus = list(
      uri = "/dk/atira/pure/researchoutput/status/published",
      term = list(en_GB = "Published", da_DK = "Udgivet")
    ),
    publicationDate = list(year = date_parts[1], month = date_parts[2], day = date_parts[3])
  )
  if (!is.null(existing_idx)) template$publicationStatuses[[existing_idx]] <- status
  else template$publicationStatuses[[length(template$publicationStatuses) + 1]] <- status

  # Workflow
  template$workflow$step <- "approved"
  template$workflow$description$en_GB <- "Validated"
  template$workflow$description$da_DK <- "Valideret"

  # Managing org & dates
  template$managingOrganization$uuid <- student$organization_uuid
  template$submissionYear <- date_parts[1]
  template$awardDate <- as.character(student$termination_date)

  # Awarding institution
  template$awardingInstitutions <- list(list(organizationRef = list(systemName = "Organization", uuid = award_org_uuid)))

  # Book series
  bs_uuid <- if (isTRUE(staging)) student$bookseries_uuid_staging else student$bookseries_uuid_production
  if (length(template$bookSeries) >= 1 && !(student$bookseries_title %in% unlist(template$bookSeries))) {
    template$bookSeries[[length(template$bookSeries) + 1]] <- list(journal = list(uuid = bs_uuid, systemName = "Journal"), no = date_parts[2], volume = date_parts[1])
  } else if (length(template$bookSeries) == 0) {
    template$bookSeries[[1]] <- list(journal = list(systemName = "Journal", uuid = bs_uuid), no = date_parts[2], volume = date_parts[1])
  }

  # Research area
  template$mainResearchArea$uri <- paste0("/dk/atira/pure/mainresearcharea/", student$mainResearchArea_EN)
  template$mainResearchArea$term$en_GB <- ""
  template$mainResearchArea$term$da_DK <- ""

  # Supervisors
  if (nrow(supervisors) > 0) {
    sup_list <- vector("list", nrow(supervisors))
    for (i in seq_len(nrow(supervisors))) {
      role <- if (identical(supervisors$role[i], "Main supervisor"))
        list(uri = "/dk/atira/pure/researchoutput/roles/internalexternal/thesis/supervisor", term = list(en_GB = "Supervisor", da_DK = "Vejleder"))
      else
        list(uri = "/dk/atira/pure/researchoutput/roles/internalexternal/thesis/co_supervisor", term = list(en_GB = "Co-supervisor", da_DK = "Medvejleder"))
      sup_list[[i]] <- list(person = list(systemName = "Person", uuid = supervisors$uuid[i]), role = role)
    }
    template$supervisors <- sup_list
  }
  template
}


#' Build payload for new thesis creation from package template. NOTE: this code is designed for the PURE version of Aarhus University. Chances are high, that you need to adapt the URIs etc. for your institution's version of PURE.
#' @param template list loaded from system.file('extdata','phd_template.json', package='purephd')
#' @param student row (mapped)
#' @param supervisors tibble rows for that AUID
#' @param staging logical; chooses bookseries uuid field
#' @return list
#' @export
build_new_payload <- function(template, student, supervisors, staging = FALSE) {
  date_parts <- strsplit(as.character(student$termination_date), "-")[[1]]
  nm <- strsplit(student$full_name, " ")[[1]]
  first <- paste(nm[seq_len(length(nm)-1)], collapse = " ")
  last  <- nm[length(nm)]

  ttl <- split_title_subtitle(student$dissertation_title)
  template$title$value <- ttl$title
  if (!identical(ttl$subtitle, "")) template$subTitle$value <- ttl$subtitle

  template$publicationStatuses[[1]]$publicationDate$year  <- date_parts[1]
  template$publicationStatuses[[1]]$publicationDate$month <- date_parts[2]
  template$publicationStatuses[[1]]$publicationDate$day   <- date_parts[3]

  template$contributors[[1]]$name$firstName <- first
  template$contributors[[1]]$name$lastName  <- last
  template$contributors[[1]]$person$uuid    <- student$uuid

  template$contributors[[1]]$organizations[[1]]$uuid <- student$phd_organization_uuid
  template$organizations[[1]]$uuid <- student$phd_organization_uuid

  template$managingOrganization$uuid <- student$organization_uuid
  template$submissionYear <- date_parts[1]
  template$awardDate <- as.character(student$termination_date)

  bs_uuid <- if (isTRUE(staging)) student$bookseries_uuid_staging else student$bookseries_uuid_production
  template$bookSeries[[1]]$journal$uuid <- bs_uuid
  template$bookSeries[[1]]$no <- date_parts[2]
  template$bookSeries[[1]]$volume <- date_parts[1]

  template$mainResearchArea$uri <- paste0("/dk/atira/pure/mainresearcharea/", student$mainResearchArea_EN)

  if (nrow(supervisors) > 0) {
    sup_list <- vector("list", nrow(supervisors))
    for (i in seq_len(nrow(supervisors))) {
      role <- if (identical(supervisors$role[i], "Main supervisor"))
        list(uri = "/dk/atira/pure/researchoutput/roles/internalexternal/thesis/supervisor", term = list(en_GB = "Supervisor", da_DK = "Vejleder"))
      else
        list(uri = "/dk/atira/pure/researchoutput/roles/internalexternal/thesis/co_supervisor", term = list(en_GB = "Co-supervisor", da_DK = "Medvejleder"))
      sup_list[[i]] <- list(person = list(systemName = "Person", uuid = supervisors$uuid[i]), role = role)
    }
    template$supervisors <- sup_list
  }
  template
}
