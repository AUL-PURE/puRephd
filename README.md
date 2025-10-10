# puRephd
**DOI**: [![DOI](https://zenodo.org/badge/511112726.svg)](https://zenodo.org/doi/10.5281/zenodo.17249334)  
**Maintainer**: [![ORCiD](https://img.shields.io/badge/ORCiD-0000--0001--7349--5401-green.svg)](http://orcid.org/0000-0001-7349-5401) David N. Matzig (<dnma@kb.dk>) 

`puRephd` is an R package that automates the creation and updating of PhD thesis records in PURE, based on structured CSV data. It matches students and supervisors with internal PURE profiles, enriches metadata, and uploads it via the PURE API.


## Introduction

Registering PhD theses in PURE has traditionally been a manual and fragmented process, involving repeated data entry, cross-checking multiple sources, and a high risk of human error. `puRephd` replaces this with an automated workflow. Each month, the graduate school provides a standardized CSV export from PhD Planner containing all completed PhD programmes. The package uses this file to create or enrich research-output entries in PURE. 

The package enables:

- matching students and supervisors with internal PURE profiles,
- assigning theses to the correct departmental series (e.g., "PhD theses - Department of XYZ"),
- identifying existing records in PURE to avoid duplication,
- generating templates for new or updated thesis entries,
- uploading data via the PURE API,
- logging success/failure and producing reports for follow-up.

Additionally, the package allows for the inclusion of metadata not previously available in PURE, such as internal supervisor names and departmental classification of theses.


## Disclaimer

The current version _0.1.X_ of `puRephd` is specifically designed for __Aarhus University's__ implementation of PURE. It includes hard-coded identifiers and settings tailored to AU's internal structure.

If you are from another institution and wish to use or adapt the package, please note:

- AU-specific IDs hard-coded in the R scripts will need to be modified,
- the PURE JSON template for the PhD theses may need to be modified,
- the structure of the input file may differ from your local setup,
- PURE API endpoints and authentication may require customization.

We welcome feedback and contributions to help make future versions more flexible and institution-agnostic.


## Input file requirements

This script relies on four CSV input files with a header row, comma as delimiter, and UTF‑8 encoding. Date fields (if any) use DD/MM/YYYY (e.g., 31/12/2025). Each file's column names and order must be exact, including cases where the same header name appears more than once - the script distinguishes these by position. You can read more about the file requirements here: [./dummyData](dummyData/).


## Install & load

To use the package, you must first install it locally. This assumes you have the source folder available. The package is not on CRAN, so installation is done using `remotes::install_local()`, or `remotes::install_github()`.

```{r}
if (!require("puRephd")) {
  # check if `remotes` is installed
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes")
  }
  # install `puRephd` from github
  remotes::install_github("https://github.com/AUL-PURE/puRephd", dependencies = TRUE)
  # Load package
  library(puRephd)
} else {
  # Load package
library(puRephd)
}
```


## Quick start

This section walks you through the initial setup required to use the package, including authentication, environment configuration, and loading internal PURE data.

### 1. Import of API-keys via constants.R-file

PURE requires authentication via API keys. These keys must have read access to the `/persons/` endpoint, and read and write access to the `/research-outputs/` endpoint. Store the keys in a file called `constants.R` with variables `api_key` and `api_key_staging`. Use `use_pure_credentials()` to load them into your session.

```{r}
# Import personal api-keys
source("path/to/constants.R") # load api_key, api_key_staging

# API-keys are loaded automatically from constants.R
# No need to modify manually
puRephd::use_pure_credentials(
  # Your production API-key with read&write authorization for PURE's /research-outputs/
  prod_key = api_key, 
  # Your staging API-key with read&write authorization for PURE's /research-outputs/
  staging_key = api_key_staging)
```

### 2. Set system environment

You can override default settings using `Sys.setenv()`. This is useful for specifying which PURE server to use (production or staging), and which organization UUID to assign as the awarding institution. The UUID for Aarhus University is provided as an example.

```{r}
# Optional: override defaults
Sys.setenv(
  # URL of AU-PURE production server
  PURE_BASE_URL_PROD = "pure.au.dk", 
  
  # URL of AU-PURE staging server
  PURE_BASE_URL_STAGING = "au-staging.elsevierpure.com", 
  
  # UUID of the University awarding the PhD degree (here: Aarhus University, AU)
  PURE_AWARDING_ORG_UUID = "fa9d17d6-3d7c-43bf-93f7-a18c40cf0778")
```

### 3. Download or load the tibble of all internal persons from PURE

PURE stores internal persons (e.g., students, supervisors) with unique identifiers (UUIDs). This function downloads or loads a cached list of internal persons, which is used to match individuals from the graduate school data.

```{r}
# Set config to staging (TRUE) or production (FALSE)
cfg <- puRephd::load_config(staging = TRUE)

# Internal persons
internal_persons <- puRephd::get_internal_persons(base_url = cfg$base_url, 
                                                  headers = cfg$headers, 
                                                  download_new_and_save = FALSE, # set to false, if you've run it with TRUE before and the .RDS-file exists in your ./1_data/ folder
                                                  size = 1000,
                                                  offset = 0)
```
### 4. Run the workflow

This is the main function of the package. It takes several CSV files as input and performs the following steps:

- Matches students and supervisors with PURE records
- Enriches metadata with organizational information
- Generates JSON payloads for each thesis
- Optionally uploads the data to PURE via PUT requests
- Logs results and errors to the specified output folder

**For input CSV file requirements see [./dummyData](dummyData/).**

```{r}
res <- run_phd_workflow(
  gradschool_csv      = "path/to/theses_to_be_created.csv", # File-path to the .csv file (UTF-8 encoded) provided by the graduate school with all newly awarded PhD titles.
  existing_theses_csv = "path/to/existing_theses_in_PURE.csv", # File-path to the .csv file from a PURE-report listing all existing PhD theses in PURE together with their UUIDs.
  accounts_csv        = "path/to/internal_organizations_and_their_parents.csv", # Path to CSV-file linking internal organizations and their parent organizations.
  institutes_csv      = "path/to/institute_thesaurus.csv", # Path to thesaurus file linking internal organizations with organization names as provided by the graduate school.
  out_dir             = "path/to/output_directory", # Path to the folder, where the logging-files, etc. of this project should be stored. Will be created, if non-existing.
  dry_run             = TRUE, # If TRUE, run script without `PUT`-ting theses to PURE.
  staging             = TRUE,
  internal_persons = internal_persons,
  verbose = TRUE,        # show progress
  abort_on_error = FALSE  # stop on first error
)
```

It is recommended to first run a "dry run" (`dry_run = TRUE`), in order to be able to inspect its output, and make necessary changes to the raw data, before setting `dry_run = FALSE` to upload it to the PURE server.

### 5. Inspect a random payload

After running the workflow, you can inspect the generated JSON payloads for both updated and newly created theses. This is useful for validation before uploading to PURE. The JSON structure follows PURE's API schema for research outputs.

```{r}
# Updated, existing theses
rand_existing_id <- sample(names(res$json_existing), 1)
jsonlite::toJSON(res$json_existing[[rand_existing_id]], auto_unbox = TRUE, pretty = TRUE)

# Newly created theses
rand_new_auid <- sample(names(res$json_new), 1)
jsonlite::toJSON(res$json_new[[rand_new_auid]], auto_unbox = TRUE, pretty = TRUE)
```






















