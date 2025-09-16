# puRephd

This R package provides tools to automate the creation and updating of PhD thesis records in PURE, based on structured CSV data from the graduate school. It helps match students and supervisors with internal PURE records, enrich metadata, and upload it via the PURE API.

The package is structured in the following way:

puRephd/  
├─ DESCRIPTION, NAMESPACE, LICENSE, .Rbuildignore, README.md  
├─ R/  
│  ├─ README.md                    # overview over all scripts and their containing functions  
│  ├─ config.R                     # load_config(), use_pure_credentials()  
│  ├─ io_read.R                    # read_gradschool(), read_existing_theses(), read_mappings()  
│  ├─ persons.R                    # get_internal_persons(), match_students(), match_supervisors()  
│  ├─ orgs.R                       # map_organizations_bookseries(), validate_bookseries()  
│  ├─ text.R                       # escape_xhtml(), split_title_subtitle()  
│  ├─ payload_build.R              # build_existing_payload(), build_new_payload()  
│  ├─ pure_api.R                   # pure_get(), pure_put()  
│  ├─ reports.R                    # write_reports()  
│  └─ workflow.R                   # run_phd_workflow()  
└─ inst/extdata/phd_template.json  # your thesis JSON template (package-bundled)  


## Install (local) & load

To use the package, you must first install it locally. This assumes you have the source folder available. The package is not on CRAN, so installation is done using `remotes::install_local()`, or `remotes::install_github()`.

```r
# from local folder created by Copilot
remotes::install_local("puRephd")
library(puRephd)
```


## Quick start

This section walks you through the initial setup required to use the package, including authentication, environment configuration, and loading internal PURE data.

### 1. Import of API-keys via constants.R-file

PURE requires authentication via API keys. These keys must have read and write access to the `/research-outputs/` endpoint. Store them in a file called `constants.R` with variables `api_key` and `api_key_staging`. Use `use_pure_credentials()` to load them into your session.

```r
# import personal api-keys
source("path/to/constants.R") # load api_key, api_key_staging

# API-keys are loaded automatically from constants.R
# no need to modify manually
puRephd::use_pure_credentials(
  # Your production API-key with read&write authorization for /research-outputs/
  prod_key = api_key, 
  # Your staging API-key with read&write authorization for /research-outputs/
  staging_key = api_key_staging)
```

### 2. Set system environment

You can override default settings using `Sys.setenv()`. This is useful for specifying which PURE server to use (production or staging), and which organization UUID to assign as the awarding institution. The UUID for Aarhus University is provided as an example.

```r
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

```r
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

**Input CSV file requirements:**

- All files must be UTF-8 encoded
- `gradschool_csv`: must contain columns like `AUID`, `Full name`, `Dissertation title`, `Team name`, `Account`
- `existing_theses_csv`: must contain `AUID` of PhD students and `UUID` of existing theses
- `accounts_csv` and `institutes_csv`: must contain mappings for organizational units and book series
- Folder structure should include `1_data/`, `2_scripts/`, and `3_output/`

```r
res <- run_phd_workflow(
  gradschool_csv      = "path/to/theses_to_be_created.csv", # File-path to the .csv file (UTF-8 encoded) provided by the graduate school with all newly awarded PhD titles.
  existing_theses_csv = "path/to/existing_theses_in_PURE.csv", # File-path to the .csv file from a PURE-report listing all existing PhD theses in PURE together with their UUIDs.
  accounts_csv        = "path/to/internal_organizations_and_their_parents.csv", # Path to CSV-file linking internal organizations and their parent organizations.
  institutes_csv      = "path/to/institute_thesaurus.csv", # Path to thesaurus file linking internal organizations with organization names as provided by the graduate school.
  out_dir             = "path/to/output_directory", # Path to the folder, where the logging-files, etc. of this project should be stored. Will be created, if non-existing.
  dry_run             = TRUE, # Ff TRUE, run script without `PUT`-ting theses to PURE.
  staging             = TRUE,
  internal_persons = internal_persons,
  verbose = TRUE,        # show progress
  abort_on_error = FALSE  # stop on first error)
```

It is recommended to first run a "dry run" (`dry_run = TRUE`), in order to be able to inspect its output, and make necessary changes to the raw data, before setting `dry_run = FALSE` to upload it to the PURE server.

### 5. Inspect a random payload


After running the workflow, you can inspect the generated JSON payloads for both updated and newly created theses. This is useful for validation before uploading to PURE. The JSON structure follows PURE's API schema for research outputs.

```r
# Updated, existing theses
rand_existing_id <- sample(names(res$json_existing), 1)
jsonlite::toJSON(res$json_existing[[rand_existing_id]], auto_unbox = TRUE, pretty = TRUE)

# Newly created theses
rand_new_auid <- sample(names(res$json_new), 1)
jsonlite::toJSON(res$json_new[[rand_new_auid]], auto_unbox = TRUE, pretty = TRUE)

```



