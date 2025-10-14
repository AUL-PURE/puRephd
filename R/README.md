# R Folder Documentation

This document provides an overview of the R scripts included in the `puRephd` package. Each section corresponds to a script file in the `R/` folder and lists the functions defined within, along with a brief description of their purpose.

---

## config.R

- `load_config()`: Reads API keys and base URLs from environment variables or function arguments.

- `use_pure_credentials()`: Set PURE credentials into env vars (helper).

- `%||%`: A custom infix operator that provides a default value when x is either NULL or an empty string (""). This operator is useful when you want to ensure that a variable has a meaningful value, and if not, fall back to a default.

---

## pure_api.R

- `pure_get()`: GET from PURE API.

- `pure_put()`: PUT to PURE API.

- `bulk_download_from_api()`: Bulk download from PURE API.

---

## utils.R

- `assert_that()`: Simple assert helper.

---

## io_read.R

- `read_gradschool()`: Reads grad school CSV of PhD students, following a specific format. The columns of the file have to have the following columns in the following order: 
  1. "__Full name__": The PhD student's full name (FIRST_NAME LAST_NAME),
  2. "__AUID__": The PhD student's employee-id,
  3. "__Last termination type__": - , 
  4. "__Last termination date__": Award date,
  5. "__Team name__": Abbrev. faculty,
  6. "__Account__": Name of PhD student's department,
  7. "__Dissertation title__": Dissertation title (format: "TITLE: SUBTITLE"),
  8. "__Full name__": The supervisor's full name (FIRST_NAME LAST_NAME),
  9. "__E-mail__": The supervisor's official e-mail address,
  10. "__Role__": Supervisor role ("Main supervisor", "Co-supervisor"),
  11. "__Account__": Name of internal supervisor's department.


- `read_existing_theses()`: Reads a CSV file of existing theses, exported from PURE via its report-module. It needs to contain a column with the students' employee-ids and the theses' uuids.

- `read_mappings()`: A function to map the univeristy's organizations and institutes to the information provided by grad school CSV.

---

## orgs.R

- `map_organizations_bookseries()`: Map organizations + book series + faculty.

- `validate_bookseries()`: Validate that book series exists for each thesis.

---

## persons.R

- `get_internal_persons()`: Get internal persons from PURE or cache.

- `match_students()`:  Match PhD students against internal persons.

- `match_supervisors()`: Match internal supervisors against internal persons (drop external supervisors). ATTENTION! Filters hard-coded e-mail domains that contain ".au" and ".rm". Needs to be changed in the script for your own organisation.

---

## text.R

- `escape_xhtml()`: Escape XHTML special characters.
- `split_title_subtitle()`: Split title/subtitle on colon.

---


## payload_build.R

- `build_existing_payload()`: Build API-payload for existing thesis update.

- `build_new_payload()`: Build API-payload for new thesis creation from theses json-template.

---

## workflow.R

- `run_phd_workflow()`: Run the full PhD theses workflow (with progress + robust errors).

---


## reports.R  

- `write_reports()`: Write standard reports.

---




