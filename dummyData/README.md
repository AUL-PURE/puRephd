# Requirements

This script relies on four CSV input files with a header row, comma as delimiter, and UTF‑8 encoding. Date fields (if any) use DD/MM/YYYY (e.g., 31/12/2025). Each file’s column names and order must be exact, including cases where the same header name appears more than once—the script distinguishes these by position. 1

## 1 `theses_to_be_created.csv` - source list of PhD theses to register

Each row represents a thesis to supervisor relation; multiple rows with the same candidate + title indicate multiple supervisors. Required columns in this exact order (note the duplicate names by design):
`Full name`, `AUID`, `Last termination type`, `Last termination date`, `Team name`, `Account`, `Dissertation title`, `Full name`, `E-mail`, `Role`, `Account`. 

The first Full name/Account pair refers to the candidate; the second Full name/Account (and E-mail, Role) refer to the supervisor.


## 2 `existing_theses_in_PURE.csv` - de‑duplication reference from PURE report module

Used to avoid creating records that already exist in PURE. Required columns (exact order):
`UUID`, `Titel`, `Type`, `Indberetningsår`, `Bidragsyderes organisationer`, `Navn`, `UUID`, `Efternavn`, `Fornavn(e)`, `Medarbejder id`.

This file intentionally contains two columns named `UUID`. By position, the first `UUID` is the publication (thesis) UUID; the second `UUID` (appearing after Navn) corresponds to the person (contributor) UUID.


## 3 `institute_thesaurus.csv` - organizational thesaurus & book‑series mapping

Lookup of internal institutes/departments with PURE identifiers, parent links, research area tags, and associated PhD book‑series identifiers (both production and staging). Required columns (exact order):
`ID`, `organization_name_EN`, `organization_name_DK`, `UUID`, `mainResearchArea_EN`, `type`, `parentsUUID`, `grad_school_org`, `bookseries_title`, `bookseries_uuid_production`, `bookseries_uuid_staging`.

PhD book-series have to exist in PURE.


## 4 `internal_organizations_and_their_parents.csv` - org to parent mapping

Minimal mapping used to resolve local organizational hierarchy for accounts and affiliations. Required columns (exact order): `Company name`, `Parent`.
