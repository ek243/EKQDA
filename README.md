# EKQDA

<!-- badges: start -->
[![R-CMD-check](https://github.com/ek243/EKQDA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ek243/EKQDA/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

EKQDA is an R package for computer-assisted qualitative data analysis. It
stores documents, codes, coded segments and memos in a single SQLite file, and
offers both an R API and an interactive Shiny app. It is intended as a
free, scriptable alternative to the no-longer-maintained
[RQDA](https://rqda.r-forge.r-project.org/) package.

The package is in early development (version 0.1.0).

## Installation

Install the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("ek243/EKQDA")
```

## Quick start

``` r
library(EKQDA)

# Create a project (an SQLite file) and import plain-text documents
project <- qual_init("my_research.sqlite")
qual_import_text(project, "interview1.txt")
qual_import_text(project, "interview2.txt")

# Define codes and apply them to segments of text
qual_add_code(project, "justice", "Discussions of justice", "#FF3333")
qual_add_code(project, "equality", "Mentions of equality", "#33FF33")
qual_apply_code(project, doc_id = 1, code_id = 1, start = 50, end = 150,
                text = "The justice system needs reform...")

# Retrieve and analyse
qual_get_codings(project, code_id = 1)
qual_code_frequency(project)
qual_code_cooccurrence(project)                     # documents containing both codes
qual_code_cooccurrence(project, level = "segment")  # overlapping coded segments

DBI::dbDisconnect(project$connection)
```

See `vignette("introduction", package = "EKQDA")` for a fuller walk-through.

## The Shiny app

``` r
qual_launch_app("my_research.sqlite")
```

The app lets you:

- add and read documents,
- create codes and apply them to text,
- write code, document and general memos,
- retrieve all segments for a code and export them as CSV, text or Word,
- view code frequencies and a code co-occurrence plot.

## Functions

| Function | Purpose |
|---|---|
| `qual_init()` | Create or open a project database |
| `qual_import_text()` | Import a plain-text document |
| `qual_add_code()`, `qual_apply_code()` | Define codes and code segments |
| `qual_get_documents()`, `qual_get_codes()`, `qual_get_codings()` | Read project contents |
| `qual_code_frequency()` | How often each code is used |
| `qual_code_cooccurrence()` | Code co-occurrence matrix (document or segment level) |
| `qual_get_code_memos()`, `qual_get_document_memos()`, `qual_get_general_memos()` | Read memos |
| `qual_project_summary()` | Counts of documents, codes and codings |
| `qual_launch_app()` | Open the Shiny interface |

## Current limitations

- Documents must be plain text.
- In the app, you paste the passage you want to code and enter its start and
  end positions; selecting text directly in the document view is not yet
  supported.
- Projects are local, single-user SQLite files.
- There is no import from RQDA project files yet.

## Development

``` r
devtools::document()  # regenerate NAMESPACE and help pages
devtools::test()      # run the tests
devtools::check()     # full R CMD check
```

## License

MIT, see [LICENSE.md](LICENSE.md).
