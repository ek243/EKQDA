#' Create a new code
#'
#' @inheritParams qual_import_text
#' @param name Name of the code.
#' @param description Optional description of what the code captures.
#' @param color Display colour as a hex string, used in the Shiny app.
#'
#' @return The `project`, invisibly.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' qual_add_code(project, "theme_justice", "Discussions of justice", "#FF3333")
#' qual_get_codes(project)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_add_code <- function(project, name, description = "", color = "#3366FF") {
  DBI::dbExecute(
    project$connection,
    "INSERT INTO codes (name, description, color) VALUES (?, ?, ?)",
    params = list(name, description, color)
  )
  invisible(project)
}

#' Apply a code to a text selection
#'
#' Records that the segment of a document between `start` and `end` (character
#' positions) has been coded with a given code.
#'
#' @inheritParams qual_import_text
#' @param doc_id Id of the document, as returned by [qual_get_documents()].
#' @param code_id Id of the code, as returned by [qual_get_codes()].
#' @param start,end First and last character position of the coded segment.
#' @param text The coded text itself.
#'
#' @return The `project`, invisibly.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' txt <- tempfile(fileext = ".txt")
#' writeLines("The justice system needs reform.", txt)
#' qual_import_text(project, txt, "interview1")
#' qual_add_code(project, "justice")
#' qual_apply_code(project, doc_id = 1, code_id = 1, start = 5, end = 18,
#'                 text = "justice system")
#' qual_get_codings(project)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_apply_code <- function(project, doc_id, code_id, start, end, text) {
  DBI::dbExecute(
    project$connection,
    "INSERT INTO codings (doc_id, code_id, start, end, text) VALUES (?, ?, ?, ?, ?)",
    params = list(doc_id, code_id, start, end, text)
  )
  invisible(project)
}

#' Retrieve coded segments
#'
#' @inheritParams qual_import_text
#' @param code_id Optional. Only return segments coded with this code.
#' @param doc_id Optional. Only return segments from this document. If both
#'   `code_id` and `doc_id` are given, segments must match both.
#'
#' @return A data frame with one row per coded segment and the columns
#'   `document`, `code`, `text`, `start` and `end`.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' qual_get_codings(project)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_get_codings <- function(project, code_id = NULL, doc_id = NULL) {
  query <- "
    SELECT d.name AS document, c.name AS code, cod.text, cod.start, cod.end
    FROM codings cod
    JOIN documents d ON cod.doc_id = d.id
    JOIN codes c ON cod.code_id = c.id
  "

  # Filters are passed as bound parameters rather than pasted into the SQL
  conditions <- character()
  params <- list()
  if (!is.null(code_id)) {
    conditions <- c(conditions, "cod.code_id = ?")
    params <- c(params, list(code_id))
  }
  if (!is.null(doc_id)) {
    conditions <- c(conditions, "cod.doc_id = ?")
    params <- c(params, list(doc_id))
  }
  if (length(conditions) > 0) {
    query <- paste(query, "WHERE", paste(conditions, collapse = " AND "))
  }

  DBI::dbGetQuery(
    project$connection, query,
    params = if (length(params) > 0) params else NULL
  )
}
