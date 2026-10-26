#' Get memos for a specific code
#'
#' @inheritParams qual_import_text
#' @param code_id Id of the code, as returned by [qual_get_codes()].
#'
#' @return A data frame of memos attached to the code, most recently updated
#'   first.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' qual_get_code_memos(project, code_id = 1)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_get_code_memos <- function(project, code_id) {
  DBI::dbGetQuery(
    project$connection,
    "SELECT * FROM memos WHERE type = 'code' AND target_id = ? ORDER BY updated DESC",
    params = list(code_id)
  )
}

#' Get memos for a specific document
#'
#' @inheritParams qual_import_text
#' @param doc_id Id of the document, as returned by [qual_get_documents()].
#'
#' @return A data frame of memos attached to the document, most recently
#'   updated first.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' qual_get_document_memos(project, doc_id = 1)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_get_document_memos <- function(project, doc_id) {
  DBI::dbGetQuery(
    project$connection,
    "SELECT * FROM memos WHERE type = 'document' AND target_id = ? ORDER BY updated DESC",
    params = list(doc_id)
  )
}

#' Get all general memos
#'
#' General memos are not attached to a particular code or document.
#'
#' @inheritParams qual_import_text
#'
#' @return A data frame of general memos, most recently updated first.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' qual_get_general_memos(project)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_get_general_memos <- function(project) {
  DBI::dbGetQuery(
    project$connection,
    "SELECT * FROM memos WHERE type = 'general' ORDER BY updated DESC"
  )
}
