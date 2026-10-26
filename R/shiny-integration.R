#' Get a project summary
#'
#' @inheritParams qual_import_text
#'
#' @return A list with the number of `documents`, `codes` and `codings` in the
#'   project.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' qual_project_summary(project)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_project_summary <- function(project) {
  list(
    documents = DBI::dbGetQuery(project$connection, "SELECT COUNT(*) AS n FROM documents")$n,
    codes = DBI::dbGetQuery(project$connection, "SELECT COUNT(*) AS n FROM codes")$n,
    codings = DBI::dbGetQuery(project$connection, "SELECT COUNT(*) AS n FROM codings")$n
  )
}

#' List the documents in a project
#'
#' @inheritParams qual_import_text
#'
#' @return A data frame with the columns `id` and `name`, sorted by name.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' qual_get_documents(project)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_get_documents <- function(project) {
  DBI::dbGetQuery(project$connection, "SELECT id, name FROM documents ORDER BY name")
}

#' List the codes in a project
#'
#' @inheritParams qual_import_text
#'
#' @return A data frame with the columns `id`, `name` and `color`, sorted by
#'   name.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' qual_get_codes(project)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_get_codes <- function(project) {
  DBI::dbGetQuery(project$connection, "SELECT id, name, color FROM codes ORDER BY name")
}
