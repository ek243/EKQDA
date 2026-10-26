#' Code frequency analysis
#'
#' Counts how many times each code has been applied.
#'
#' @inheritParams qual_import_text
#'
#' @return A data frame with the columns `name` and `frequency`, sorted from
#'   the most to the least frequently applied code. Codes that have not been
#'   applied are included with a frequency of zero.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' qual_add_code(project, "justice")
#' qual_code_frequency(project)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_code_frequency <- function(project) {
  query <- "
    SELECT c.name, COUNT(cod.id) AS frequency
    FROM codes c
    LEFT JOIN codings cod ON c.id = cod.code_id
    GROUP BY c.id, c.name
    ORDER BY frequency DESC
  "

  DBI::dbGetQuery(project$connection, query)
}

#' Code co-occurrence matrix
#'
#' Counts how often pairs of codes occur together.
#'
#' With `level = "document"`, two codes co-occur when both have been applied
#' somewhere in the same document; each document counts at most once per pair.
#' With `level = "segment"`, two codes co-occur when two coded segments of the
#' same document overlap in character positions; each pair of overlapping
#' segments counts once.
#'
#' The diagonal gives the number of documents (or coded segments) in which a
#' code appears, so the matrix can be turned into a normalised measure such as
#' a Jaccard index, or into a weighted network, for further analysis.
#'
#' @inheritParams qual_import_text
#' @param level Unit of co-occurrence: `"document"` (default) or `"segment"`.
#'
#' @return A symmetric integer matrix with one row and one column per code
#'   (named by code name, in alphabetical order). It has zero rows and
#'   columns if the project has no codes.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' txt <- tempfile(fileext = ".txt")
#' writeLines("Justice and equality matter.", txt)
#' qual_import_text(project, txt, "interview1")
#' qual_add_code(project, "justice")
#' qual_add_code(project, "equality")
#' qual_apply_code(project, 1, 1, 1, 7, "Justice")
#' qual_apply_code(project, 1, 2, 13, 20, "equality")
#' qual_code_cooccurrence(project)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_code_cooccurrence <- function(project, level = c("document", "segment")) {
  level <- match.arg(level)

  codes <- DBI::dbGetQuery(
    project$connection,
    "SELECT id, name FROM codes ORDER BY name, id"
  )
  codings <- DBI::dbGetQuery(
    project$connection,
    "SELECT doc_id, code_id, start, end FROM codings"
  )

  n_codes <- nrow(codes)
  result <- matrix(
    0L, nrow = n_codes, ncol = n_codes,
    dimnames = list(codes$name, codes$name)
  )

  # Ignore codings that point to a code that no longer exists
  codings <- codings[codings$code_id %in% codes$id, , drop = FALSE]
  if (n_codes == 0L || nrow(codings) == 0L) {
    return(result)
  }
  code_index <- match(codings$code_id, codes$id)

  if (level == "document") {
    doc_index <- match(codings$doc_id, unique(codings$doc_id))
    presence <- matrix(0L, nrow = max(doc_index), ncol = n_codes)
    presence[cbind(doc_index, code_index)] <- 1L
    result[] <- as.integer(crossprod(presence))
    return(result)
  }

  # Segment level: count pairs of overlapping segments within each document
  for (rows in split(seq_len(nrow(codings)), codings$doc_id)) {
    idx <- code_index[rows]
    from <- codings$start[rows]
    to <- codings$end[rows]

    for (i in seq_along(rows)) {
      result[idx[i], idx[i]] <- result[idx[i], idx[i]] + 1L
    }
    if (length(rows) < 2L) next

    overlaps <- outer(from, to, "<=") & t(outer(from, to, "<="))
    for (i in seq_len(length(rows) - 1L)) {
      for (j in (i + 1L):length(rows)) {
        if (overlaps[i, j] && idx[i] != idx[j]) {
          result[idx[i], idx[j]] <- result[idx[i], idx[j]] + 1L
          result[idx[j], idx[i]] <- result[idx[j], idx[i]] + 1L
        }
      }
    }
  }

  result
}
