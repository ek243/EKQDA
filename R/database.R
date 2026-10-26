#' Initialize a qualitative project database
#'
#' Creates (or opens) an SQLite database that stores the documents, codes,
#' coded segments and memos of a qualitative project. Existing databases are
#' opened unchanged; missing tables are created.
#'
#' @param path Path to the SQLite database file. Defaults to
#'   `"qual_project.sqlite"` in the working directory.
#'
#' @return A `qual_project` object: a list with the database `connection`
#'   and the `path`. Close the connection with
#'   `DBI::dbDisconnect(project$connection)` when you are done.
#'
#' @examples
#' path <- tempfile(fileext = ".sqlite")
#' project <- qual_init(path)
#' qual_project_summary(project)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_init <- function(path = "qual_project.sqlite") {
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)

  # Create core tables
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS documents (
      id INTEGER PRIMARY KEY,
      name TEXT,
      content TEXT,
      created TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS codes (
      id INTEGER PRIMARY KEY,
      name TEXT,
      description TEXT,
      color TEXT,
      created TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS codings (
      id INTEGER PRIMARY KEY,
      doc_id INTEGER,
      code_id INTEGER,
      start INTEGER,
      end INTEGER,
      text TEXT,
      memo TEXT,
      FOREIGN KEY(doc_id) REFERENCES documents(id),
      FOREIGN KEY(code_id) REFERENCES codes(id)
    )
  ")

  # Memos can attach to a code, a document, or the project in general
  DBI::dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS memos (
      id INTEGER PRIMARY KEY,
      type TEXT NOT NULL,  -- 'code', 'document', 'general'
      target_id INTEGER,   -- code_id or doc_id, NULL for general
      title TEXT NOT NULL,
      content TEXT NOT NULL,
      created TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
      updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")

  structure(list(connection = conn, path = path), class = "qual_project")
}

#' Import a text document into a project
#'
#' Reads a plain-text file and stores its content in the project database.
#'
#' @param project A `qual_project` object created by [qual_init()].
#' @param file_path Path to a plain-text file.
#' @param name Name under which the document is stored. Defaults to the
#'   file name.
#'
#' @return The `project`, invisibly.
#'
#' @examples
#' project <- qual_init(tempfile(fileext = ".sqlite"))
#' txt <- tempfile(fileext = ".txt")
#' writeLines("Interview text goes here.", txt)
#' qual_import_text(project, txt, name = "interview1")
#' qual_get_documents(project)
#' DBI::dbDisconnect(project$connection)
#' @export
qual_import_text <- function(project, file_path, name = NULL) {
  if (is.null(name)) name <- basename(file_path)
  content <- readr::read_file(file_path)

  DBI::dbExecute(
    project$connection,
    "INSERT INTO documents (name, content) VALUES (?, ?)",
    params = list(name, content)
  )

  invisible(project)
}
