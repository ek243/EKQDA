#' Launch the EKQDA Shiny app
#'
#' Opens the interactive coding interface for a project. The app lets you add
#' and read documents, create codes, code text selections, write memos, and
#' retrieve and export coded segments.
#'
#' @param project_path Path to the SQLite project database. The file is
#'   created if it does not exist. If `NULL` (the default), a temporary
#'   project is used and discarded when the app closes.
#' @param launch.browser Passed to [shiny::runApp()]. Defaults to opening a
#'   browser in interactive sessions.
#'
#' @return Called for its side effect of running the app; returns `NULL`
#'   invisibly once the app has been stopped.
#'
#' @examples
#' if (interactive()) {
#'   qual_launch_app()
#' }
#' @export
qual_launch_app <- function(project_path = NULL, launch.browser = interactive()) {
  if (is.null(project_path)) {
    project_path <- tempfile(fileext = ".sqlite")
  }
  project <- qual_init(project_path)

  app_dir <- system.file("shiny-app", package = "EKQDA")
  if (!nzchar(app_dir)) {
    DBI::dbDisconnect(project$connection)
    stop("Could not find the Shiny app directory. Try re-installing `EKQDA`.",
         call. = FALSE)
  }

  # The app reads the project from an option instead of from the global
  # environment, and the option is restored when the app stops.
  old_options <- options(ekqda.project = project)
  on.exit({
    options(old_options)
    DBI::dbDisconnect(project$connection)
  }, add = TRUE)

  # shiny::runApp() attaches shiny with require(). If the version of shiny on
  # disk differs from the one already loaded in this session (for example
  # after shiny was updated while R was running), that tries to unload the
  # loaded copy and fails when other loaded packages, such as miniUI, import
  # it. Attaching the already-loaded namespace first avoids this.
  if (!"package:shiny" %in% search()) {
    attachNamespace("shiny")
  }

  shiny::runApp(app_dir, launch.browser = launch.browser)
  invisible(NULL)
}

# The Shiny app in inst/shiny-app uses these packages. They are listed in
# Imports so that they are installed together with EKQDA; referring to them
# here lets R CMD check see that they are used.
ignore_unused_imports <- function() {
  DT::datatable
  dplyr::select
  ggplot2::ggplot
  officer::read_docx
  shinyjs::useShinyjs
  shinythemes::shinytheme
}
