# An empty project in a temporary database, closed when the test finishes.
local_project <- function(env = parent.frame()) {
  project <- qual_init(tempfile(fileext = ".sqlite"))
  withr::defer(DBI::dbDisconnect(project$connection), envir = env)
  project
}

# A small project used by several tests.
#
#   doc 1: "Justice and equality matter."   doc 2: "Justice requires fairness."
#   codes: justice (1), equality (2), fairness (3)
#
#   doc 1: justice 1-7, equality 13-20, fairness 1-20 (overlaps both)
#   doc 2: justice 1-7, fairness 18-25
demo_project <- function(env = parent.frame()) {
  project <- local_project(env)

  doc1 <- tempfile(fileext = ".txt")
  doc2 <- tempfile(fileext = ".txt")
  writeLines("Justice and equality matter.", doc1)
  writeLines("Justice requires fairness.", doc2)
  qual_import_text(project, doc1, "doc1")
  qual_import_text(project, doc2, "doc2")

  qual_add_code(project, "justice", "About justice", "#FF0000")
  qual_add_code(project, "equality")
  qual_add_code(project, "fairness")

  qual_apply_code(project, 1, 1, 1, 7, "Justice")
  qual_apply_code(project, 1, 2, 13, 20, "equality")
  qual_apply_code(project, 1, 3, 1, 20, "Justice and equality")
  qual_apply_code(project, 2, 1, 1, 7, "Justice")
  qual_apply_code(project, 2, 3, 18, 25, "fairness")

  project
}
