test_that("qual_init creates the expected tables", {
  project <- local_project()

  expect_s3_class(project, "qual_project")
  tables <- DBI::dbListTables(project$connection)
  expect_true(all(c("documents", "codes", "codings", "memos") %in% tables))
})

test_that("qual_init can reopen an existing project without losing data", {
  path <- tempfile(fileext = ".sqlite")
  first <- qual_init(path)
  qual_add_code(first, "justice")
  DBI::dbDisconnect(first$connection)

  second <- qual_init(path)
  withr::defer(DBI::dbDisconnect(second$connection))
  expect_equal(qual_get_codes(second)$name, "justice")
})

test_that("qual_import_text stores content and defaults the name to the file name", {
  project <- local_project()
  txt <- file.path(tempdir(), "interview_a.txt")
  writeLines("Some interview text.", txt)

  qual_import_text(project, txt)
  qual_import_text(project, txt, name = "custom name")

  docs <- qual_get_documents(project)
  expect_setequal(docs$name, c("interview_a.txt", "custom name"))

  content <- DBI::dbGetQuery(project$connection, "SELECT content FROM documents")$content
  expect_true(all(grepl("Some interview text.", content, fixed = TRUE)))
})

test_that("qual_project_summary counts documents, codes and codings", {
  expect_equal(
    qual_project_summary(local_project()),
    list(documents = 0L, codes = 0L, codings = 0L)
  )
  expect_equal(
    qual_project_summary(demo_project()),
    list(documents = 2L, codes = 3L, codings = 5L)
  )
})
