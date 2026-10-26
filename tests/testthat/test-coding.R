test_that("qual_add_code stores name, description and colour", {
  project <- local_project()
  qual_add_code(project, "justice", "About justice", "#FF0000")
  qual_add_code(project, "equality")

  codes <- qual_get_codes(project)
  expect_equal(codes$name, c("equality", "justice"))  # sorted by name
  expect_equal(codes$color[codes$name == "justice"], "#FF0000")
  expect_equal(codes$color[codes$name == "equality"], "#3366FF")  # default
})

test_that("qual_get_codings returns all segments with document and code names", {
  codings <- qual_get_codings(demo_project())

  expect_equal(nrow(codings), 5L)
  expect_named(codings, c("document", "code", "text", "start", "end"))
})

test_that("qual_get_codings filters by code, by document, and by both", {
  project <- demo_project()

  by_code <- qual_get_codings(project, code_id = 1)
  expect_equal(nrow(by_code), 2L)
  expect_true(all(by_code$code == "justice"))

  by_doc <- qual_get_codings(project, doc_id = 2)
  expect_equal(nrow(by_doc), 2L)
  expect_true(all(by_doc$document == "doc2"))

  both <- qual_get_codings(project, code_id = 1, doc_id = 2)
  expect_equal(nrow(both), 1L)
  expect_equal(both$document, "doc2")
  expect_equal(both$code, "justice")
})

test_that("qual_get_codings treats filter values as data, not as SQL", {
  project <- demo_project()
  expect_equal(nrow(qual_get_codings(project, code_id = "1 OR 1=1")), 0L)
  expect_equal(qual_project_summary(project)$codings, 5L)
})

test_that("qual_get_codings returns an empty data frame for an empty project", {
  codings <- qual_get_codings(local_project())
  expect_equal(nrow(codings), 0L)
  expect_named(codings, c("document", "code", "text", "start", "end"))
})
