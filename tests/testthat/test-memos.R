add_memo <- function(project, type, target_id, title, content) {
  DBI::dbExecute(
    project$connection,
    "INSERT INTO memos (type, target_id, title, content) VALUES (?, ?, ?, ?)",
    params = list(type, target_id, title, content)
  )
}

test_that("memo getters return only memos of the requested kind", {
  project <- local_project()
  add_memo(project, "code", 1, "Code memo", "About code 1")
  add_memo(project, "code", 2, "Other code memo", "About code 2")
  add_memo(project, "document", 1, "Document memo", "About doc 1")
  add_memo(project, "general", NA, "General memo", "About the project")

  expect_equal(qual_get_code_memos(project, 1)$title, "Code memo")
  expect_equal(qual_get_document_memos(project, 1)$title, "Document memo")
  expect_equal(qual_get_general_memos(project)$title, "General memo")
})

test_that("memo getters return empty data frames when there are no memos", {
  project <- local_project()
  expect_equal(nrow(qual_get_code_memos(project, 1)), 0L)
  expect_equal(nrow(qual_get_document_memos(project, 1)), 0L)
  expect_equal(nrow(qual_get_general_memos(project)), 0L)
})
