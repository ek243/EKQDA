test_that("qual_code_frequency counts applications and keeps unused codes", {
  project <- demo_project()
  qual_add_code(project, "unused")

  freq <- qual_code_frequency(project)

  expect_named(freq, c("name", "frequency"))
  expect_equal(freq$frequency[freq$name == "justice"], 2)
  expect_equal(freq$frequency[freq$name == "equality"], 1)
  expect_equal(freq$frequency[freq$name == "fairness"], 2)
  expect_equal(freq$frequency[freq$name == "unused"], 0)
  expect_false(is.unsorted(rev(freq$frequency)))  # most frequent first
})

test_that("qual_code_cooccurrence at document level counts shared documents", {
  cooc <- qual_code_cooccurrence(demo_project())

  expected <- matrix(
    c(1L, 1L, 1L,
      1L, 2L, 2L,
      1L, 2L, 2L),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("equality", "fairness", "justice"),
                    c("equality", "fairness", "justice"))
  )
  expect_equal(cooc, expected)
  expect_true(isSymmetric(cooc))
})

test_that("qual_code_cooccurrence at segment level counts overlapping segments", {
  cooc <- qual_code_cooccurrence(demo_project(), level = "segment")

  expected <- matrix(
    c(1L, 1L, 0L,
      1L, 2L, 1L,
      0L, 1L, 2L),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("equality", "fairness", "justice"),
                    c("equality", "fairness", "justice"))
  )
  expect_equal(cooc, expected)
})

test_that("qual_code_cooccurrence handles projects with no codes or no codings", {
  expect_equal(dim(qual_code_cooccurrence(local_project())), c(0L, 0L))

  project <- local_project()
  qual_add_code(project, "a")
  qual_add_code(project, "b")
  cooc <- qual_code_cooccurrence(project)
  expect_equal(dim(cooc), c(2L, 2L))
  expect_true(all(cooc == 0L))
})

test_that("qual_code_cooccurrence rejects unknown levels", {
  expect_error(qual_code_cooccurrence(local_project(), level = "paragraph"))
})
