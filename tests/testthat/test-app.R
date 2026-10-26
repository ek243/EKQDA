app_dir <- function() system.file("shiny-app", package = "EKQDA")

# Load the app's server function into an environment that can see shiny
load_server <- function() {
  env <- new.env(parent = asNamespace("shiny"))
  sys.source(file.path(app_dir(), "server.R"), envir = env)
  env$server
}

test_that("the Shiny app files are installed and parse", {
  expect_true(nzchar(app_dir()))
  for (file in c("ui.R", "server.R")) {
    expect_no_error(parse(file.path(app_dir(), file)))
  }
})

test_that("the app stops with a helpful message when launched without a project", {
  skip_if_not_installed("shiny")
  withr::local_options(ekqda.project = NULL)

  expect_error(shiny::testServer(load_server(), {}), "qual_launch_app")
})

test_that("the app server loads a project and renders its plots", {
  skip_if_not_installed("shiny")
  project <- demo_project()
  qual_add_code(project, "unused")
  withr::local_options(ekqda.project = project)

  shiny::testServer(load_server(), {
    session$setInputs(show_empty_codes = FALSE)
    session$flushReact()

    expect_equal(nrow(app_state$documents), 2L)
    expect_equal(nrow(app_state$codes), 4L)
    expect_equal(nrow(app_state$codings), 5L)

    expect_no_error(output$code_pie_chart)
    expect_no_error(output$cooccurrence_heatmap)
    expect_no_error(output$code_frequency_plot)

    # Codes without any coding appear only when asked for
    expect_false("unused" %in% analysis_state$code_frequency$code_name)
    session$setInputs(show_empty_codes = TRUE)
    expect_true("unused" %in% analysis_state$code_frequency$code_name)
    expect_no_error(output$code_frequency_plot)
    expect_no_error(output$code_frequency_table)
  })
})

test_that("code retrieval and the Word and text exports work", {
  skip_if_not_installed("shiny")
  withr::local_options(ekqda.project = demo_project())

  shiny::testServer(load_server(), {
    session$setInputs(show_empty_codes = FALSE)
    session$flushReact()

    session$setInputs(retrieval_code = "1", max_retrieval = 100)
    session$setInputs(run_retrieval = 1)
    expect_equal(nrow(analysis_state$retrieved_codings), 2L)

    docx <- output$download_retrieval_docx
    expect_gt(file.size(docx), 0)
    docx_text <- officer::docx_summary(officer::read_docx(docx))$text
    expect_true(any(grepl("Segment 1", docx_text)))
    expect_true(any(grepl("Justice", docx_text)))

    txt <- readLines(output$download_retrieval_txt)
    expect_true(any(grepl("CODE RETRIEVAL REPORT", txt)))
    expect_true(any(grepl("SEGMENT 2 of 2", txt)))

    freq_docx <- output$download_freq_docx
    freq_text <- officer::docx_summary(officer::read_docx(freq_docx))$text
    expect_true("justice" %in% freq_text)
  })
})
