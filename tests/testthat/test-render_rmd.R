test_that("render_rmd aborts when rmarkdown is not installed", {
  local_mocked_bindings(
    check_installed = function(...) cli::cli_abort("not installed"),
    .package = "rlang"
  )
  expect_error(render_rmd("file.Rmd"), "not installed")
})

test_that("render_rmd aborts when path is NULL and rstudioapi is not installed", {
  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )
  local_mocked_bindings(
    is_installed = function(pkg) FALSE,
    .package = "rlang"
  )
  expect_error(render_rmd(), "path.*is required")
})

test_that("render_rmd aborts when rstudioapi returns NULL path", {
  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    .package = "rlang"
  )
  local_mocked_bindings(
    getActiveDocumentContext = function() list(path = NULL),
    .package = "rstudioapi"
  )
  expect_error(render_rmd(), "path.*is required")
})

test_that("render_rmd uses rstudioapi path when path is NULL and rstudioapi available", {
  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    .package = "rlang"
  )
  local_mocked_bindings(
    getActiveDocumentContext = function() list(path = "/project/doc.Rmd"),
    .package = "rstudioapi"
  )

  captured <- NULL
  local_mocked_bindings(
    render = function(input, ...) {
      captured <<- input
      "output.md"
    },
    .package = "rmarkdown"
  )

  result <- render_rmd()
  expect_equal(captured, "/project/doc.Rmd")
  expect_equal(result, "output.md")
})

test_that("render_rmd with explicit path calls rmarkdown::render", {
  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )

  captured <- NULL
  local_mocked_bindings(
    render = function(input, ...) {
      captured <<- input
      "output.md"
    },
    .package = "rmarkdown"
  )

  result <- render_rmd(path = "/project/doc.Rmd")
  expect_equal(captured, "/project/doc.Rmd")
  expect_equal(result, "output.md")
})

test_that("render_rmd forwards extra arguments via ... to rmarkdown::render", {
  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )

  captured_args <- NULL
  local_mocked_bindings(
    render = function(input, ...) {
      captured_args <<- list(...)
      "output.md"
    },
    .package = "rmarkdown"
  )

  render_rmd(path = "doc.Rmd", quiet = TRUE, envir = globalenv())
  expect_true(captured_args$quiet)
  expect_identical(captured_args$envir, globalenv())
})

test_that("render_rmd passes custom output_format to rmarkdown::render", {
  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )

  captured_format <- NULL
  local_mocked_bindings(
    render = function(input, output_format, ...) {
      captured_format <<- output_format
      "output.md"
    },
    .package = "rmarkdown"
  )

  render_rmd(path = "doc.Rmd", output_format = "pdf_document")
  expect_equal(captured_format, "pdf_document")
})
