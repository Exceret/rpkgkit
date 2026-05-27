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
