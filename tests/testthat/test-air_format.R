test_that("air_format aborts when path is provided explicitly", {
  # Current behavior: passing a non-NULL path aborts because
  # the condition only handles NULL path + rstudioapi availability
  expect_error(
    air_format("test.R"),
    "is required"
  )
})

test_that("air_format aborts when path is NULL and rstudioapi unavailable", {
  local_mocked_bindings(
    is_installed = function(pkg) FALSE,
    .package = "rlang"
  )

  expect_error(
    air_format(),
    "is required"
  )
})

test_that("air_format calls system2 with active document path when path is NULL", {
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    .package = "rlang"
  )

  local_mocked_bindings(
    getActiveDocumentContext = function() {
      list(path = "/home/user/project/R/my_script.R")
    },
    .package = "rstudioapi"
  )

  system2_calls <- list()
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      system2_calls <<- append(
        system2_calls,
        list(list(
          command = command,
          args = args,
          dots = list(...)
        ))
      )
      0L
    },
    .package = "base"
  )

  result <- air_format()

  expect_length(system2_calls, 1L)
  expect_equal(system2_calls[[1L]]$command, "air")
  expect_equal(
    system2_calls[[1L]]$args,
    c("format", "/home/user/project/R/my_script.R")
  )
  expect_equal(result, 0L)
})

test_that("air_format passes additional arguments to system2", {
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    .package = "rlang"
  )

  local_mocked_bindings(
    getActiveDocumentContext = function() list(path = "/dummy/file.R"),
    .package = "rstudioapi"
  )

  system2_calls <- list()
  local_mocked_bindings(
    system2 = function(command, args, ...) {
      system2_calls <<- append(
        system2_calls,
        list(list(
          command = command,
          args = args,
          dots = list(...)
        ))
      )
      0L
    },
    .package = "base"
  )

  air_format(stdout = TRUE, stderr = TRUE)

  expect_length(system2_calls, 1L)
  expect_equal(system2_calls[[1L]]$dots$stdout, TRUE)
  expect_equal(system2_calls[[1L]]$dots$stderr, TRUE)
})

test_that("air_format returns the exit status from system2", {
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    .package = "rlang"
  )

  local_mocked_bindings(
    getActiveDocumentContext = function() list(path = "/dummy/file.R"),
    .package = "rstudioapi"
  )

  local_mocked_bindings(
    system2 = function(...) 42L,
    .package = "base"
  )

  expect_equal(air_format(), 42L)
})
