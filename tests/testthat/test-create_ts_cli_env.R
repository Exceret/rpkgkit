test_that("create_ts_cli_env returns an environment", {
  env <- rpkgkit:::create_ts_cli_env("cli_alert_info")
  expect_type(env, "environment")
})

test_that("create_ts_cli_env with default list contains all 4 functions", {
  env <- rpkgkit:::create_ts_cli_env()
  expected <- c(
    "cli_alert_info",
    "cli_alert_success",
    "cli_alert_warning",
    "cli_alert_danger"
  )
  expect_true(all(expected %in% ls(env)))
})

test_that("create_ts_cli_env with custom list only contains requested functions", {
  env <- rpkgkit:::create_ts_cli_env(c("cli_alert_info", "cli_alert_danger"))
  expect_true("cli_alert_info" %in% ls(env))
  expect_true("cli_alert_danger" %in% ls(env))
  expect_false("cli_alert_success" %in% ls(env))
})

test_that("create_ts_cli_env with empty character vector returns empty env", {
  env <- rpkgkit:::create_ts_cli_env(character(0))
  expect_length(ls(env), 0L)
})

test_that("create_ts_cli_env wraps functions with add_timestamp_to_cli", {
  captured_func <- NULL
  local_mocked_bindings(
    exists = function(x, envir) TRUE,
    .package = "base"
  )
  local_mocked_bindings(
    get0 = function(x, envir) function(...) cat("[mock cli]: ...\n"),
    .package = "base"
  )
  local_mocked_bindings(
    add_timestamp_to_cli = function(cli_func) {
      captured_func <<- cli_func
      function(...) cli_func(...)
    },
    .package = "rpkgkit"
  )

  env <- rpkgkit:::create_ts_cli_env("cli_alert_info")
  expect_true("cli_alert_info" %in% ls(env))
})
