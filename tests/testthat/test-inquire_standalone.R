test_that("inquire_standalone filters standalone files from gh response", {
  mock_response <- list(
    list(name = "standalone-utils.R", some_field = "a"),
    list(name = "standalone-io.R", some_field = "b"),
    list(name = "utils.R", some_field = "c"),
    list(name = "standalone-plot.R", some_field = "d"),
    list(name = "main.R", some_field = "e")
  )

  local_mocked_bindings(
    gh = function(endpoint, ...) mock_response,
    .package = "gh"
  )

  # Capture the list that lapply returns before bind_rows processes it
  standalone_list <- NULL
  local_mocked_bindings(
    bind_rows = function(...) {
      standalone_list <<- ..1
      non_null <- Filter(Negate(is.null), ..1)
      if (length(non_null) == 0L) data.frame() else as.data.frame(do.call(rbind, non_null))
    },
    .package = "dplyr"
  )

  result <- inquire_standalone("owner", "repo")

  # standalone_list should have 5 entries
  expect_length(standalone_list, 5L)
  # Non-standalone entries should be NULL
  expect_null(standalone_list[[3L]])   # utils.R
  expect_null(standalone_list[[5L]])   # main.R
  # Standalone entries preserved
  expect_equal(standalone_list[[1L]]$name, "standalone-utils.R")
  expect_equal(standalone_list[[2L]]$name, "standalone-io.R")
  expect_equal(standalone_list[[4L]]$name, "standalone-plot.R")
  # Final result has 3 rows
  expect_equal(nrow(result), 3L)
  expect_true(all(grepl("^standalone-", result$name)))
})

test_that("inquire_standalone returns empty when no standalone files", {
  mock_response <- list(
    list(name = "utils.R"),
    list(name = "main.R"),
    list(name = "plot.R")
  )

  local_mocked_bindings(
    gh = function(endpoint, ...) mock_response,
    .package = "gh"
  )

  standalone_list <- NULL
  local_mocked_bindings(
    bind_rows = function(...) {
      standalone_list <<- ..1
      non_null <- Filter(Negate(is.null), ..1)
      if (length(non_null) == 0L) data.frame() else as.data.frame(do.call(rbind, non_null))
    },
    .package = "dplyr"
  )

  result <- inquire_standalone("owner", "repo")

  expect_length(standalone_list, 3L)
  expect_true(all(sapply(standalone_list, is.null)))
  expect_equal(nrow(result), 0L)
})

test_that("inquire_standalone builds owner/repo when repo has no slash", {
  mock_response <- list(list(name = "standalone-test.R"))

  gh_calls <- list()
  local_mocked_bindings(
    gh = function(endpoint, repo_spec, ...) {
      gh_calls <<- append(gh_calls, list(list(
        endpoint = endpoint,
        repo_spec = repo_spec
      )))
      mock_response
    },
    .package = "gh"
  )

  local_mocked_bindings(
    bind_rows = function(...) as.data.frame(do.call(rbind, Filter(Negate(is.null), list(...)))),
    .package = "dplyr"
  )

  inquire_standalone("myuser", "myrepo")

  expect_length(gh_calls, 1L)
  expect_equal(gh_calls[[1L]]$repo_spec, "myuser/myrepo")
})

test_that("inquire_standalone uses repo as-is when it contains a slash", {
  mock_response <- list(list(name = "standalone-test.R"))

  gh_calls <- list()
  local_mocked_bindings(
    gh = function(endpoint, repo_spec, ...) {
      gh_calls <<- append(gh_calls, list(list(
        endpoint = endpoint,
        repo_spec = repo_spec
      )))
      mock_response
    },
    .package = "gh"
  )

  local_mocked_bindings(
    bind_rows = function(...) as.data.frame(do.call(rbind, Filter(Negate(is.null), list(...)))),
    .package = "dplyr"
  )

  inquire_standalone("ignored", "actual-owner/actual-repo")

  expect_length(gh_calls, 1L)
  expect_equal(gh_calls[[1L]]$repo_spec, "actual-owner/actual-repo")
})

test_that("inquire_standalone passes correct endpoint to gh::gh", {
  mock_response <- list(list(name = "standalone-test.R"))

  gh_calls <- list()
  local_mocked_bindings(
    gh = function(endpoint, ...) {
      gh_calls <<- append(gh_calls, list(list(endpoint = endpoint, ...)))
      mock_response
    },
    .package = "gh"
  )

  local_mocked_bindings(
    bind_rows = function(...) as.data.frame(do.call(rbind, Filter(Negate(is.null), list(...)))),
    .package = "dplyr"
  )

  inquire_standalone("owner", "repo")

  expect_length(gh_calls, 1L)
  expect_equal(gh_calls[[1L]]$endpoint, "/repos/{repo_spec}/contents/R")
  expect_equal(gh_calls[[1L]]$.accept, "application/vnd.github.v3.raw")
})
