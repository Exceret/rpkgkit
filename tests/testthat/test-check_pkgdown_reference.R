test_that("check_pkgdown_reference aborts when _pkgdown.yml not found", {
  tmp <- tempfile("pkg_test")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  expect_error(
    check_pkgdown_reference(tmp),
    "_pkgdown.yml"
  )
})

test_that("check_pkgdown_reference aborts when NAMESPACE not found", {
  tmp <- tempfile("pkg_test")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.create(file.path(tmp, "_pkgdown.yml"))

  expect_error(
    check_pkgdown_reference(tmp),
    "NAMESPACE"
  )
})

test_that("check_pkgdown_reference warns when reference section is missing", {
  tmp <- tempfile("pkg_test")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.create(file.path(tmp, "_pkgdown.yml"))
  file.create(file.path(tmp, "NAMESPACE"))

  local_mocked_bindings(
    readLines = function(f, ...) c("export(myfunc)"),
    .package = "base"
  )

  local_mocked_bindings(
    read_yaml = function(file, ...) list(title = "My Package"),
    .package = "yaml"
  )

  expect_message(
    result <- check_pkgdown_reference(tmp),
    "no.*reference.*section"
  )
  expect_null(result)
})

test_that("check_pkgdown_reference returns empty when all exported are listed (flat contents)", {
  tmp <- tempfile("pkg_test")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.create(file.path(tmp, "_pkgdown.yml"))
  file.create(file.path(tmp, "NAMESPACE"))

  local_mocked_bindings(
    readLines = function(f, ...) c("export(foo)", "export(bar)", "export(baz)"),
    .package = "base"
  )

  local_mocked_bindings(
    read_yaml = function(file, ...) {
      list(reference = list(contents = c("foo", "bar", "baz")))
    },
    .package = "yaml"
  )

  result <- check_pkgdown_reference(tmp)
  expect_equal(result, character())
})

test_that("check_pkgdown_reference detects missing functions (flat contents)", {
  tmp <- tempfile("pkg_test")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.create(file.path(tmp, "_pkgdown.yml"))
  file.create(file.path(tmp, "NAMESPACE"))

  local_mocked_bindings(
    readLines = function(f, ...) {
      c("export(foo)", "export(bar)", "export(missing_func)")
    },
    .package = "base"
  )

  local_mocked_bindings(
    read_yaml = function(file, ...) {
      list(reference = list(contents = c("foo", "bar")))
    },
    .package = "yaml"
  )

  result <- check_pkgdown_reference(tmp)
  expect_equal(result, "missing_func")
})

test_that("check_pkgdown_reference extracts from list-of-sections reference format", {
  tmp <- tempfile("pkg_test")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.create(file.path(tmp, "_pkgdown.yml"))
  file.create(file.path(tmp, "NAMESPACE"))

  local_mocked_bindings(
    readLines = function(f, ...) {
      c(
        "export(func_a)",
        "export(func_b)",
        "export(func_c)",
        "export(func_d)"
      )
    },
    .package = "base"
  )

  local_mocked_bindings(
    read_yaml = function(file, ...) {
      list(
        reference = list(
          list(title = "Group 1", contents = c("func_a", "func_b")),
          list(title = "Group 2", contents = c("func_c"))
        )
      )
    },
    .package = "yaml"
  )

  result <- check_pkgdown_reference(tmp)
  expect_equal(result, "func_d")
})

test_that("check_pkgdown_reference returns empty when all listed (list-of-sections)", {
  tmp <- tempfile("pkg_test")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.create(file.path(tmp, "_pkgdown.yml"))
  file.create(file.path(tmp, "NAMESPACE"))

  local_mocked_bindings(
    readLines = function(f, ...) c("export(foo)", "export(bar)"),
    .package = "base"
  )

  local_mocked_bindings(
    read_yaml = function(file, ...) {
      list(
        reference = list(
          list(title = "Core", contents = c("foo")),
          list(title = "Utils", contents = c("bar"))
        )
      )
    },
    .package = "yaml"
  )

  result <- check_pkgdown_reference(tmp)
  expect_equal(result, character())
})

test_that("check_pkgdown_reference handles empty NAMESPACE (no exports)", {
  tmp <- tempfile("pkg_test")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.create(file.path(tmp, "_pkgdown.yml"))
  file.create(file.path(tmp, "NAMESPACE"))

  local_mocked_bindings(
    readLines = function(f, ...) {
      c("# This namespace is empty", "import(stats)")
    },
    .package = "base"
  )

  local_mocked_bindings(
    read_yaml = function(file, ...) list(reference = list(contents = c("foo"))),
    .package = "yaml"
  )

  result <- check_pkgdown_reference(tmp)
  expect_equal(result, character())
})

test_that("check_pkgdown_reference supports explicit pkg path", {
  tmp <- tempfile("pkg_test")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.create(file.path(tmp, "_pkgdown.yml"))
  file.create(file.path(tmp, "NAMESPACE"))

  local_mocked_bindings(
    readLines = function(f, ...) c("export(myfunc)"),
    .package = "base"
  )

  local_mocked_bindings(
    read_yaml = function(file, ...) {
      list(reference = list(contents = c("myfunc")))
    },
    .package = "yaml"
  )

  result <- check_pkgdown_reference(tmp)
  expect_equal(result, character())
})

test_that("check_pkgdown_reference parses NAMESPACE export syntax correctly", {
  tmp <- tempfile("pkg_test")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  file.create(file.path(tmp, "_pkgdown.yml"))
  file.create(file.path(tmp, "NAMESPACE"))

  local_mocked_bindings(
    readLines = function(f, ...) {
      c(
        'export(foo)',
        'export(bar)  # some comment',
        'export(baz)',
        'exportPattern("^[^\\.]")'
      )
    },
    .package = "base"
  )

  local_mocked_bindings(
    read_yaml = function(file, ...) {
      list(reference = list(contents = c("foo", "bar", "baz")))
    },
    .package = "yaml"
  )

  result <- check_pkgdown_reference(tmp)
  expect_equal(result, character())
})
