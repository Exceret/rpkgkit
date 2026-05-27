test_that("is_pkg returns FALSE when path is not a directory", {
  tmp <- tempfile("not_a_dir")
  expect_false(rpkgkit:::is_pkg(tmp))
})

test_that("is_pkg returns FALSE when directory has no DESCRIPTION file", {
  tmp <- tempfile("empty_dir")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  expect_false(rpkgkit:::is_pkg(tmp))
})

test_that("is_pkg returns TRUE when directory contains DESCRIPTION file", {
  tmp <- tempfile("pkg_dir")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  file.create(file.path(tmp, "DESCRIPTION"))

  expect_true(rpkgkit:::is_pkg(tmp))
})
