is_pkg <- function(path) {
  dir.exists(path) && file.exists(file.path(path, "DESCRIPTION"))
}
