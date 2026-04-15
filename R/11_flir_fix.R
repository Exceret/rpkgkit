#' @export
flir_fix <- function(path, ...) {
  path <- if (is.null(path) && rlang::is_installed("rstudioapi")) {
    rstudioapi::getActiveDocumentContext()$path
  } else {
    cli::cli_abort(("c" = "{.arg path} is required"))
  }

  if (file.exists(path)) {
    flir::fix(path = path, ...)
  } else if (is_pkg(path)) {
    flir::fix_package(path = path, ...)
  } else if (dir.exists(path)) {
    flir::fix_dir(path = path, ...)
  }
}
