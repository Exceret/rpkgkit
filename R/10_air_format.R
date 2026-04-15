#' @export
air_format <- function(path = NULL, ...) {
  path <- if (is.null(path) && rlang::is_installed("rstudioapi")) {
    rstudioapi::getActiveDocumentContext()$path
  } else {
    cli::cli_abort(("c" = "{.arg path} is required"))
  }

  system2(
    command = "air",
    args = c(
      "format",
      path
    ),
    ...
  )
}
