#' Format R code using air
#'
#' @param path Path to the R file to format. If NULL, attempts to use the active
#'   document in RStudio (requires `rstudioapi` package).
#' @param ... Additional arguments passed to `system2()`.
#'
#' @return The exit status of the `air format` command (invisibly).
#'
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
