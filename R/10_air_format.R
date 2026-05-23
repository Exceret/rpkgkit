#' Format R code using air
#'
#' @param path Path to the R file to format. If NULL, attempts to use the active
#'   document in RStudio (requires `rstudioapi` package).
#' @param ... Additional arguments passed to `system2()`.
#'
#' @details
#' Install [air](https://github.com/posit-dev/air):
#'
#' Linux: `curl -LsSf https://github.com/posit-dev/air/releases/latest/download/air-installer.sh | sh`
#' Windows: `powershell -ExecutionPolicy Bypass -c "irm https://github.com/posit-dev/air/releases/latest/download/air-installer.ps1 | iex"`
#' uv: `uv tool install air-formatter`
#' brew: `brew install air`
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
