#' Make function calls explicit by adding double colons
#'
#' @description
#' This function reads an R file and adds explicit package namespace qualifiers
#' (double colons) to function calls using the `pedant` package. If no path is
#' provided, it attempts to use the currently active file in RStudio.
#'
#' @param path A character string specifying the path to the R file to process.
#'   If `NULL` and RStudio is available, the active document path is used.
#' @param ... Additional arguments (currently unused).
#'
#' @return Invisible `NULL`. The function modifies the file in place.
#'
#' @details
#' The function requires the `pedant` package to be installed. It reads the file,
#' adds double colons to function calls to make package dependencies explicit,
#' and writes the modified code back to the same file.
#'
#' @examples
#' \dontrun{
#' make_func_call_explicit("path/to/file.R")
#' make_func_call_explicit()  # Uses active RStudio file
#' }
#'
#' @export
make_func_call_explicit <- function(
  path = NULL,
  use_packages = pedant::current_packages(),
  ignore_functions = pedant::imported_functions(),
  ...
) {
  rlang::check_dots_empty0()
  rlang::check_installed("pedant")
  path <- if (is.null(path) && rlang::is_installed("rstudioapi")) {
    rstudioapi::getActiveDocumentContext()$path
  } else {
    cli::cli_abort(("c" = "{.arg path} is required"))
  }

  cli::cli_alert_info("Retrieving function calls from {.pkg {use_packages}}")
  formated_code <- pedant::add_double_colons(
    code = readLines(path),
    use_packages = use_packages,
    ignore_functions = ignore_functions
  )
  writeLines(formated_code, path)
  cli::cli_alert_success(
    "Successfully made function call explicit in {.file {path}}"
  )
}
