#' @export
render_md <- function(path = NULL, output_format = "md_document", ...) {
  rlang::check_installed("rmarkdown")
  path <- if (is.null(path) && rlang::is_installed("rstudioapi")) {
    rstudioapi::getActiveDocumentContext()$path
  } else {
    cli::cli_abort(("c" = "{.arg path} is required"))
  }
  rmarkdown::render(input = path, output_format = output_format)
}
