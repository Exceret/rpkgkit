#' @title Inquire Standalone Files from a GitHub Repository
#'
#' @description Retrieve information about all standalone R files in the R/ directory of a GitHub repository. Standalone files are identified by the "standalone-" prefix in their filename.
#'
#' @param owner A character string specifying the repository owner's username.
#' @param repo A character string specifying the repository name.
#'
#' @return A tibble with three columns:
#' \describe{
#'   \item{`owner/repo`}{Character string, the repository identifier in "owner/repo" format.}
#'   \item{description}{Character string, the description extracted from the file's YAML header or roxygen documentation.}
#'   \item{usage}{Character string, the code to import the standalone file using `usethis::use_standalone()`.}
#' }
#'
#' @details This function queries the GitHub API to list files in the R/ directory, filters for files starting with "standalone-", parses each file's YAML metadata (delimited by "# ---") and roxygen tags to extract descriptions, and generates usage code for importing each standalone file.
#'
#' @export
inquire_standalone <- function(owner, repo) {
  rlang::check_installed(c("jsonlite", "cli", "purrr"))
  api_url <- paste0(
    "https://api.github.com/repos/",
    owner,
    "/",
    repo,
    "/contents/R"
  )

  response <- tryCatch(
    jsonlite::fromJSON(readLines(api_url, warn = FALSE)),
    error = function(e) {
      cli::cli_abort(
        "Failed to fetch R/ directory from {.val {owner}/{repo}}: {e$message}"
      )
    }
  )

  standalone_files <- response[grepl("^standalone-", response$name), ]

  if (NROW(standalone_files) == 0L) {
    cli::cli_warn("No standalone files found in {.val {owner}/{repo}}")
    return(tibble::tibble(
      `owner/repo` = character(),
      description = character(),
      usage = character()
    ))
  }

  purrr::map_dfr(seq_len(NROW(standalone_files)), function(i) {
    file_info <- standalone_files[i, ]

    content <- tryCatch(
      readLines(file_info$download_url, warn = FALSE),
      error = function(e) {
        cli::cli_warn("Cannot read {.val {file_info$name}}: {e$message}")
        NULL
      }
    )
    if (is.null(content)) {
      return(NULL)
    }

    delim <- which(content == "# ---")
    if (length(delim) < 2L) {
      return(NULL)
    }

    yaml_lines <- content[(delim[1] + 1L):(delim[2] - 1L)]

    meta <- stats::setNames(
      vapply(
        yaml_lines,
        function(line) {
          parts <- strsplit(line, ":\\s*", fixed = TRUE)[[1]]
          if (length(parts) >= 2L) trimws(parts[2]) else ""
        },
        FUN.VALUE = character(1)
      ),
      vapply(
        yaml_lines,
        function(line) {
          trimws(strsplit(line, ":\\s*", fixed = TRUE)[[1]][1])
        },
        FUN.VALUE = character(1)
      )
    )

    desc_idx <- grep("@(?:title|description)", content)

    desc_text <- ""
    if (length(desc_idx) > 0L) {
      after_desc <- content[min(desc_idx) + 1L]
      desc_text <- gsub("^#'\\s*(?:@description\\s*)?", "", trimws(after_desc))
    }

    pkg_repo <- paste0(owner, "/", repo)
    usage <- sprintf(
      'usethis::use_standalone("%s", "%s")',
      pkg_repo,
      gsub("^standalone-(.*)\\.R", "\\1", file_info$name)
    )

    tibble::tibble(
      `owner/repo` = !!pkg_repo,
      description = desc_text,
      usage = usage
    )
  })
}
