#' Internal validation helpers
#'
#' @noRd
NULL

#' Assert that a file exists
#' @param path File path to check
#' @param arg Argument name for error messages
#' @noRd
assert_file_exists <- function(path, arg = "path") {
  path <- normalize_path(path)
  if (!fs::file_exists(path)) {
    cli::cli_abort("{.arg {arg}} does not exist: {.file {path}}")
  }
  invisible(path)
}

#' Expand ~ and normalize a file path
#' @param path A file path
#' @return The expanded path
#' @noRd
normalize_path <- function(path) {
  path.expand(path)
}

#' Assert that a file is a PDF
#' @param path File path to check
#' @param arg Argument name for error messages
#' @noRd
assert_pdf <- function(path, arg = "path") {
  path <- assert_file_exists(path, arg)
  ext <- tolower(fs::path_ext(path))
  if (ext != "pdf") {
    cli::cli_abort("{.arg {arg}} must be a PDF file, got {.file .{ext}}")
  }
  invisible(path)
}

#' Assert that a file path ends in .html
#' @param path File path to check
#' @param arg Argument name for error messages
#' @noRd
assert_html_path <- function(path, arg = "path") {
  path <- normalize_path(path)
  if (!grepl("\\.html$", path, ignore.case = TRUE)) {
    cli::cli_abort("{.arg {arg}} must end in {.file .html}.")
  }
  invisible(path)
}

#' Assert that a value is a positive number
#' @param x Value to check
#' @param arg Argument name for error messages
#' @noRd
assert_positive_number <- function(x, arg = "x") {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0) {
    cli::cli_abort("{.arg {arg}} must be a single positive number.")
  }
  invisible(x)
}

#' Assert that a value is a magick image
#' @param x Value to check
#' @param arg Argument name for error messages
#' @noRd
assert_magick_image <- function(x, arg = "x") {
  if (!inherits(x, "magick-image")) {
    cli::cli_abort("{.arg {arg}} must be a {.cls magick-image} object.")
  }
  invisible(x)
}
