#' Freeze a reference PDF
#'
#' Copies a PDF file to a reference directory for use as a golden master.
#'
#' @param pdf_path Path to the PDF to freeze.
#' @param ref_dir Directory to store the reference PDF. Default `"reference"`.
#' @param overwrite Whether to overwrite an existing reference. Default `FALSE`.
#'
#' @return The path to the frozen reference PDF (invisibly).
#'
freeze_reference <- function(pdf_path, ref_dir = "reference", overwrite = FALSE) {
  pdf_path <- assert_pdf(pdf_path, "pdf_path")

  fs::dir_create(ref_dir)
  dest <- fs::path(ref_dir, basename(pdf_path))

  if (fs::file_exists(dest) && !overwrite) {
    cli::cli_abort(
      c(
        "Reference already exists: {.file {dest}}",
        "i" = "Use {.code overwrite = TRUE} to replace it."
      )
    )
  }

  fs::file_copy(pdf_path, dest, overwrite = overwrite)
  cli::cli_alert_success("Frozen reference: {.file {dest}}")
  invisible(as.character(dest))
}

#' Compare a new PDF against a frozen reference
#'
#' Convenience wrapper that finds the reference PDF in the reference directory
#' and calls [compare_pdfs()].
#'
#' @param new_pdf Path to the new PDF to compare.
#' @param ref_dir Directory containing the reference PDF. Default `"reference"`.
#' @param ref_name Filename of the reference PDF within `ref_dir`. If `NULL`
#'   (default), uses the same filename as `new_pdf`.
#' @param ... Additional arguments passed to [compare_pdfs()].
#'
#' @return A `comparepdf_result` object.
#'
compare_against_reference <- function(new_pdf, ref_dir = "reference",
                                      ref_name = NULL, ...) {
  new_pdf <- assert_pdf(new_pdf, "new_pdf")

  if (is.null(ref_name)) {
    ref_name <- basename(new_pdf)
  }

  ref_pdf <- fs::path(ref_dir, ref_name)

  if (!fs::file_exists(ref_pdf)) {
    cli::cli_abort(
      c(
        "Reference PDF not found: {.file {ref_pdf}}",
        "i" = "Run {.fun freeze_reference} first to create a reference."
      )
    )
  }

  compare_pdfs(as.character(ref_pdf), new_pdf, ...)
}
