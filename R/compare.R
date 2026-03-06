#' Compare two PDFs and generate a diff report
#'
#' This is the simplest way to use comparepdf. Pass two PDFs and a report
#' destination — that's it.
#'
#' @param ref_pdf Path to the reference (original) PDF.
#' @param new_pdf Path to the new (regenerated) PDF.
#' @param report_path Path for the HTML diff report.
#'   Default `"comparepdf_report.html"`.
#' @param ... Additional arguments passed to [compare_pdfs()] (e.g. `dpi`,
#'   `fuzz`).
#'
#' @return A `comparepdf_result` object (invisibly). The HTML report is
#'   written to `report_path` and opened in your browser.
#'
#' @examples
#' \dontrun{
#' compare_pdf("before.pdf", "after.pdf", "my_diff_report.html")
#' }
#'
#' @export
compare_pdf <- function(ref_pdf, new_pdf, report_path = "comparepdf_report.html", ...) {
  report_path <- assert_html_path(report_path, "report_path")
  result <- compare_pdfs(ref_pdf, new_pdf, ...)
  generate_diff_report(result, output_path = report_path)
  invisible(result)
}

#' Compare two page images
#'
#' Compares two magick images pixel-by-pixel using [magick::image_compare()].
#'
#' @param ref_image A `magick-image` object (reference page).
#' @param new_image A `magick-image` object (new page).
#' @param fuzz Fuzz tolerance percentage (0-100). Default 0 for exact
#'   pixel comparison.
#' @param metric Comparison metric passed to [magick::image_compare()].
#'   Default `"AE"` (absolute error count).
#'
#' @return A list with components:
#'   - `match`: logical, `TRUE` if distortion is 0
#'   - `distortion`: numeric, the distortion value from the comparison
#'   - `diff_image`: a `magick-image` showing pixel differences
#'
compare_pages <- function(ref_image, new_image, fuzz = 0, metric = "AE") {
  assert_magick_image(ref_image, "ref_image")
  assert_magick_image(new_image, "new_image")
  assert_positive_number(fuzz + 1, "fuzz") # allow 0, just check it's numeric and non-negative

  if (!is.numeric(fuzz) || length(fuzz) != 1 || is.na(fuzz) || fuzz < 0) {
    cli::cli_abort("{.arg fuzz} must be a non-negative number.")
  }

  diff_result <- magick::image_compare(
    ref_image,
    new_image,
    metric = metric,
    fuzz = fuzz
  )

  distortion <- as.numeric(attr(diff_result, "distortion"))

  list(
    match = identical(distortion, 0) || distortion == 0,
    distortion = distortion,
    diff_image = diff_result
  )
}

#' Compare two PDF files page-by-page
#'
#' Renders both PDFs and compares them page-by-page. Returns a structured
#' result object with per-page comparison details.
#'
#' @param ref_pdf Path to the reference PDF.
#' @param new_pdf Path to the new PDF.
#' @param dpi Resolution for rendering. Default 300.
#' @param fuzz Fuzz tolerance percentage (0-100). Default 0.
#' @param metric Comparison metric. Default `"AE"`.
#'
#' @return A `comparepdf_result` S3 object (list) with:
#'   - `pages`: list of per-page results (each with `match`, `distortion`,
#'     `diff_image`, `ref_image`, `new_image`)
#'   - `all_match`: logical, `TRUE` if all pages match
#'   - `page_count_match`: logical, `TRUE` if both PDFs have the same page count
#'   - `ref_pages`: integer, number of pages in reference PDF
#'   - `new_pages`: integer, number of pages in new PDF
#'   - `ref_pdf`: path to reference PDF
#'   - `new_pdf`: path to new PDF
#'   - `dpi`: DPI used
#'   - `fuzz`: fuzz used
#'   - `metric`: metric used
#'
compare_pdfs <- function(ref_pdf, new_pdf, dpi = 300, fuzz = 0, metric = "AE") {
  ref_pdf <- assert_pdf(ref_pdf, "ref_pdf")
  new_pdf <- assert_pdf(new_pdf, "new_pdf")
  assert_positive_number(dpi, "dpi")

  ref_info <- pdftools::pdf_info(ref_pdf)
  new_info <- pdftools::pdf_info(new_pdf)
  ref_n <- ref_info$pages
  new_n <- new_info$pages

  page_count_match <- ref_n == new_n
  if (!page_count_match) {
    cli::cli_alert_warning(
      "Page count mismatch: reference has {ref_n} pages, new has {new_n} pages."
    )
  }

  n_compare <- min(ref_n, new_n)
  pages_to_compare <- seq_len(n_compare)

  tmp_dir_ref <- tempfile("comparepdf_ref_")
  tmp_dir_new <- tempfile("comparepdf_new_")
  dir.create(tmp_dir_ref)
  dir.create(tmp_dir_new)
  on.exit(unlink(c(tmp_dir_ref, tmp_dir_new), recursive = TRUE), add = TRUE)

  page_results <- vector("list", n_compare)
  names(page_results) <- sprintf("page_%03d", pages_to_compare)

  cli::cli_progress_bar(
    "Comparing pages",
    total = n_compare,
    .envir = parent.frame()
  )

  for (i in pages_to_compare) {
    # Render one page at a time to save memory
    ref_file <- pdftools::pdf_convert(
      ref_pdf, format = "png", pages = i, dpi = dpi,
      filenames = file.path(tmp_dir_ref, "page_%03d.%s"),
      verbose = FALSE
    )
    new_file <- pdftools::pdf_convert(
      new_pdf, format = "png", pages = i, dpi = dpi,
      filenames = file.path(tmp_dir_new, "page_%03d.%s"),
      verbose = FALSE
    )

    ref_img <- magick::image_read(ref_file)
    new_img <- magick::image_read(new_file)

    comp <- compare_pages(ref_img, new_img, fuzz = fuzz, metric = metric)

    page_results[[i]] <- list(
      page = i,
      match = comp$match,
      distortion = comp$distortion,
      diff_image = comp$diff_image,
      ref_image = ref_img,
      new_image = new_img
    )

    cli::cli_progress_update(.envir = parent.frame())
  }

  cli::cli_progress_done(.envir = parent.frame())

  all_match <- page_count_match && all(vapply(page_results, `[[`, logical(1), "match"))

  result <- structure(
    list(
      pages = page_results,
      all_match = all_match,
      page_count_match = page_count_match,
      ref_pages = ref_n,
      new_pages = new_n,
      ref_pdf = ref_pdf,
      new_pdf = new_pdf,
      dpi = dpi,
      fuzz = fuzz,
      metric = metric
    ),
    class = "comparepdf_result"
  )

  if (all_match) {
    cli::cli_alert_success("All {n_compare} pages match!")
  } else {
    n_diff <- sum(!vapply(page_results, `[[`, logical(1), "match"))
    cli::cli_alert_danger("{n_diff} of {n_compare} page{?s} differ.")
  }

  result
}
