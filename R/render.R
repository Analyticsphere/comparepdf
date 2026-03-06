#' Render PDF pages to images
#'
#' Converts each page of a PDF to a magick image object using `pdftools`.
#'
#' @param pdf_path Path to a PDF file.
#' @param dpi Resolution in dots per inch for rendering. Default 300.
#' @param pages Integer vector of page numbers to render, or `NULL` for all pages.
#'
#' @return A named list of `magick-image` objects (`page_001`, `page_002`, ...).
#'
render_pdf <- function(pdf_path, dpi = 300, pages = NULL) {
  pdf_path <- assert_pdf(pdf_path, "pdf_path")
  assert_positive_number(dpi, "dpi")

  info <- pdftools::pdf_info(pdf_path)
  total_pages <- info$pages

  if (is.null(pages)) {
    pages <- seq_len(total_pages)
  } else {
    if (!is.numeric(pages) || any(is.na(pages)) || any(pages < 1) || any(pages > total_pages)) {
      cli::cli_abort(
        "{.arg pages} must be integers between 1 and {total_pages}."
      )
    }
    pages <- as.integer(pages)
  }

  tmp_dir <- tempfile("comparepdf_render_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  images <- vector("list", length(pages))
  names(images) <- sprintf("page_%03d", pages)

  cli::cli_progress_bar(
    "Rendering PDF pages",
    total = length(pages),
    .envir = parent.frame()
  )

  for (i in seq_along(pages)) {
    tmp_file <- pdftools::pdf_convert(
      pdf_path,
      format = "png",
      pages = pages[i],
      dpi = dpi,
      filenames = file.path(tmp_dir, "page_%03d.%s"),
      verbose = FALSE
    )
    images[[i]] <- magick::image_read(tmp_file)
    cli::cli_progress_update(.envir = parent.frame())
  }

  cli::cli_progress_done(.envir = parent.frame())
  images
}
