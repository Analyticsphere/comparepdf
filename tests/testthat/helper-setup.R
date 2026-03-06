#' Create a simple test PDF
#'
#' Generates a minimal PDF with the given text content for testing purposes.
#'
#' @param text Character vector of text to write (one element per page).
#' @param path File path for the output PDF.
#' @return The path to the created PDF (invisibly).
#' @noRd
create_test_pdf <- function(text = "Hello, world!", path = tempfile(fileext = ".pdf")) {
  grDevices::pdf(path, width = 5, height = 5)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (page_text in text) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, page_text, cex = 2)
  }
  invisible(path)
}
