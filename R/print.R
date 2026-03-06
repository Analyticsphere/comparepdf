#' Print a comparepdf result
#'
#' @param x A `comparepdf_result` object.
#' @param ... Ignored.
#'
#' @export
print.comparepdf_result <- function(x, ...) {
  cli::cli_h2("PDF Comparison Result")
  cli::cli_alert_info("Reference: {.file {x$ref_pdf}} ({x$ref_pages} pages)")
  cli::cli_alert_info("New:       {.file {x$new_pdf}} ({x$new_pages} pages)")
  cli::cli_alert_info("Settings:  DPI={x$dpi}, fuzz={x$fuzz}")

  if (!x$page_count_match) {
    cli::cli_alert_warning(
      "Page count mismatch! Reference: {x$ref_pages}, New: {x$new_pages}"
    )
  }

  cli::cli_text("")

  for (page in x$pages) {
    if (page$match) {
      cli::cli_alert_success("Page {page$page}: match")
    } else {
      cli::cli_alert_danger(
        "Page {page$page}: DIFFERS (distortion = {format(page$distortion, big.mark = ',')})"
      )
    }
  }

  cli::cli_text("")
  if (x$all_match) {
    cli::cli_alert_success("All pages match!")
  } else {
    n_diff <- sum(!vapply(x$pages, `[[`, logical(1), "match"))
    n_total <- length(x$pages)
    cli::cli_alert_danger("{n_diff} of {n_total} page{?s} differ.")
  }

  invisible(x)
}

#' Summarize a comparepdf result
#'
#' @param object A `comparepdf_result` object.
#' @param ... Ignored.
#'
#' @return A data.frame with columns `page`, `match`, and `distortion`.
#'
#' @export
summary.comparepdf_result <- function(object, ...) {
  df <- data.frame(
    page = vapply(object$pages, `[[`, integer(1), "page"),
    match = vapply(object$pages, `[[`, logical(1), "match"),
    distortion = vapply(object$pages, `[[`, numeric(1), "distortion")
  )
  df
}
