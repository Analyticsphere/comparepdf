#' Generate an HTML diff report
#'
#' Creates a self-contained HTML report showing triptych images
#' (reference | new | diff) for each page that differs.
#'
#' @param result A `comparepdf_result` object from [compare_pdfs()].
#' @param output_path Path for the HTML report file.
#'   Default `"comparepdf_report.html"`.
#' @param open Whether to open the report in a browser in interactive sessions.
#'   Default `TRUE`.
#'
#' @return The path to the HTML report (invisibly).
#'
generate_diff_report <- function(result, output_path = "comparepdf_report.html",
                                 open = TRUE) {
  if (!inherits(result, "comparepdf_result")) {
    cli::cli_abort("{.arg result} must be a {.cls comparepdf_result} object.")
  }

  output_path <- assert_html_path(output_path, "output_path")

  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    fs::dir_create(output_dir)
  }

  html <- build_report_html(result)

  writeLines(html, output_path)
  cli::cli_alert_success("Report saved to {.file {output_path}}")

  if (open && interactive()) {
    utils::browseURL(output_path)
  }

  invisible(output_path)
}

#' Build the full HTML report string
#' @param result A `comparepdf_result` object
#' @return Character string of HTML
#' @noRd
build_report_html <- function(result) {
  summary_df <- summary(result)
  n_match <- sum(summary_df$match)
  n_diff <- sum(!summary_df$match)
  n_total <- nrow(summary_df)

  # only build sections for pages that actually differ
  diff_pages <- Filter(function(p) !p$match, result$pages)

  cli::cli_progress_bar(
    "Building report ({n_diff} differing pages)",
    total = length(diff_pages)
  )

  if (length(diff_pages) > 0) {
    page_sections <- vapply(diff_pages, build_page_section, character(1))
    for (i in seq_along(diff_pages)) {
      cli::cli_progress_update()
    }
  } else {
    page_sections <- character(0)
  }
  cli::cli_progress_done()

  sprintf(
    '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>PDF Comparison Report</title>
<style>
%s
</style>
</head>
<body>
<div class="container">
<h1>PDF Comparison Report</h1>
<div class="meta">
  <p><strong>Reference:</strong> %s (%d pages)</p>
  <p><strong>New:</strong> %s (%d pages)</p>
  <p><strong>Settings:</strong> DPI=%d, fuzz=%g</p>
</div>
<div class="summary %s">
  <h2>Summary</h2>
  <p>%s</p>
  %s
</div>
%s
</div>
</body>
</html>',
    report_css(),
    htmlspecialchars(result$ref_pdf), result$ref_pages,
    htmlspecialchars(result$new_pdf), result$new_pages,
    result$dpi, result$fuzz,
    if (result$all_match) "summary-pass" else "summary-fail",
    if (result$all_match) {
      sprintf("All %d pages match!", n_total)
    } else {
      sprintf("%d of %d page(s) differ.", n_diff, n_total)
    },
    if (!result$page_count_match) {
      sprintf(
        "<p class=\"warning\">Page count mismatch: reference has %d pages, new has %d pages.</p>",
        result$ref_pages, result$new_pages
      )
    } else {
      ""
    },
    paste(page_sections, collapse = "\n")
  )
}

#' Build HTML section for a single page
#' @param page_result A single page result list
#' @return Character string of HTML
#' @noRd
build_page_section <- function(page_result) {
  status_class <- if (page_result$match) "page-match" else "page-diff"
  status_icon <- if (page_result$match) "&#10003;" else "&#10007;"
  status_text <- if (page_result$match) "Match" else "Differs"

  if (page_result$match) {
    # For matching pages, just show a summary line
    sprintf(
      '<div class="page-section %s">
  <h3><span class="status-icon">%s</span> Page %d &mdash; %s</h3>
</div>',
      status_class, status_icon, page_result$page, status_text
    )
  } else {
    # For differing pages, show the triptych
    ref_b64 <- image_to_base64(page_result$ref_image)
    new_b64 <- image_to_base64(page_result$new_image)
    diff_b64 <- image_to_base64(page_result$diff_image)

    sprintf(
      '<div class="page-section %s">
  <h3><span class="status-icon">%s</span> Page %d &mdash; %s (distortion: %s)</h3>
  <div class="triptych">
    <div class="panel">
      <h4>Reference</h4>
      <img src="data:image/png;base64,%s" alt="Reference page %d">
    </div>
    <div class="panel">
      <h4>New</h4>
      <img src="data:image/png;base64,%s" alt="New page %d">
    </div>
    <div class="panel">
      <h4>Diff</h4>
      <img src="data:image/png;base64,%s" alt="Diff page %d">
    </div>
  </div>
</div>',
      status_class, status_icon, page_result$page, status_text,
      format(page_result$distortion, big.mark = ","),
      ref_b64, page_result$page,
      new_b64, page_result$page,
      diff_b64, page_result$page
    )
  }
}

#' Convert a magick image to base64-encoded PNG
#' @param img A magick-image object
#' @return Character string of base64-encoded data
#' @noRd
image_to_base64 <- function(img) {
  raw_bytes <- magick::image_write(img, path = NULL, format = "png")
  base64enc::base64encode(raw_bytes)
}

#' Escape HTML special characters
#' @param x Character string
#' @return Escaped character string
#' @noRd
htmlspecialchars <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' CSS for the diff report
#' @return Character string of CSS
#' @noRd
report_css <- function() {
  "
* { box-sizing: border-box; margin: 0; padding: 0; }
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
  background: #f5f5f5;
  color: #333;
  line-height: 1.6;
}
.container {
  max-width: 1400px;
  margin: 0 auto;
  padding: 20px;
}
h1 {
  margin-bottom: 16px;
  font-size: 1.8em;
}
.meta {
  background: #fff;
  padding: 16px;
  border-radius: 8px;
  margin-bottom: 16px;
  border: 1px solid #ddd;
}
.meta p { margin: 4px 0; font-size: 0.9em; }
.summary {
  padding: 16px;
  border-radius: 8px;
  margin-bottom: 24px;
  border: 1px solid #ddd;
}
.summary h2 { margin-bottom: 8px; }
.summary-pass { background: #d4edda; border-color: #28a745; }
.summary-fail { background: #f8d7da; border-color: #dc3545; }
.warning { color: #856404; font-weight: bold; margin-top: 8px; }
.page-section {
  background: #fff;
  border-radius: 8px;
  padding: 16px;
  margin-bottom: 16px;
  border: 1px solid #ddd;
}
.page-match { border-left: 4px solid #28a745; }
.page-diff { border-left: 4px solid #dc3545; }
.page-match h3 { color: #28a745; }
.page-diff h3 { color: #dc3545; }
.status-icon { font-size: 1.2em; margin-right: 4px; }
h3 { margin-bottom: 12px; }
.triptych {
  display: flex;
  gap: 12px;
  overflow-x: auto;
}
.panel {
  flex: 1;
  min-width: 0;
  text-align: center;
}
.panel h4 {
  margin-bottom: 8px;
  font-size: 0.9em;
  color: #666;
}
.panel img {
  max-width: 100%;
  height: auto;
  border: 1px solid #ddd;
  border-radius: 4px;
}
"
}
