test_that("HTML report only shows differing pages", {
  pdf1 <- create_test_pdf(c("Same", "A"))
  pdf2 <- create_test_pdf(c("Same", "B"))

  res <- compare_pdfs(pdf1, pdf2, fuzz = 0)
  html <- build_report_html(res)

  # report should mention the page that differs
  expect_true(grepl("Page 2", html))
  # there should be exactly one page-section (the differing page)
  sections <- gregexpr('<div class="page-section', html, fixed = TRUE)[[1]]
  expect_equal(if (sections[1] == -1) 0 else length(sections), 1)
  # the single section should be a diff section, not a match
  expect_true(grepl('class="page-section page-diff"', html))
  expect_false(grepl('class="page-section page-match"', html))
})


test_that("HTML report contains no page sections when all match", {
  pdf <- create_test_pdf(c("A", "B"))
  res <- compare_pdfs(pdf, pdf)
  html <- build_report_html(res)

  expect_true(res$all_match)
  # no page sections should be rendered in the body
  sections <- gregexpr('<div class="page-section', html, fixed = TRUE)[[1]]
  expect_equal(if (sections[1] == -1) 0 else length(sections), 0)
})

test_that("generate_diff_report requires an html output path", {
  pdf <- create_test_pdf("A")
  res <- compare_pdfs(pdf, pdf)

  expect_error(
    generate_diff_report(res, output_path = file.path(tempdir(), "report.txt"), open = FALSE),
    "must end in .*html"
  )

  expect_no_error(
    generate_diff_report(res, output_path = file.path(tempdir(), "report.HTML"), open = FALSE)
  )
})
