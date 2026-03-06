test_that("compare_pages detects identical pages", {
  pdf_path <- create_test_pdf("Same text")
  images <- render_pdf(pdf_path)

  result <- compare_pages(images[[1]], images[[1]])

  expect_true(result$match)
  expect_equal(result$distortion, 0)
  expect_true(inherits(result$diff_image, "magick-image"))
})

test_that("compare_pages detects different pages", {
  pdf1 <- create_test_pdf("Version A")
  pdf2 <- create_test_pdf("Version B")
  img1 <- render_pdf(pdf1)[[1]]
  img2 <- render_pdf(pdf2)[[1]]

  result <- compare_pages(img1, img2, fuzz = 0)

  expect_false(result$match)
  expect_gt(result$distortion, 0)
})

test_that("compare_pdfs returns correct structure for identical PDFs", {
  pdf_path <- create_test_pdf(c("Page 1", "Page 2"))

  result <- compare_pdfs(pdf_path, pdf_path)

  expect_s3_class(result, "comparepdf_result")
  expect_true(result$all_match)
  expect_true(result$page_count_match)
  expect_equal(result$ref_pages, 2)
  expect_equal(result$new_pages, 2)
  expect_length(result$pages, 2)
})

test_that("compare_pdfs detects differences", {
  pdf1 <- create_test_pdf(c("Same", "Different A"))
  pdf2 <- create_test_pdf(c("Same", "Different B"))

  result <- compare_pdfs(pdf1, pdf2, fuzz = 0)

  expect_false(result$all_match)
  expect_true(result$page_count_match)
  # Page 1 should match, page 2 should differ
  expect_true(result$pages[[1]]$match)
  expect_false(result$pages[[2]]$match)
})

test_that("compare_pdfs handles page count mismatch", {
  pdf1 <- create_test_pdf(c("A", "B", "C"))
  pdf2 <- create_test_pdf(c("A", "B"))

  result <- compare_pdfs(pdf1, pdf2)

  expect_false(result$page_count_match)
  expect_equal(result$ref_pages, 3)
  expect_equal(result$new_pages, 2)
  expect_length(result$pages, 2) # Only compares min pages
  expect_false(result$all_match) # page count mismatch means not all match
})

test_that("compare_pages validates inputs", {
  expect_error(compare_pages("not_image", "not_image"), "magick-image")
})

test_that("compare_pdf requires an html report path", {
  pdf <- create_test_pdf("Same text")

  expect_error(
    compare_pdf(pdf, pdf, report_path = file.path(tempdir(), "comparison_report.htmld")),
    "must end in .*html"
  )

  expect_no_error(
    compare_pdf(pdf, pdf, report_path = file.path(tempdir(), "comparison_report.HTML"))
  )
})
