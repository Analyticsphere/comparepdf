test_that("render_pdf renders all pages", {
  pdf_path <- create_test_pdf(c("Page 1", "Page 2", "Page 3"))
  images <- render_pdf(pdf_path)

  expect_length(images, 3)
  expect_named(images, c("page_001", "page_002", "page_003"))
  expect_true(all(vapply(images, inherits, logical(1), "magick-image")))
})

test_that("render_pdf renders selected pages", {
  pdf_path <- create_test_pdf(c("A", "B", "C"))
  images <- render_pdf(pdf_path, pages = c(1, 3))

  expect_length(images, 2)
  expect_named(images, c("page_001", "page_003"))
})

test_that("render_pdf validates inputs", {
  expect_error(render_pdf("nonexistent.pdf"), "does not exist")

  txt_path <- tempfile(fileext = ".txt")
  writeLines("not a pdf", txt_path)
  expect_error(render_pdf(txt_path), "must be a PDF")
})

test_that("render_pdf rejects invalid page numbers", {
  pdf_path <- create_test_pdf("One page")
  expect_error(render_pdf(pdf_path, pages = 5), "between 1 and 1")
  expect_error(render_pdf(pdf_path, pages = 0), "between 1 and 1")
})
