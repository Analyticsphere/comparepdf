test_that("freeze_reference copies PDF to reference dir", {
  pdf_path <- create_test_pdf("Freeze me")
  ref_dir <- file.path(tempdir(), "test_ref")
  on.exit(unlink(ref_dir, recursive = TRUE), add = TRUE)

  result <- freeze_reference(pdf_path, ref_dir = ref_dir)

  expect_true(file.exists(result))
  expect_equal(basename(result), basename(pdf_path))
})

test_that("freeze_reference refuses to overwrite by default", {
  pdf_path <- create_test_pdf("Original")
  ref_dir <- file.path(tempdir(), "test_ref2")
  on.exit(unlink(ref_dir, recursive = TRUE), add = TRUE)

  freeze_reference(pdf_path, ref_dir = ref_dir)
  expect_error(
    freeze_reference(pdf_path, ref_dir = ref_dir),
    "already exists"
  )
})

test_that("freeze_reference allows overwrite when requested", {
  pdf_path <- create_test_pdf("Original")
  ref_dir <- file.path(tempdir(), "test_ref3")
  on.exit(unlink(ref_dir, recursive = TRUE), add = TRUE)

  freeze_reference(pdf_path, ref_dir = ref_dir)
  expect_no_error(
    freeze_reference(pdf_path, ref_dir = ref_dir, overwrite = TRUE)
  )
})

test_that("compare_against_reference works end-to-end", {
  pdf_path <- create_test_pdf(c("Page 1", "Page 2"))
  ref_dir <- file.path(tempdir(), "test_ref4")
  on.exit(unlink(ref_dir, recursive = TRUE), add = TRUE)

  freeze_reference(pdf_path, ref_dir = ref_dir)
  result <- compare_against_reference(pdf_path, ref_dir = ref_dir)

  expect_s3_class(result, "comparepdf_result")
  expect_true(result$all_match)
})

test_that("compare_against_reference errors when no reference exists", {
  pdf_path <- create_test_pdf("No ref")
  ref_dir <- file.path(tempdir(), "test_ref_empty")
  on.exit(unlink(ref_dir, recursive = TRUE), add = TRUE)
  dir.create(ref_dir, showWarnings = FALSE)

  expect_error(
    compare_against_reference(pdf_path, ref_dir = ref_dir),
    "not found"
  )
})
