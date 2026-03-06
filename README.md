# comparepdf

`comparepdf` helps you identify differences between two PDF files. The package is built for a golden-reference workflow against code that generates PDF reports:

1. Start from your clean or main branch.
2. Generate a PDF before making code changes. This is your golden reference.
3. Make changes to the code that produces the PDF.
4. Generate a new PDF.
5. Compare the two PDFs with `compare_pdf()`.

Under the hood, `comparepdf` renders each PDF page to an image, performs an image diff page by page, and writes a self-contained HTML report.

## Install

Install from GitHub with `remotes`:

```r
install.packages("remotes")
remotes::install_github("Analyticsphere/comparepdf")
```

## Use

```r
library(comparepdf)

compare_pdf(
  ref_pdf = "/path/to/golden-reference-file.pdf",
  new_pdf = "/path/to/updated-file.pdf",
  report_path = "/path/to/pdf_comparison_report.html"
)
```

Replace `/path/to/` and the example file names above with the appropriate paths and file names on your local machine. 

The resulting comparision HTML report can have any name, although it must end in `.html`; `pdf_comparison_report.html` is only used as example. Note that the package will overwrite the HTML report file if you run `compare_pdf()` more than once with the same `report_path`.

### What `compare_pdf()` does

- Takes a reference PDF and a new PDF.
- Compares matching pages at the pixel level.
- Writes an HTML report to the path and file name specified in `report_path`.

### HTML report output

The HTML report is the main output users need:

- A summary showing whether the PDFs match.
- A warning if the two PDFs have different page counts.
- For each differing page, side-by-side images of the reference page, the new page, and the diff image.
- A self-contained file that can be shared or opened locally in a browser.

In interactive R sessions, the report is also opened in your browser automatically.