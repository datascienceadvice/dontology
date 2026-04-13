suppressWarnings(suppressPackageStartupMessages({
  library(testthat)
  library(DBI)
  library(RSQLite)
  library(rmarkdown)
}))

# Helper to set up an in-memory database
setup_test_db <- function() {
  con <- dbConnect(RSQLite::SQLite(), ":memory:")
  create_schema(con)
  return(con)
}

context("Export Functionality")

test_that("export_document handles Markdown (.md) export", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  # 1. Root setup
  doc <- DocumentInstance$new("DOC_MD", con, label = "Markdown Report")
  doc$set_prop("content", "Root Introduction")

  # 2. Child setup
  s1 <- add_child(doc, "SEC_1", "Section One")
  s1$set_prop("content", "Child Content")

  # save root and child
  s1$save()
  doc$save()

  # 3. Export
  tmp_file <- tempfile(fileext = ".md")
  on.exit(if (file.exists(tmp_file)) unlink(tmp_file), add = TRUE)

  export_document(doc, format = "md", file_path = tmp_file)

  # 4. Verify
  expect_true(file.exists(tmp_file))
  lines <- readLines(tmp_file, encoding = "UTF-8")

  expect_true(any(grepl("# Markdown Report", lines)))
  expect_true(any(grepl("Root Introduction", lines)))
  expect_true(any(grepl("## Section One", lines)))
  expect_true(any(grepl("Child Content", lines))) # Теперь будет TRUE!
})

test_that("export_document handles HTML export", {
  # Skip if Pandoc is not available
  skip_if_not(rmarkdown::pandoc_available())

  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("DOC_HTML", con, label = "HTML Report")
  doc$save()

  tmp_file <- tempfile(fileext = ".html")
  on.exit(if (file.exists(tmp_file)) unlink(tmp_file), add = TRUE)

  export_document(doc, format = "html", file_path = tmp_file)

  expect_true(file.exists(tmp_file))
  # Basic HTML check
  html_lines <- readLines(tmp_file, warn = FALSE)
  expect_true(any(grepl("<title>HTML Report</title>", html_lines)) || any(grepl("HTML Report", html_lines)))
})

test_that("export_document handles Word (.docx) export", {
  skip_if_not(rmarkdown::pandoc_available())

  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("DOC_WORD", con, label = "Word Report")
  # Add a table to test Lua filter path logic indirectly
  s1 <- add_child(doc, "SEC_1", "Summary")
  s1$set_table("table_data", data.frame(A = 1, B = 2))
  s1$save()

  doc$save()

  tmp_file <- tempfile(fileext = ".docx")
  on.exit(if (file.exists(tmp_file)) unlink(tmp_file), add = TRUE)

  # Should run without error even if styles.docx is missing (uses default Word)
  expect_error(export_document(doc, format = "word", file_path = tmp_file), NA)
  expect_true(file.exists(tmp_file))
})

test_that("export_document handles PDF export (conditional)", {
  # Skip if Pandoc or LaTeX is not available
  skip_if_not(rmarkdown::pandoc_available())

  # Check if a LaTeX engine is present (common in pharma CI)
  latex_available <- nchar(Sys.which("xelatex")) > 0 || nchar(Sys.which("pdflatex")) > 0
  if (!latex_available) skip("LaTeX engine (XeLaTeX) not found")

  con <- setup_test_db()
  on.exit(dbDisconnect(con))

  doc <- DocumentInstance$new("DOC_PDF", con, label = "PDF Report")
  doc$save()

  tmp_file <- tempfile(fileext = ".pdf")
  on.exit(if (file.exists(tmp_file)) unlink(tmp_file), add = TRUE)

  # Note: This might still fail if specific fonts (Arial) are missing on Linux CI
  # Using try() or simply expecting success
  expect_error(export_document(doc, format = "pdf", file_path = tmp_file), NA)
  expect_true(file.exists(tmp_file))
})

test_that("export_document fails gracefully on unsupported format", {
  con <- setup_test_db()
  on.exit(dbDisconnect(con))
  doc <- DocumentInstance$new("ERR_DOC", con)

  # Check for the specific error message
  expect_error(
    export_document(doc, format = "excel", file_path = "test.xlsx"),
    "Unsupported format: excel"
  )
})
